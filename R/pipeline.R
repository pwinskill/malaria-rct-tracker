# Shared orchestration used by both the weekly run and the backfill.
#
#   fetch -> normalise -> dedupe -> SCREEN (eligibility) -> EXTRACT -> store
#
# The screen decides include/exclude for every new candidate; only included
# records are extracted, stored, and put in the brief. Excluded records are
# logged (with the reason) to data/screened_out.jsonl for audit. Both included
# and excluded ids are remembered, so a re-run neither re-reports nor re-screens
# them; only "deferred" candidates (beyond the per-run screening cap) retry.

.FETCHERS <- list(
  pubmed         = function(cfg, s, e) fetch_pubmed(cfg, s, e),
  clinicaltrials = function(cfg, s, e) fetch_clinicaltrials(cfg, s, e),
  europepmc      = function(cfg, s, e) fetch_europepmc(cfg, s, e),
  ictrp          = function(cfg, s, e) fetch_ictrp(cfg, s, e)
)

#' Run the tracker pipeline for a date window
#'
#' @param cfg Config list from [load_config()].
#' @param start_date,end_date ISO dates (YYYY-MM-DD) bounding the search.
#' @param run_type "weekly" or "backfill" (recorded on each record).
#' @param max_items Optional override for the per-run extraction (LLM) cap.
#' @return The number of new records included (invisibly).
#' @export
run_pipeline <- function(cfg, start_date, end_date, run_type, max_items = NULL) {
  load_dotenv()
  today <- as.character(Sys.Date())

  # Transparency: say up front whether the API key resolved, so LLM stages never
  # silently degrade to rules without the user noticing.
  key_present <- nzchar(Sys.getenv(cfg$extraction$api_key_env %||% "ANTHROPIC_API_KEY", ""))
  message(sprintf("[key] ANTHROPIC_API_KEY %s | screening=%s extraction=%s",
                  if (key_present) "found" else "NOT found -> LLM stages fall back to rules",
                  cfg$screening$mode %||% "off", cfg$extraction$mode %||% "off"))

  raw <- list()
  for (name in names(cfg$sources)) {
    conf <- cfg$sources[[name]]
    if (!isTRUE(conf$enabled)) next
    message(sprintf("[%s] fetching %s .. %s", name, start_date, end_date))
    recs <- tryCatch(
      .FETCHERS[[name]](cfg, start_date, end_date),
      error = function(e) { message(sprintf("[%s] ERROR: %s", name, conditionMessage(e))); list() })
    message(sprintf("[%s] %d candidate(s)", name, length(recs)))
    raw <- c(raw, recs)
  }

  raw <- lapply(raw, normalize_record)
  merged <- merge_within_batch(raw)

  state <- load_state(cfg)
  new <- split_new(merged, state)
  message(sprintf("%d unique, %d new after dedupe against state", length(merged), length(new)))

  # Manual blocklist: drop anything the user has permanently excluded, before we
  # spend on screening. Matches id OR any identity key, so it holds across runs.
  excl <- load_exclusions(cfg)
  if (length(excl) && length(new)) {
    keep <- vapply(new, function(r) !any(c(r$id, r$.all_keys %||% character(0)) %in% excl), logical(1))
    if (any(!keep)) message(sprintf("%d candidate(s) dropped by manual exclusions", sum(!keep)))
    new <- new[keep]
  }

  if (!length(new)) {
    prepend_brief(cfg, list(), today)
    tryCatch(render_explorer(cfg),
             error = function(e) message(sprintf("[explorer] skipped: %s", conditionMessage(e))))
    return(invisible(0L))
  }

  new <- lapply(new, function(r) { r$first_seen <- today; r$run_type <- run_type; r })

  # --- eligibility screen ---
  new <- screen_records(new, cfg)
  is_dec <- function(r, d) identical(r$screening_decision, d)
  included <- Filter(function(r) is_dec(r, "include"), new)
  excluded <- Filter(function(r) is_dec(r, "exclude"), new)
  deferred <- Filter(function(r) is_dec(r, "deferred"), new)
  message(sprintf("%d included, %d excluded, %d deferred",
                  length(included), length(excluded), length(deferred)))

  # --- extraction on the included set only ---
  included <- enrich(included, cfg, max_items = max_items)

  # --- store ---
  append_records(cfg, included)
  append_excluded(cfg, excluded)

  # --- state: remember everything decided this run (not deferred) ---
  decided <- c(included, excluded)
  if (length(decided)) {
    keys <- unique(unlist(lapply(decided, function(r) r$.all_keys %||% r$id)))
    state$reported <- unique(c(state$reported, vapply(included, function(r) r$id, character(1))))
    state$seen_ids <- unique(c(state$seen_ids, keys))
    save_state(cfg, state)
  }

  brief <- prepend_brief(cfg, included, today)
  message(sprintf("brief updated: %s", brief))

  # Refresh the interactive HTML explorer from the full stored dataset. Never
  # let a rendering hiccup fail the data run - the data is already saved.
  tryCatch(render_explorer(cfg),
           error = function(e) message(sprintf("[explorer] skipped: %s", conditionMessage(e))))

  invisible(length(included))
}

#' Weekly run - looks back `search$weekly_lookback_days` from today.
#' @param cfg Config list; defaults to [load_config()].
#' @return Number of new records included (invisibly).
#' @export
run_weekly <- function(cfg = load_config()) {
  days <- cfg$search$weekly_lookback_days %||% 10
  end <- Sys.Date()
  start <- end - as.integer(days)
  n <- run_pipeline(cfg, as.character(start), as.character(end), run_type = "weekly")
  message(sprintf("DONE: %d new malaria RCT record(s) this week.", n))
  invisible(n)
}

#' Historical backfill
#'
#' @param days Look back this many days from `end` (used if `start` is NULL).
#' @param start,end ISO dates. `start` overrides `days`; `end` defaults to today.
#' @param mode Optional extraction-mode override ("rules"/"llm"/"off").
#' @param max_items Optional override for the per-run extraction (LLM) cap.
#' @param cfg Config list; defaults to [load_config()].
#' @return Number of new records included (invisibly).
#' @export
run_backfill <- function(days = NULL, start = NULL, end = NULL, mode = NULL,
                         max_items = NULL, cfg = load_config()) {
  if (!is.null(mode)) cfg$extraction$mode <- mode
  end <- end %||% as.character(Sys.Date())
  if (is.null(start)) {
    days <- days %||% cfg$search$backfill_days %||% 365
    start <- as.character(as.Date(end) - as.integer(days))
  }
  message(sprintf("BACKFILL %s .. %s (extraction=%s, screening=%s)",
                  start, end, cfg$extraction$mode %||% "off", cfg$screening$mode %||% "off"))
  n <- run_pipeline(cfg, start, end, run_type = "backfill", max_items = max_items)
  message(sprintf("DONE: %d record(s) added by backfill.", n))
  invisible(n)
}

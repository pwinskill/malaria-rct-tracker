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
  # Backstop: treat everything already stored (and screened-out) as seen, even if
  # state.json lagged the store because a previous run was interrupted between
  # writing a chunk and saving state. Prevents re-screening / double-charging on
  # resume, independent of state.json's freshness.
  state$seen_ids <- unique(c(state$seen_ids, .state_from_store(cfg)$seen_ids))
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
    render_brief(cfg)
    tryCatch(render_explorer(cfg),
             error = function(e) message(sprintf("[explorer] skipped: %s", conditionMessage(e))))
    return(invisible(0L))
  }

  new <- lapply(new, function(r) { r$first_seen <- today; r$run_type <- run_type; r })

  # --- screen -> extract -> STORE, in checkpointed chunks --------------------
  # The dataset is persisted after every chunk, so an interrupted or crashed run
  # keeps all completed work and a re-run resumes automatically (already-decided
  # candidates are dropped by split_new above). Per-run caps are enforced across
  # chunks, not reset per chunk.
  chunk_n     <- max(1L, as.integer(cfg$checkpoint_every %||% 50L))
  screen_cap  <- cfg$screening$max_items_per_run %||% 5000
  extract_cap <- max_items %||% cfg$extraction$max_items_per_run %||% 2000
  total <- length(new)
  screened <- 0L; extracted <- 0L
  n_inc <- 0L; n_exc <- 0L; n_def <- 0L

  pos <- 0L
  while (pos < total) {
    hi <- min(pos + chunk_n, total)
    chunk <- new[(pos + 1L):hi]
    pos <- hi

    # screen within the remaining global screening budget. Count ATTEMPTS, not
    # successes: a record that reaches the LLM but fails/defers still consumed a
    # (possibly billed) call, so the cap must bound attempts to hold.
    room <- max(0L, screen_cap - screened)
    chunk <- screen_records(chunk, cfg, max_items = room)
    screened <- screened + min(length(chunk), room)

    is_dec <- function(r, d) identical(r$screening_decision, d)
    inc <- Filter(function(r) is_dec(r, "include"),  chunk)
    exc <- Filter(function(r) is_dec(r, "exclude"),  chunk)
    def <- Filter(function(r) is_dec(r, "deferred"), chunk)

    # extract only the included set, within the remaining global extraction budget
    if (length(inc)) {
      eroom <- max(0L, extract_cap - extracted)
      inc <- enrich(inc, cfg, max_items = eroom)
      extracted <- extracted + min(length(inc), eroom)
      # Derived fields (intervention_class, countries/region) read `place`, which
      # the extractor fills - so they are computed here, after extraction and
      # before the record is stored, not back at normalize_record().
      inc <- lapply(inc, derive_fields)
    }

    state <- .checkpoint(cfg, state, inc, exc)   # append store + save state now
    n_inc <- n_inc + length(inc); n_exc <- n_exc + length(exc); n_def <- n_def + length(def)
    message(sprintf("[checkpoint] %d/%d processed | run so far: %d included, %d excluded, %d deferred",
                    pos, total, n_inc, n_exc, n_def))
  }

  message(sprintf("%d included, %d excluded, %d deferred", n_inc, n_exc, n_def))

  # Brief and explorer are both derived VIEWS rebuilt from the full stored
  # dataset, so they always reflect everything on disk - including records
  # persisted by an earlier interrupted run - not just this run's output.
  brief <- render_brief(cfg)
  message(sprintf("brief updated: %s", brief))
  tryCatch(render_explorer(cfg),
           error = function(e) message(sprintf("[explorer] skipped: %s", conditionMessage(e))))

  invisible(n_inc)
}

# Persist one processed chunk and fold its decided ids into `state` (returned).
# Order is deliberate: write the records to the store BEFORE recording their ids
# as seen, so a crash in between re-adds a duplicate row (cleaned by
# reconcile_dataset) on the next run rather than losing the record forever.
.checkpoint <- function(cfg, state, included, excluded) {
  append_records(cfg, included)
  append_excluded(cfg, excluded)
  decided <- c(included, excluded)
  if (length(decided)) {
    keys <- unique(unlist(lapply(decided, function(r) r$.all_keys %||% r$id)))
    state$reported <- unique(c(state$reported, vapply(included, function(r) r$id, character(1))))
    state$seen_ids <- unique(c(state$seen_ids, keys))
    save_state(cfg, state)
  }
  state
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

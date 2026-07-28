# Read/write the dataset: state.json, trials.csv, trials.jsonl, screened_out.jsonl,
# plus the manual-exclusions blocklist.

.state_path      <- function(cfg) file.path(cfg$output$data_dir, cfg$output$state_file)
.csv_path        <- function(cfg) file.path(cfg$output$data_dir, cfg$output$trials_csv)
.jsonl_path      <- function(cfg) file.path(cfg$output$data_dir, cfg$output$trials_jsonl)
.screened_path   <- function(cfg)
  file.path(cfg$output$data_dir, cfg$output$screened_out_file %||% "screened_out.jsonl")
.exclusions_path <- function(cfg)
  file.path(cfg$output$data_dir, cfg$output$exclusions_file %||% "exclusions.txt")

# --- dedupe memory ---------------------------------------------------------
# state.json is the durable memory that makes long runs resumable, so its own
# read and write must tolerate a crash. save_state() writes atomically and keeps
# the previous good copy as .bak; load_state() falls back to .bak and, failing
# that, rebuilds from the stored dataset - so a torn write can never brick every
# future run (or force a delete-and-re-screen that double-charges the LLM).

.norm_state <- function(st) {
  st$reported <- as.character(st$reported %||% character(0))
  if (is.null(st$seen_ids)) st$seen_ids <- st$reported   # migrate legacy files
  st$seen_ids <- as.character(st$seen_ids %||% character(0))
  st
}

# Reconstruct dedupe memory from what is actually on disk. Used as the last-ditch
# recovery in load_state and as a startup backstop in the pipeline.
.state_from_store <- function(cfg) {
  stored <- vapply(.read_store(cfg), function(r) as.character(r$id %||% ""), character(1))
  stored <- unique(stored[nzchar(stored)])
  list(reported = stored, seen_ids = unique(c(stored, .screened_ids(cfg))))
}

load_state <- function(cfg) {
  p <- .state_path(cfg); bak <- paste0(p, ".bak")
  if (!file.exists(p) && !file.exists(bak)) return(.norm_state(list()))   # genuine fresh start
  st <- if (file.exists(p)) tryCatch(fromJSON(p, simplifyVector = TRUE), error = function(e) NULL) else NULL
  if (is.null(st) && file.exists(bak))
    st <- tryCatch(fromJSON(bak, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(st)) {
    warning("state.json unreadable; rebuilding dedupe memory from the stored dataset", call. = FALSE)
    st <- .state_from_store(cfg)
  }
  .norm_state(st)
}

save_state <- function(cfg, state) {
  p <- .state_path(cfg); ensure_dir(dirname(p))
  tmp <- paste0(p, ".tmp"); bak <- paste0(p, ".bak")
  out <- list(
    reported = I(as.character(state$reported %||% character(0))),
    seen_ids = I(as.character(state$seen_ids %||% character(0)))
  )
  write_json(out, tmp, pretty = TRUE, auto_unbox = TRUE)
  # Rotate: keep the last good file as .bak, then move the freshly-written temp
  # into place. Every rename targets a non-existent path, so it works on Windows
  # too (rename-over-existing fails there). A crash between the two renames
  # leaves .bak holding a complete, loadable state.
  if (file.exists(p)) { if (file.exists(bak)) unlink(bak); file.rename(p, bak) }
  if (!file.rename(tmp, p)) { file.copy(tmp, p, overwrite = TRUE); unlink(tmp) }
  invisible(p)
}

# Read all stored records back from trials.jsonl as a list of named lists.
# Returns list() when the store does not exist yet. Malformed lines are skipped.
.read_store <- function(cfg) {
  jsonl_p <- .jsonl_path(cfg)
  if (!file.exists(jsonl_p)) return(list())
  lines <- readLines(jsonl_p, warn = FALSE, encoding = "UTF-8")
  Filter(Negate(is.null), lapply(lines[nzchar(lines)],
         function(l) tryCatch(fromJSON(l, simplifyVector = TRUE), error = function(e) NULL)))
}

# --- shared row builders ---------------------------------------------------
.records_df <- function(records) {
  rows <- lapply(records, function(r)
    as.data.frame(stats::setNames(lapply(CSV_FIELDS, function(k) as.character(r[[k]] %||% "")), CSV_FIELDS),
                  stringsAsFactors = FALSE, check.names = FALSE))
  do.call(rbind, rows)
}
.write_jsonl <- function(con, records) {
  for (r in records) {
    obj <- stats::setNames(lapply(FIELDS, function(k) as.character(r[[k]] %||% "")), FIELDS)
    writeLines(toJSON(obj, auto_unbox = TRUE), con)
  }
}

# --- append / rewrite the dataset -----------------------------------------
# Append included records to CSV + JSONL. Abstract is kept in JSONL, dropped from CSV.
append_records <- function(cfg, records) {
  if (!length(records)) return(invisible())
  ensure_dir(cfg$output$data_dir)
  # Write the authoritative JSONL FIRST, then the derived CSV. reconcile treats
  # JSONL as the source of truth, so a crash between the two must never leave a
  # record in CSV but not JSONL (reconcile would then erase it). JSONL >= CSV.
  con <- file(.jsonl_path(cfg), open = "a", encoding = "UTF-8")
  .write_jsonl(con, records); close(con)
  csv_p <- .csv_path(cfg)
  write_header <- !file.exists(csv_p)
  write.table(.records_df(records), csv_p, append = !write_header, sep = ",",
              row.names = FALSE, col.names = write_header, qmethod = "double",
              fileEncoding = "UTF-8")
  invisible(csv_p)
}

# Overwrite CSV + JSONL from scratch (used by exclude_records()).
.rewrite_store <- function(cfg, records) {
  csv_p <- .csv_path(cfg); jsonl_p <- .jsonl_path(cfg)
  con <- file(jsonl_p, open = "w", encoding = "UTF-8")
  .write_jsonl(con, records); close(con)
  if (length(records)) {
    write.table(.records_df(records), csv_p, append = FALSE, sep = ",", row.names = FALSE,
                col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")
  } else if (file.exists(csv_p)) {
    file.remove(csv_p)
  }
  invisible(csv_p)
}

# Append screened-out candidates (with reason) to an audit log.
append_excluded <- function(cfg, records) {
  if (!length(records)) return(invisible())
  ensure_dir(cfg$output$data_dir)
  keep <- c("id", "title", "source", "source_id", "url",
            "publication_date", "registry_updated", "record_type",
            "phase", "screening_reason", "screening_confidence", "first_seen", "run_type")
  con <- file(.screened_path(cfg), open = "a", encoding = "UTF-8")
  on.exit(close(con))
  for (r in records) {
    obj <- stats::setNames(lapply(keep, function(k) as.character(r[[k]] %||% "")), keep)
    writeLines(toJSON(obj, auto_unbox = TRUE), con)
  }
  invisible(.screened_path(cfg))
}

# --- manual exclusions (a durable blocklist) -------------------------------
# A record whose id OR any identity key appears here is permanently dropped -
# before screening, on every run, independent of state.json. Survives a state
# reset or a full re-backfill. Hand-editable: one id per line, "#" comments allowed.
load_exclusions <- function(cfg) {
  p <- .exclusions_path(cfg)
  if (!file.exists(p)) return(character(0))
  # Strip only a whitespace-preceded (or line-leading) "#" comment, so an id
  # that legitimately contains "#" (e.g. a DOI with a fragment) is not truncated.
  ids <- trimws(sub("(^|\\s)#.*$", "", readLines(p, warn = FALSE, encoding = "UTF-8")))
  unique(ids[nzchar(ids)])
}

#' Permanently exclude records from the dataset
#'
#' Adds the given ids to the manual blocklist (`data/exclusions.txt`) AND removes
#' any matching rows from `trials.csv` / `trials.jsonl`. Future runs will never
#' re-add them, even after a state reset. This is the correct way to delete a
#' record you've reviewed and rejected.
#'
#' @param ids One or more record ids (the `id` column, e.g. "doi:10.../..", "pmid:123").
#' @param reason Optional note, stored as a comment beside each id for provenance.
#' @param cfg Config list; defaults to [load_config()].
#' @return The number of rows removed from the dataset (invisibly).
#' @export
exclude_records <- function(ids, reason = NULL, cfg = load_config()) {
  ids <- unique(trimws(as.character(ids)))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) return(invisible(0L))

  p <- .exclusions_path(cfg)
  ensure_dir(dirname(p))
  already <- load_exclusions(cfg)
  con <- file(p, open = "a", encoding = "UTF-8")
  for (id in setdiff(ids, already)) {
    writeLines(if (!is.null(reason) && nzchar(reason)) paste0(id, "  # ", reason) else id, con)
  }
  close(con)

  removed <- .purge_store(cfg, ids)
  message(sprintf("[exclude] blocklisted %d id(s); removed %d row(s) from the dataset",
                  length(ids), removed))
  invisible(removed)
}

# Rewrite the store without any record whose id is in `ids`. Returns count removed.
.purge_store <- function(cfg, ids) {
  if (!file.exists(.jsonl_path(cfg))) return(0L)
  recs <- .read_store(cfg)
  keep <- Filter(function(r) !((r$id %||% "") %in% ids), recs)
  removed <- length(recs) - length(keep)
  if (removed > 0L) .rewrite_store(cfg, keep)
  removed
}

# Read the canonical ids recorded in the screened-out audit log.
.screened_ids <- function(cfg) {
  p <- .screened_path(cfg)
  if (!file.exists(p)) return(character(0))
  ids <- vapply(readLines(p, warn = FALSE, encoding = "UTF-8"), function(l) {
    if (!nzchar(l)) return("")
    d <- tryCatch(fromJSON(l, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(d)) "" else d$id %||% ""
  }, character(1))
  unique(ids[nzchar(ids)])
}

#' Reconcile the dataset after a merge
#'
#' De-duplicates `trials.csv` / `trials.jsonl` (rows sharing an `id` are collapsed
#' to one) and rebuilds `state.json` so its dedupe memory matches what is actually
#' stored plus the screened-out log. Run this after resolving a git merge that
#' touched `data/` (e.g. a local backfill colliding with the weekly job), or any
#' time you suspect duplicates. `seen_ids` is unioned, never shrunk, so no memory
#' is lost.
#'
#' @param cfg Config list; defaults to [load_config()].
#' @return The number of records kept (invisibly).
#' @export
reconcile_dataset <- function(cfg = load_config()) {
  recs <- .read_store(cfg)
  n_before <- length(recs)

  ids <- vapply(recs, function(r) r$id %||% "", character(1))
  recs <- recs[nzchar(ids)]; ids <- ids[nzchar(ids)]
  # Collapse rows sharing an id by MERGING them (priority-ordered gap-fill), not
  # by keeping the first - so a richer duplicate (e.g. the LLM-extracted copy)
  # isn't discarded in favour of a barer one.
  keep <- lapply(unique(ids), function(the_id) {
    grp <- recs[ids == the_id]
    prio <- vapply(grp, function(r) .SOURCE_PRIORITY[[r$source %||% ""]] %||% 9L, integer(1))
    grp <- grp[order(prio)]
    base <- grp[[1]]
    for (j in seq_along(grp)[-1]) base <- merge_records(base, grp[[j]])
    base
  })
  dups <- n_before - length(keep)
  .rewrite_store(cfg, keep)

  kept_ids <- vapply(keep, function(r) r$id %||% "", character(1))
  st <- load_state(cfg)
  st$reported <- unique(kept_ids)
  st$seen_ids <- unique(c(st$seen_ids, kept_ids, .screened_ids(cfg)))
  save_state(cfg, st)

  message(sprintf("[reconcile] %d record(s) kept, %d duplicate row(s) removed; state rebuilt",
                  length(keep), dups))
  invisible(length(keep))
}

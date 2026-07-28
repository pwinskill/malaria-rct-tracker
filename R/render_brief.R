# Prepend a dated, human-readable section to the markdown brief.

.BRIEF_MARKER <- "<!-- New weekly entries are inserted directly below this line -->"

.brief_header <- function() {
  paste0(
    "# Malaria RCT Brief\n\n",
    "Automated weekly scan for new malaria randomized controlled trials.\n",
    "Sources: PubMed, ClinicalTrials.gov, Europe PMC (incl. medRxiv preprints and\n",
    "Cochrane review metadata). Most recent week on top. The structured dataset lives\n",
    "in data/trials.csv.\n\n",
    "---\n\n",
    .BRIEF_MARKER, "\n"
  )
}

.brief_line <- function(rec, label, key) {
  v <- rec[[key]] %||% ""
  if (has_text(v)) sprintf("- **%s:** %s\n", label, v) else ""
}

.brief_entry <- function(rec) {
  impact <- if (has_text(rec$impact_summary)) rec$impact_summary else "(no result stated yet)"
  effect <- paste(Filter(nzchar, c(
    rec$effect_metric %||% "",
    rec$effect_estimate %||% "",
    if (has_text(rec$effect_ci)) sprintf("(95%% CI %s)", rec$effect_ci) else ""
  )), collapse = " ")

  md <- sprintf("### %s\n", if (has_text(rec$title)) rec$title else "(untitled)")
  src_id <- trimws(paste0(rec$source %||% "",
                          if (has_text(rec$source_id)) sprintf(" (%s)", rec$source_id) else ""))
  if (nzchar(src_id)) md <- paste0(md, sprintf("- **Source / ID:** %s\n", src_id))
  md <- paste0(md, .brief_line(rec, "Journal", "journal"))
  md <- paste0(md, .brief_line(rec, "Type", "record_type"))
  md <- paste0(md, .brief_line(rec, "Phase", "phase"))
  md <- paste0(md, .brief_line(rec, "Place", "place"))
  md <- paste0(md, .brief_line(rec, "Design", "design"))
  md <- paste0(md, .brief_line(rec, "Status", "status"))
  md <- paste0(md, .brief_line(rec, "Published", "publication_date"))
  tp <- paste(Filter(nzchar, c(rec$trial_start %||% "", rec$trial_completion %||% "")), collapse = " to ")
  if (nzchar(tp)) md <- paste0(md, sprintf("- **Trial period:** %s\n", tp))
  md <- paste0(md, .brief_line(rec, "Intervention(s)", "interventions_raw"))
  md <- paste0(md, .brief_line(rec, "Class", "intervention_class"))
  md <- paste0(md, .brief_line(rec, "Population", "population"))
  md <- paste0(md, .brief_line(rec, "N", "n_total"))
  md <- paste0(md, sprintf("- **Impact:** %s\n", impact))
  if (nzchar(effect)) md <- paste0(md, sprintf("- **Effect:** %s\n", effect))
  md <- paste0(md, .brief_line(rec, "Species", "species"))
  md <- paste0(md, .brief_line(rec, "Follow-up", "follow_up"))
  md <- paste0(md, sprintf("- **Link:** %s\n", rec$url %||% ""))
  md
}

#' (Re)build the markdown brief from the stored dataset
#'
#' The brief is a derived VIEW: it is rebuilt in full from the dataset every run,
#' grouped into dated sections by each record's `first_seen` (newest on top). This
#' makes it self-healing - records persisted by an earlier interrupted run always
#' appear, exactly like the HTML explorer - rather than being tied to a single
#' run's in-memory output.
#'
#' @param cfg Config list; defaults to [load_config()].
#' @param records Optional list of records; defaults to reading `trials.jsonl`.
#' @return The path written (invisibly).
#' @export
render_brief <- function(cfg = load_config(), records = NULL) {
  if (is.null(records)) records <- .read_store(cfg)
  path <- file.path(cfg$output$outputs_dir, cfg$output$brief_filename)
  ensure_dir(cfg$output$outputs_dir)

  body <- .brief_header()
  if (length(records)) {
    dates <- vapply(records, function(r) {
      d <- as.character(r$first_seen %||% ""); if (nzchar(d)) d else "undated"
    }, character(1))
    # newest first_seen on top; "undated" (no date) sorts last
    uniq_dates <- unique(dates)
    uniq_dates <- uniq_dates[order(uniq_dates == "undated", -xtfrm(uniq_dates))]
    sections <- unlist(lapply(uniq_dates, function(d) {
      grp <- records[dates == d]
      c(sprintf("## Week of %s\n", d), vapply(grp, .brief_entry, character(1)))
    }))
    body <- sub(.BRIEF_MARKER, paste0(.BRIEF_MARKER, "\n\n", paste(sections, collapse = "\n")),
                body, fixed = TRUE)
  }
  writeLines(body, path)
  invisible(path)
}

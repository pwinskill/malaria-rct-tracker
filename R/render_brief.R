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

# Insert this run's records as a new dated section directly below the marker
# (newest on top). Empty runs are a no-op once the brief exists, so quiet weeks
# don't accumulate "no new trials" filler.
prepend_brief <- function(cfg, records, week_label) {
  path <- file.path(cfg$output$outputs_dir, cfg$output$brief_filename)
  ensure_dir(cfg$output$outputs_dir)

  body <- if (file.exists(path)) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    .brief_header()
  }

  if (!length(records)) {
    if (!file.exists(path)) writeLines(body, path)
    return(invisible(path))
  }

  section <- c(sprintf("## Week of %s\n", week_label),
               vapply(records, .brief_entry, character(1)))
  block <- paste(section, collapse = "\n")

  if (grepl(.BRIEF_MARKER, body, fixed = TRUE)) {
    body <- sub(.BRIEF_MARKER,
                paste0(.BRIEF_MARKER, "\n\n", block),
                body, fixed = TRUE)
  } else {
    body <- paste0(body, "\n\n", block)
  }

  writeLines(body, path)
  invisible(path)
}

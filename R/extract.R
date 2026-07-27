# Structured extraction - fill Tier-1 gaps and Tier-2 fields.
#
# Runs only on records the screen INCLUDED. Modes (config$extraction$mode):
#   llm   - Anthropic API with structured outputs, best quality
#   rules - regex heuristics only, no key, $0
#   off   - do nothing
# In llm mode every record still gets the rules pass as a floor, so a per-run
# cap or a single failed call never leaves a record blank. `extracted_by`
# records how each record was populated - at RECORD granularity: a record
# stamped "llm:<model>" may still have had a few gaps filled by the rules floor.

# Fields the LLM may populate (never overwrites a non-empty value).
.LLM_FIELDS <- c(
  "place", "design", "comparator", "population", "n_total", "primary_outcome",
  "impact_summary", "effect_metric", "effect_estimate", "effect_ci", "p_value",
  "follow_up", "species", "transmission_setting", "age_range", "safety_summary", "funder",
  "trial_start", "trial_completion"
)

.EXTRACT_SCHEMA <- local({
  props <- stats::setNames(lapply(.LLM_FIELDS, function(f) list(type = "string")), .LLM_FIELDS)
  list(type = "object", properties = props, required = .LLM_FIELDS, additionalProperties = FALSE)
})

.EXTRACT_SYSTEM <- paste0(
  "You extract structured data from a malaria randomized controlled trial for a research ",
  "registry. Use ONLY the provided title, abstract and metadata. Return every field as a ",
  "short string; use \"\" when a field is genuinely not determinable. Never invent values - ",
  "but DO use information present anywhere in the text, INCLUDING THE TITLE.\n\n",
  "Field definitions:\n",
  "- place: country/countries or specific sites/region where the trial was run. Extract from ",
  "the title or abstract (a country named in the title counts). Multiple -> comma-separated.\n",
  "- design: design descriptors (e.g. 'double-blind, placebo-controlled, cluster-randomized, phase 3').\n",
  "- comparator: what the intervention was compared against (placebo / active comparator / standard care).\n",
  "- population: who was enrolled (e.g. 'children under 5', 'pregnant women', 'adults').\n",
  "- n_total: total participants or clusters enrolled; digits only.\n",
  "- primary_outcome: the primary endpoint.\n",
  "- impact_summary: one plain sentence stating the main result.\n",
  "- effect_metric / effect_estimate / effect_ci: the primary effect ",
  "(e.g. 'protective efficacy' / '67%' / '55-76').\n",
  "- p_value; follow_up (e.g. '12 months'); species ('P. falciparum' | 'P. vivax' | 'mixed'); ",
  "transmission_setting (endemicity/seasonality); age_range; safety_summary; funder.\n",
  "- trial_start / trial_completion: when the trial started and when it (or its primary outcome ",
  "measurement) ended, if stated (e.g. 'conducted between 2019 and 2021' -> start '2019', ",
  "completion '2021'); use \"\" if not stated."
)

enrich <- function(records, cfg, max_items = NULL) {
  mode <- cfg$extraction$mode %||% "off"
  if (!length(records)) return(records)
  if (identical(mode, "off"))
    return(lapply(records, function(r) { r$extracted_by <- "off"; r }))
  if (identical(mode, "rules"))
    return(lapply(records, function(r) { r <- apply_rules(r); r$extracted_by <- "rules"; r }))

  cap <- max_items %||% cfg$extraction$max_items_per_run %||% 2000
  key <- llm_key(cfg$extraction)
  model <- cfg$extraction$model %||% "claude-sonnet-5"
  if (!nzchar(key)) {
    message("[extract] no API key found; using rule-based extraction")
    return(lapply(records, function(r) { r <- apply_rules(r); r$extracted_by <- "rules"; r }))
  }

  used <- 0L
  for (i in seq_along(records)) {
    # record_context() is never empty (it always has "Title:"/"Source:"), so
    # guard on actual content - otherwise a content-free record burns a paid call.
    if (i <= cap && (has_text(records[[i]]$abstract) || has_text(records[[i]]$title))) {
      records[[i]] <- tryCatch(
        .extract_llm(records[[i]], key, model),
        error = function(e) {
          message(sprintf("[extract] LLM failed for %s: %s; using rules",
                          records[[i]]$id, conditionMessage(e)))
          r <- apply_rules(records[[i]]); r$extracted_by <- "rules"; r
        })
      # count only genuine LLM successes; a parse failure falls back to rules
      # inside .extract_llm and is stamped accordingly.
      if (startsWith(records[[i]]$extracted_by %||% "", "llm:")) used <- used + 1L
    } else {
      records[[i]] <- apply_rules(records[[i]])
      records[[i]]$extracted_by <- "rules"
    }
    records[[i]] <- apply_rules(records[[i]])  # floor: fill any remaining gaps
  }
  if (length(records) > cap)
    message(sprintf("[extract] LLM-extracted %d of %d (cap=%d); remainder used rules",
                    used, length(records), cap))
  records
}

# Apply a parsed extraction result to a record. Never overwrites a non-empty
# field. Split out from the network call so it can be unit-tested.
.extract_apply <- function(rec, data, model) {
  if (is.null(data)) return(rec)
  for (f in .LLM_FIELDS) {
    val <- llm_str(data[[f]])
    if (nzchar(val) && !has_text(rec[[f]])) rec[[f]] <- val
  }
  rec$extracted_by <- paste0("llm:", model)
  rec
}

.extract_llm <- function(rec, key, model) {
  # thinking disabled: this is a read-and-fill task, and claude-sonnet-5 runs
  # adaptive thinking by default, which would share (and can exhaust) max_tokens
  # and truncate the 17-field JSON.
  data <- llm_json(key, model, .EXTRACT_SYSTEM, record_context(rec), .EXTRACT_SCHEMA,
                   max_tokens = 1500L, thinking = list(type = "disabled"))
  # Unparseable/truncated response -> fall back to rules and stamp it as such,
  # mirroring the screen path, so the record isn't left with a blank/false
  # provenance and the caller's "used" tally stays honest.
  if (is.null(data)) { r <- apply_rules(rec); r$extracted_by <- "rules"; return(r) }
  .extract_apply(rec, data, model)
}

# --------------------------------------------------------------- rule fallback
.RE_EFF <- "(efficacy|effectiveness)\\D{0,15}(\\d{1,3}(?:\\.\\d+)?)\\s?%"
.RE_CI  <- "95%\\s?CI[^0-9]*([0-9.]+)\\D+([0-9.]+)"
.RE_P   <- "\\bp\\s?[<=]\\s?(0?\\.\\d+)"
.RE_N   <- "\\bn\\s?=\\s?([0-9,]{2,7})"

# Malaria-endemic and commonly-studied countries, for pulling `place` from a
# title/abstract when no LLM is available.
.PLACE_COUNTRIES <- c(
  "Burkina Faso", "Cote d'Ivoire", "Côte d'Ivoire", "Ivory Coast", "Sierra Leone",
  "South Africa", "South Sudan", "Papua New Guinea", "Equatorial Guinea", "Democratic Republic of the Congo",
  "DR Congo", "DRC", "Mali", "Kenya", "Uganda", "Tanzania", "Ghana", "Nigeria", "Malawi",
  "Mozambique", "Zambia", "Zimbabwe", "Senegal", "Gambia", "Guinea", "Benin", "Togo", "Niger",
  "Chad", "Cameroon", "Gabon", "Congo", "Angola", "Ethiopia", "Rwanda", "Burundi", "Sudan",
  "Somalia", "Madagascar", "Liberia", "Guinea-Bissau", "Mauritania", "Eritrea", "Botswana",
  "Namibia", "Eswatini", "Swaziland", "India", "Indonesia", "Cambodia", "Thailand", "Myanmar",
  "Laos", "Vietnam", "Bangladesh", "Pakistan", "Afghanistan", "Philippines", "Papua",
  "Solomon Islands", "Vanuatu", "Colombia", "Brazil", "Peru", "Venezuela", "Haiti", "Ecuador"
)

.place_from_text <- function(txt) {
  if (!nzchar(txt %||% "")) return("")
  hits <- character(0)
  for (co in .PLACE_COUNTRIES) {
    if (grepl(paste0("\\b", co, "\\b"), txt, ignore.case = TRUE, perl = TRUE))
      hits <- c(hits, co)
  }
  hits <- unique(hits)
  # Drop any hit that is a whole-word substring of a longer hit, so
  # "Papua New Guinea" does not also yield "Guinea" and "Papua".
  keep <- vapply(hits, function(h) {
    others <- hits[hits != h]
    !any(grepl(paste0("\\b", h, "\\b"), others, ignore.case = TRUE))
  }, logical(1))
  paste(hits[keep], collapse = ", ")
}

.match1 <- function(text, pattern) {
  m <- regmatches(text, regexec(pattern, text, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 0) character(0) else m
}

apply_rules <- function(rec) {
  txt <- record_context(rec)
  if (!nzchar(txt)) return(rec)

  if (!has_text(rec$place)) {
    p <- .place_from_text(paste(rec$title %||% "", rec$abstract %||% ""))
    if (nzchar(p)) rec$place <- p
  }
  if (!has_text(rec$impact_summary)) {
    src <- if (has_text(rec$abstract)) rec$abstract else rec$title %||% ""
    for (s in unlist(strsplit(src, "(?<=[.!?])\\s+", perl = TRUE))) {
      if (grepl("efficac|reduc|incidence|prevalence|no differ|significan|hazard|risk ratio",
                s, ignore.case = TRUE)) { rec$impact_summary <- substr(trimws(s), 1, 300); break }
    }
  }
  m <- .match1(txt, .RE_EFF)
  if (length(m) >= 3 && !has_text(rec$effect_estimate)) {
    if (!has_text(rec$effect_metric)) rec$effect_metric <- "efficacy (%)"
    rec$effect_estimate <- paste0(m[3], "%")
  }
  m <- .match1(txt, .RE_CI)
  if (length(m) >= 3 && !has_text(rec$effect_ci)) rec$effect_ci <- paste0(m[2], "-", m[3])
  m <- .match1(txt, .RE_P)
  if (length(m) >= 2 && !has_text(rec$p_value)) rec$p_value <- m[2]
  m <- .match1(txt, .RE_N)
  if (length(m) >= 2 && !has_text(rec$n_total)) rec$n_total <- gsub(",", "", m[2])
  rec
}

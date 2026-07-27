# The canonical trial record schema shared across the pipeline.
#
# One named list per record. Field order below is the CSV column order.
#
#   IDENTITY   - source metadata + provenance of the record itself
#   SCREENING  - eligibility decision from the LLM screen (systematic-review style)
#   TIER1/2    - structured fields, filled from metadata + the extraction step
#   META       - bookkeeping (abstract kept for screening/extraction, dropped from CSV)
#
# Records also carry dot-prefixed working fields (.doi, .ncts, .pmids, .all_keys)
# used only for identity/dedupe; these are never written to disk.

# --- identity / provenance -------------------------------------------------
IDENTITY <- c(
  "id",                # canonical dedupe key (doi / nct / pmid); see normalize.R
  "title",
  "source",            # pubmed | clinicaltrials | europepmc | ictrp
  "source_id",         # PMID / NCT / PMCID etc.
  "journal",
  "url",
  "publication_date",  # PubMed ArticleDate/PubDate; Europe PMC firstPublicationDate
  "registry_updated",  # ClinicalTrials.gov lastUpdatePostDate (registry edit, NOT a pub date)
  "trial_start",       # when the trial started (CT.gov startDate, or extracted from abstract)
  "trial_completion",  # primary completion (CT.gov primaryCompletionDate, or extracted)
  "record_type"        # trial | preprint | registration | protocol | review | other
)

# --- eligibility screen ----------------------------------------------------
SCREENING <- c(
  "screening_decision",    # include | exclude | deferred | ""(unscreened)
  "screening_reason",      # one-sentence rationale (audit trail)
  "screening_confidence",  # low | medium | high
  "phase"                  # e.g. "Phase 3", "Not applicable", "" if unstated
)

# --- Tier 1: core, from source metadata + abstract -------------------------
TIER1 <- c(
  "place",              # country / site(s) / region
  "design",             # e.g. cluster-randomized, double-blind, phase 3
  "status",             # ongoing | completed | results posted | published | preprint
  "interventions_raw",  # free-text intervention description
  "intervention_class", # normalised vocab (see normalize.R)
  "comparator",
  "population",         # e.g. children <5, pregnant women, all ages
  "n_total",            # total enrolled (participants or clusters)
  "primary_outcome",
  "impact_summary"      # human-readable 1-2 sentence headline result
)

# --- Tier 2: extracted when present ---------------------------------------
TIER2 <- c(
  "effect_metric", "effect_estimate", "effect_ci", "p_value", "follow_up",
  "species", "transmission_setting", "age_range", "safety_summary", "funder"
)

# --- meta / bookkeeping ----------------------------------------------------
META <- c(
  "extracted_by",  # llm:<model> | rules | off
  "conditions",    # ClinicalTrials.gov condition list (kept separate from abstract)
  "abstract",      # source abstract; used for screening/extraction, dropped from CSV
  "first_seen",    # ISO date this record first entered the dataset
  "run_type"       # weekly | backfill
)

FIELDS <- c(IDENTITY, SCREENING, TIER1, TIER2, META)

# Columns written to the human-facing CSV (abstract omitted for readability).
CSV_FIELDS <- setdiff(FIELDS, "abstract")

# Return a record (named list) with every field present (empty string default).
new_record <- function(...) {
  rec <- as.list(stats::setNames(rep("", length(FIELDS)), FIELDS))
  args <- list(...)
  for (k in names(args)) {
    if (k %in% FIELDS) {
      v <- args[[k]]
      rec[[k]] <- if (is.null(v) || length(v) == 0) "" else as.character(v)[1]
    }
  }
  rec$.doi <- ""
  rec$.ncts <- character(0)
  rec$.pmids <- character(0)
  rec
}

# The text an LLM (screen or extractor) should reason over: everything we know.
record_context <- function(rec) {
  parts <- c(
    paste0("Title: ", rec$title %||% ""),
    if (has_text(rec$abstract))          paste0("Abstract: ", rec$abstract),
    if (has_text(rec$conditions))         paste0("Conditions: ", rec$conditions),
    if (has_text(rec$interventions_raw))  paste0("Interventions: ", rec$interventions_raw),
    if (has_text(rec$design))             paste0("Design: ", rec$design),
    if (has_text(rec$phase))              paste0("Registered phase: ", rec$phase),
    if (has_text(rec$primary_outcome))    paste0("Primary outcome: ", rec$primary_outcome),
    if (has_text(rec$status))             paste0("Status: ", rec$status),
    paste0("Source: ", rec$source %||% "")
  )
  paste(parts, collapse = "\n")
}

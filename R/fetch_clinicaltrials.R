# ClinicalTrials.gov API v2.
#
# Registered interventional, randomized malaria trials whose record was last
# updated inside the window (catches new registrations AND newly posted
# results). We parse protocolSection, and pull any linked publication PMIDs so
# a trial can later merge with its PubMed/Europe PMC record.

.CT_BASE <- "https://clinicaltrials.gov/api/v2/studies"

# Safe nested getter over parsed JSON (nested lists).
.g <- function(x, ..., .default = "") {
  for (k in c(...)) {
    if (!is.list(x) || is.null(x[[k]])) return(.default)
    x <- x[[k]]
  }
  x
}

fetch_clinicaltrials <- function(cfg, start_date, end_date) {
  advanced <- sprintf(
    paste0("AREA[StudyType]INTERVENTIONAL AND AREA[DesignAllocation]RANDOMIZED AND ",
           "AREA[LastUpdatePostDate]RANGE[%s,%s]"),
    start_date, end_date
  )
  out <- list()
  token <- NULL
  pages <- 0L
  repeat {
    query <- list(query.cond = "malaria", filter.advanced = advanced,
                  pageSize = 100, sort = "LastUpdatePostDate:desc")
    if (!is.null(token)) query$pageToken <- token
    data <- http_get_json(.CT_BASE, query)
    for (study in data$studies %||% list()) {
      rec <- .parse_ct_study(study)
      if (!is.null(rec)) out[[length(out) + 1L]] <- rec
    }
    token <- data$nextPageToken %||% NULL
    pages <- pages + 1L
    if (is.null(token)) break
    if (pages >= 500L) {  # ~50k studies; warn rather than silently truncate
      message("[clinicaltrials] page cap (500) reached; results may be truncated - narrow the window")
      break
    }
  }
  out
}

# Normalise ClinicalTrials.gov phase codes ("PHASE3", "NA", "EARLY_PHASE1", ...)
# into a readable label. "NA" -> "Not applicable" (typical for vector-control /
# cluster trials, which we deliberately keep).
.ct_phase <- function(phases) {
  p <- toupper(unlist(phases %||% character(0)))
  p <- p[nzchar(p)]
  if (!length(p)) return("")
  if (any(p == "EARLY_PHASE1")) return("Early Phase 1")
  nums <- gsub("[^0-9]", "", p)
  nums <- nums[nzchar(nums)]
  if (length(nums)) return(paste0("Phase ", paste(unique(nums), collapse = "/")))
  if (any(p == "NA")) "Not applicable" else ""
}

.parse_ct_study <- function(study) {
  ps <- study$protocolSection %||% list()
  nct <- .g(ps, "identificationModule", "nctId")
  if (!nzchar(nct)) return(NULL)

  title <- .g(ps, "identificationModule", "briefTitle")
  status <- .g(ps, "statusModule", "overallStatus")
  reg_updated <- .g(ps, "statusModule", "lastUpdatePostDateStruct", "date")
  tstart <- .g(ps, "statusModule", "startDateStruct", "date")
  tcompl <- .g(ps, "statusModule", "primaryCompletionDateStruct", "date")
  has_results <- nzchar(.g(ps, "statusModule", "resultsFirstPostDateStruct", "date"))

  allocation <- .g(ps, "designModule", "designInfo", "allocation")
  model <- .g(ps, "designModule", "designInfo", "interventionModel")
  phases <- unlist(.g(ps, "designModule", "phases", .default = list()))
  design_str <- paste(Filter(nzchar, c(allocation, model, phases)), collapse = ", ")

  interventions <- .g(ps, "armsInterventionsModule", "interventions", .default = list())
  iv_names <- paste(Filter(nzchar, vapply(interventions,
                    function(i) i$name %||% "", character(1))), collapse = "; ")

  n <- .g(ps, "designModule", "enrollmentInfo", "count")
  locations <- .g(ps, "contactsLocationsModule", "locations", .default = list())
  countries <- unique(Filter(nzchar, vapply(locations,
                      function(l) l$country %||% "", character(1))))
  place <- paste(sort(countries), collapse = ", ")

  sponsor <- .g(ps, "sponsorCollaboratorsModule", "leadSponsor", "name")
  age_min <- .g(ps, "eligibilityModule", "minimumAge")
  age_max <- .g(ps, "eligibilityModule", "maximumAge")
  age <- paste(Filter(nzchar, c(age_min, age_max)), collapse = " to ")
  conditions <- paste(unlist(.g(ps, "conditionsModule", "conditions", .default = list())),
                      collapse = ", ")

  primaries <- .g(ps, "outcomesModule", "primaryOutcomes", .default = list())
  outcome <- if (length(primaries)) primaries[[1]]$measure %||% "" else ""

  # Only the trial's OWN publications count as identity: RESULT (a paper
  # reporting this trial) or DERIVED (auto-linked by PubMed). BACKGROUND
  # citations are unrelated papers the trialists merely cite - harvesting their
  # PMIDs would wrongly merge those papers into this trial during dedupe.
  refs <- .g(ps, "referencesModule", "references", .default = list())
  pmids <- Filter(nzchar, vapply(refs, function(r) {
    ty <- toupper(as.character(r$type %||% ""))
    if (ty %in% c("RESULT", "DERIVED")) as.character(r$pmid %||% "") else ""
  }, character(1)))

  status_label <- if (has_results) "results posted" else if (nzchar(status)) tolower(status) else ""

  # conditions go in their own field (NOT abstract) so that when this record
  # merges with a publication, the publication's real abstract wins.
  rec <- new_record(
    title = title, source = "clinicaltrials", source_id = nct,
    url = sprintf("https://clinicaltrials.gov/study/%s", nct),
    registry_updated = reg_updated, trial_start = tstart, trial_completion = tcompl,
    place = place, design = design_str, status = status_label,
    phase = .ct_phase(phases), record_type = "registration",
    interventions_raw = iv_names,
    # count arrives as a JSON number; format() avoids as.character() rendering
    # round magnitudes in scientific notation (100000 -> "1e+05").
    n_total = if (has_text(n)) format(n, scientific = FALSE, trim = TRUE) else "",
    primary_outcome = outcome, age_range = age, funder = sponsor,
    conditions = conditions
  )
  rec$.pmids <- as.character(pmids)
  rec
}

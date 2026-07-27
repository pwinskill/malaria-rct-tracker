# Europe PMC REST search.
#
# One robust endpoint that also folds in medRxiv preprints and Cochrane review
# metadata. resultType=core returns abstracts. We page with cursorMark. Where
# Europe PMC tags an item as a randomized controlled trial we record that in
# `design`, which lets you filter out the reviews/protocols the broad query
# also returns.

.EPMC_BASE <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"

fetch_europepmc <- function(cfg, start_date, end_date) {
  # Malaria scoped to title/abstract/keyword/MeSH (not incidental full-text).
  # RCT signal = MEDLINE-curated RCT publication type OR a preprint (SRC:PPR)
  # whose title carries a randomi* token. Europe PMC's unique value is preprints,
  # which have no pub-type tag; the wildcard MUST be unquoted (a quoted "randomi*"
  # is matched literally and returns nothing). PubMed carries recall for published
  # papers; the eligibility screen supplies precision.
  query <- sprintf(paste0(
    '(TITLE:"malaria" OR ABSTRACT:"malaria" OR KW:"malaria" OR MESH:"malaria" OR ',
    'TITLE:"plasmodium" OR ABSTRACT:"plasmodium") AND ',
    '(PUB_TYPE:"Randomized Controlled Trial" OR (SRC:"PPR" AND TITLE:randomi*)) AND ',
    '(FIRST_PDATE:[%s TO %s])'),
    start_date, end_date)
  params <- list(query = query, format = "json", resultType = "core",
                 pageSize = 100, cursorMark = "*")
  out <- list()
  seen_cursor <- character(0)
  pages <- 0L
  repeat {
    data <- http_get_json(.EPMC_BASE, params)
    for (res in data$resultList$result %||% list()) {
      out[[length(out) + 1L]] <- .parse_epmc(res)
    }
    cursor <- data$nextCursorMark %||% NULL
    pages <- pages + 1L
    if (is.null(cursor) || cursor %in% seen_cursor) break
    if (pages >= 500L) {  # ~50k records; warn rather than silently truncate
      message("[europepmc] page cap (500) reached; results may be truncated - narrow the window")
      break
    }
    seen_cursor <- c(seen_cursor, cursor)
    params$cursorMark <- cursor
  }
  out
}

.parse_epmc <- function(res) {
  pmid <- as.character(res$pmid %||% "")
  doi <- as.character(res$doi %||% "")
  src <- as.character(res$source %||% "")
  ext_id <- as.character(res$id %||% "")

  journal <- res$journalInfo$journal$title %||%
    res$bookOrReportDetails$publisher %||% ""

  ptypes <- tolower(unlist(res$pubTypeList$pubType %||% list()))
  is_rct <- any(grepl("randomi[sz]ed controlled trial", ptypes))
  is_preprint <- identical(src, "PPR")
  is_cochrane <- grepl("cochrane database syst rev", tolower(journal))

  url <- if (nzchar(doi)) {
    sprintf("https://doi.org/%s", doi)
  } else if (nzchar(pmid)) {
    sprintf("https://europepmc.org/article/MED/%s", pmid)
  } else {
    sprintf("https://europepmc.org/article/%s/%s", src, ext_id)
  }

  status <- if (is_preprint) "preprint" else if (is_cochrane) "cochrane review" else "published"
  design <- if (is_rct) "randomized controlled trial" else ""

  record_type <- if (is_preprint) "preprint" else if (is_cochrane) "review" else "trial"
  rec <- new_record(
    title = res$title %||% "", source = "europepmc",
    source_id = if (nzchar(pmid)) pmid else ext_id, url = url,
    publication_date = as.character(res$firstPublicationDate %||% ""),
    design = design, status = status, record_type = record_type,
    abstract = res$abstractText %||% "", journal = journal
  )
  rec$.doi <- doi
  rec
}

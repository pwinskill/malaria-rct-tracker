# PubMed via NCBI E-utilities.
#
# esearch (with usehistory=y) records the full result set on NCBI's history
# server; we then page through it with efetch in batches. This removes the old
# 500-record ceiling, so wide backfills (e.g. 10 years) are retrieved in full.
#
# The query deliberately combines the curated RCT publication type with a
# free-text 'randomi[sz]ed' clause, because the [pt] tag lags a few days/weeks
# behind brand-new articles.

.PUBMED_ESEARCH <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
.PUBMED_EFETCH  <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

# Sensitive, malaria-scoped RCT search. Malaria is required as a MeSH term or
# in title/abstract (not incidental full-text). The RCT clause is an adaptation
# of the Cochrane sensitivity-maximising filter - precision is supplied later by
# the eligibility screen, so the search favours recall.
.PUBMED_TERM <- paste0(
  "(malaria[MeSH Terms] OR malaria[tiab] OR plasmodium[tiab] OR antimalarial*[tiab]) AND ",
  "(randomized controlled trial[pt] OR controlled clinical trial[pt] OR randomized[tiab] OR ",
  "randomised[tiab] OR placebo[tiab] OR randomly[tiab] OR trial[ti])"
)

.MONTHS <- stats::setNames(
  sprintf("%02d", 1:12),
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

.pubmed_common <- function(cfg) {
  p <- list(db = "pubmed",
            tool = cfg$tool_name %||% "malaria-rct-tracker",
            email = cfg$contact_email %||% "")
  env <- cfg$sources$pubmed$api_key_env %||% ""
  k <- if (nzchar(env)) Sys.getenv(env, "") else ""
  if (nzchar(k)) p$api_key <- k
  p
}

fetch_pubmed <- function(cfg, start_date, end_date) {
  common <- .pubmed_common(cfg)
  es <- c(common, list(
    term = .PUBMED_TERM, retmode = "json", retmax = 0, sort = "date",
    datetype = "pdat", mindate = start_date, maxdate = end_date, usehistory = "y"
  ))
  js <- http_get_json(.PUBMED_ESEARCH, es)
  res <- js$esearchresult %||% list()
  count <- suppressWarnings(as.integer(res$count %||% "0"))
  if (is.na(count) || count == 0) return(list())
  webenv <- res$webenv %||% ""
  query_key <- res$querykey %||% ""

  out <- list()
  batch <- 200L
  starts <- seq(0L, count - 1L, by = batch)
  for (k in seq_along(starts)) {
    fp <- c(common, list(
      WebEnv = webenv, query_key = query_key,
      retstart = starts[k], retmax = batch,
      retmode = "xml", rettype = "abstract"
    ))
    xml <- http_get_text(.PUBMED_EFETCH, fp)
    out <- c(out, parse_pubmed_xml(xml))
    if (length(starts) > 1) Sys.sleep(0.2)  # polite pacing across pages
  }
  out
}

.xt <- function(node, xpath) {
  n <- xml_find_first(node, xpath)
  v <- xml_text(n)
  if (length(v) == 0 || is.na(v)) "" else trimws(v)
}

parse_pubmed_date <- function(y, m, d) {
  if (!nzchar(y %||% "")) return("")
  # normalise abbreviation case ("MAR"/"march" -> "Mar") before the lookup, so a
  # non-titlecase month isn't missed and silently defaulted to January.
  key <- substr(m %||% "", 1, 3)
  key <- paste0(toupper(substr(key, 1, 1)), tolower(substr(key, 2, 3)))
  mm <- .MONTHS[key]
  mm <- if (is.na(mm)) {
    if (grepl("^[0-9]+$", m %||% "")) sprintf("%02d", as.integer(m)) else "01"
  } else {
    unname(mm)
  }
  dd <- if (grepl("^[0-9]+$", d %||% "")) sprintf("%02d", as.integer(d)) else "01"
  sprintf("%s-%s-%s", y, mm, dd)
}

parse_pubmed_xml <- function(xml) {
  doc <- tryCatch(read_xml(xml), error = function(e) NULL)
  if (is.null(doc)) return(list())
  arts <- xml_find_all(doc, ".//PubmedArticle")
  lapply(arts, .parse_pubmed_article)
}

.parse_pubmed_article <- function(art) {
  pmid <- .xt(art, ".//MedlineCitation/PMID")
  title <- .xt(art, ".//ArticleTitle")

  abstract_nodes <- xml_find_all(art, ".//Abstract/AbstractText")
  abstract <- paste(trimws(xml_text(abstract_nodes)), collapse = " ")
  abstract <- trimws(abstract)

  journal <- .xt(art, ".//Journal/Title")

  y <- .xt(art, ".//ArticleDate/Year")
  if (!nzchar(y)) y <- .xt(art, ".//Journal/JournalIssue/PubDate/Year")
  m <- .xt(art, ".//ArticleDate/Month")
  if (!nzchar(m)) m <- .xt(art, ".//Journal/JournalIssue/PubDate/Month")
  d <- .xt(art, ".//ArticleDate/Day")
  date <- parse_pubmed_date(y, m, d)

  doi <- .xt(art, ".//ArticleIdList/ArticleId[@IdType='doi']")

  ptypes <- tolower(xml_text(xml_find_all(art, ".//PublicationType")))
  design <- if (any(grepl("randomized", ptypes))) "randomized controlled trial" else ""

  # ClinicalTrials.gov accession(s) recorded on the article -> NCT bridge.
  ncts <- xml_text(xml_find_all(
    art, ".//DataBankList/DataBank[DataBankName='ClinicalTrials.gov']/AccessionNumberList/AccessionNumber"
  ))
  ncts <- trimws(ncts[nzchar(trimws(ncts))])

  is_protocol <- grepl("study protocol|protocol for (a|an|the)", tolower(title))
  rec <- new_record(
    title = title, source = "pubmed", source_id = pmid,
    url = sprintf("https://pubmed.ncbi.nlm.nih.gov/%s/", pmid),
    publication_date = date, design = design, status = "published",
    record_type = if (is_protocol) "protocol" else "trial",
    abstract = abstract, journal = journal
  )
  rec$.doi <- doi
  rec$.ncts <- ncts
  rec
}

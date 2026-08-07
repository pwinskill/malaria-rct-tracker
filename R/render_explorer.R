# Generate a self-contained, interactive HTML explorer for the dataset.
#
# Produces a single file (docs/index.html by default) with the whole dataset
# embedded inline as JSON - no server, no external assets, no fetch. Open it
# directly in a browser, or serve the folder with GitHub Pages. Abstracts are
# dropped (CSV-level fields only) to keep the file small. Regenerated on every
# pipeline run, and runnable by hand via render_explorer().

# Locate the HTML template. system.file() is checked first because it is
# correct both for an installed package AND under pkgload (which shims it to
# resolve inst/); the bare "inst/" path is only a fallback for being run from a
# source checkout with no package loaded.
.explorer_template <- function() {
  hit <- system.file("explorer_template.html", package = "malariarct")
  if (!nzchar(hit) || !file.exists(hit)) hit <- file.path("inst", "explorer_template.html")
  if (!file.exists(hit))
    stop("explorer_template.html not found (looked in the installed package and inst/).",
         call. = FALSE)
  hit
}

# Substitute every __TOKEN__ in one pass over the ORIGINAL template.
#
# Two properties matter and neither is free:
#   * single pass - an injected value is never rescanned, so a trial whose text
#     happens to contain the literal "__META__" cannot swallow the real token.
#   * fail loud - a token declared here but absent from the template (or a
#     __LIKE_THIS__ token in the template that nothing fills) is an error, not a
#     silently half-rendered page.
# Token names must be regex-safe; "__[A-Z0-9_]+__" is the enforced shape.
.assemble <- function(tmpl, tokens) {
  toks <- names(tokens)
  stopifnot(all(grepl("^__[A-Z0-9_]+__$", toks)))

  present <- vapply(toks, function(t) grepl(t, tmpl, fixed = TRUE), logical(1))
  if (!all(present))
    stop("explorer template is missing token(s): ", paste(toks[!present], collapse = ", "),
         call. = FALSE)

  in_tmpl <- unique(regmatches(tmpl, gregexpr("__[A-Z0-9_]+__", tmpl))[[1]])
  unknown <- setdiff(in_tmpl, toks)
  if (length(unknown))
    stop("explorer template has unfilled token(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)

  m <- gregexpr(paste(toks, collapse = "|"), tmpl)
  regmatches(tmpl, m) <- list(unlist(tokens[regmatches(tmpl, m)[[1]]], use.names = FALSE))
  tmpl
}

# Make a JSON payload safe to embed inside a <script> element.
#
# jsonlite escapes "</" but leaves "<!--", "<script" and the JS line terminators
# U+2028/U+2029 raw. A bare "<!--" inside script data flips the HTML tokenizer
# into script-data-escaped state; a following "<script" escalates it to
# double-escaped, and from there the template's real </script> no longer closes
# the element - the rest of the document is swallowed and the page renders blank
# with no console error. Escaping EVERY "<" is lossless: in JSON a "<" can only
# occur inside a string literal, where < is an exactly equivalent escape.
.js_safe <- function(x) {
  x <- gsub("<",      "\\u003c", x, fixed = TRUE)
  x <- gsub("\u2028", "\\u2028", x, fixed = TRUE)   # LINE SEPARATOR
  x <- gsub("\u2029", "\\u2029", x, fixed = TRUE)   # PARAGRAPH SEPARATOR
  x
}

# Optional burden denominators for the geography view (data/burden.csv).
# Absent or malformed -> an empty table, and the explorer simply omits the
# "per million cases" toggle rather than inventing numbers. A country missing
# from the file is EXCLUDED from the normalised view, never treated as zero.
.read_burden <- function(cfg) {
  p <- file.path(cfg$output$data_dir %||% "data", cfg$output$burden_file %||% "burden.csv")
  empty <- list(cases = stats::setNames(list(), character(0)), source = "", n = 0L)
  if (!file.exists(p)) return(empty)
  df <- tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, comment.char = "#"),
                 error = function(e) NULL)
  if (is.null(df) || !all(c("country", "cases") %in% names(df))) return(empty)
  cases <- suppressWarnings(as.numeric(df$cases))
  keep <- nzchar(trimws(as.character(df$country))) & !is.na(cases) & cases > 0
  if (!any(keep)) return(empty)
  list(cases  = as.list(stats::setNames(cases[keep], trimws(as.character(df$country))[keep])),
       source = if ("source" %in% names(df)) as.character(df$source[keep][1]) else "",
       n      = sum(keep))
}

#' Build the interactive HTML explorer
#'
#' Writes a single self-contained `index.html` (default `docs/index.html`) that
#' loads the current dataset with summary cards, a trials-over-time chart, an
#' intervention-class chart, filters and a sortable table. Everything is inline;
#' open the file directly in a browser. Publishing it (e.g. via GitHub Pages) is
#' a separate, deliberate step - this function only writes a local file.
#'
#' @param cfg Config list; defaults to [load_config()].
#' @param records Optional list of records to render; defaults to reading the
#'   stored dataset (`trials.jsonl`).
#' @param generated Date stamp shown in the page header. Defaults to today;
#'   pass a fixed value to make the output reproducible (used by the tests).
#' @return The path written (invisibly).
#' @export
render_explorer <- function(cfg = load_config(), records = NULL, generated = Sys.Date()) {
  if (is.null(records)) records <- .read_store(cfg)

  # Embed CSV-level fields only (abstract omitted for size); all values as strings.
  rows <- lapply(records, function(r)
    stats::setNames(lapply(CSV_FIELDS, function(k) as.character(r[[k]] %||% "")), CSV_FIELDS))
  data_json <- if (length(rows)) as.character(toJSON(rows, auto_unbox = TRUE)) else "[]"
  meta_json <- as.character(toJSON(
    list(generated = as.character(generated), count = length(rows)), auto_unbox = TRUE))

  burden_json <- as.character(toJSON(.read_burden(cfg), auto_unbox = TRUE))

  tmpl <- paste(readLines(.explorer_template(), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- .assemble(tmpl, list("__DATA__"   = .js_safe(data_json),
                               "__META__"   = .js_safe(meta_json),
                               "__BURDEN__" = .js_safe(burden_json)))

  dir   <- cfg$output$explorer_dir %||% "docs"
  fname <- cfg$output$explorer_filename %||% "index.html"
  ensure_dir(dir)
  out_path <- file.path(dir, fname)
  # Binary mode so the file is byte-identical on every platform: in text mode
  # Windows would emit CRLF and the ubuntu runner LF, and git would then store a
  # whole new ~230 KB blob on every alternation instead of a small delta.
  con <- file(out_path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(html), con, useBytes = TRUE)

  message(sprintf("[explorer] wrote %s (%d trial(s))", out_path, length(rows)))
  invisible(out_path)
}

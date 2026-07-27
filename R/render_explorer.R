# Generate a self-contained, interactive HTML explorer for the dataset.
#
# Produces a single file (docs/index.html by default) with the whole dataset
# embedded inline as JSON - no server, no external assets, no fetch. Open it
# directly in a browser, or serve the folder with GitHub Pages. Abstracts are
# dropped (CSV-level fields only) to keep the file small. Regenerated on every
# pipeline run, and runnable by hand via render_explorer().

# Locate the HTML template, whether loaded via pkgload (inst/) or installed.
.explorer_template <- function() {
  candidates <- c(
    file.path("inst", "explorer_template.html"),
    system.file("explorer_template.html", package = "malariarct")
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (!length(hit))
    stop("explorer_template.html not found (looked in inst/ and the installed package).",
         call. = FALSE)
  hit[1]
}

# Replace a literal token with a literal value, WITHOUT the backreference
# processing that sub()/gsub() apply to their replacement string - JSON data
# routinely contains backslashes (\", \uXXXX) that sub() would corrupt.
.inject <- function(s, token, value) {
  parts <- strsplit(s, token, fixed = TRUE)[[1]]
  if (length(parts) < 2L) return(s)
  paste0(parts[1], value, paste(parts[-1], collapse = token))
}

# Neutralise any "</" so a stray "</script>" inside the data can't close the
# embedding <script> tag. "<\/" is an equivalent, valid escape inside a JS
# string literal, so the parsed data is unchanged.
.script_safe <- function(x) gsub("</", "<\\/", x, fixed = TRUE)

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
#' @return The path written (invisibly).
#' @export
render_explorer <- function(cfg = load_config(), records = NULL) {
  if (is.null(records)) records <- .read_store(cfg)

  # Embed CSV-level fields only (abstract omitted for size); all values as strings.
  rows <- lapply(records, function(r)
    stats::setNames(lapply(CSV_FIELDS, function(k) as.character(r[[k]] %||% "")), CSV_FIELDS))
  data_json <- if (length(rows)) as.character(toJSON(rows, auto_unbox = TRUE)) else "[]"
  meta_json <- as.character(toJSON(
    list(generated = as.character(Sys.Date()), count = length(rows)), auto_unbox = TRUE))

  tmpl <- paste(readLines(.explorer_template(), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- .inject(tmpl, "__DATA__", .script_safe(data_json))
  html <- .inject(html, "__META__", .script_safe(meta_json))

  dir   <- cfg$output$explorer_dir %||% "docs"
  fname <- cfg$output$explorer_filename %||% "index.html"
  ensure_dir(dir)
  out_path <- file.path(dir, fname)
  con <- file(out_path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(enc2utf8(html), con, useBytes = TRUE)

  message(sprintf("[explorer] wrote %s (%d trial(s))", out_path, length(rows)))
  invisible(out_path)
}

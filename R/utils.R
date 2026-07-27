# Small shared helpers.

# NULL/empty-coalescing operator. Unlike base R's `%||%` (R >= 4.4, NULL only)
# this also treats length-0 vectors (e.g. character(0)) as empty. Internal;
# not exported, so it does not shadow base for package users.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# TRUE if x is a non-empty, non-NA scalar string.
has_text <- function(x) {
  x <- x %||% ""
  length(x) >= 1 && !is.na(x[1]) && nzchar(as.character(x[1]))
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

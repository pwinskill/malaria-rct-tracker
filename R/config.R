# Configuration and .env loading.

#' Load the tracker configuration
#'
#' Reads `config.yaml`. By default it looks in the current working directory
#' (the project root), so run the tracker from the repo root — the same place
#' `data/` and `outputs/` live.
#'
#' @param path Optional explicit path to a YAML config file.
#' @return A named list of settings.
#' @export
load_config <- function(path = NULL) {
  if (is.null(path)) {
    candidates <- c(
      "config.yaml",
      file.path(getwd(), "config.yaml"),
      system.file("config.yaml", package = "malariarct")
    )
    candidates <- candidates[nzchar(candidates)]
    hit <- candidates[file.exists(candidates)]
    if (!length(hit)) {
      stop("config.yaml not found. Run from the project root, or pass `path=`.",
           call. = FALSE)
    }
    path <- hit[1]
  }
  read_yaml(path)
}

# Minimal .env loader so local runs pick up API keys without extra deps.
# The project .env TAKES PRECEDENCE over any ambient environment variable, so a
# stray global ANTHROPIC_API_KEY (e.g. in ~/.Renviron or a shell export) cannot
# silently win over the project's own key. In CI there is no .env, so the
# secret-injected env var is used unchanged.
load_dotenv <- function(path = ".env") {
  if (!file.exists(path)) return(invisible(FALSE))
  for (line in readLines(path, warn = FALSE)) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#") || !grepl("=", line, fixed = TRUE)) next
    kv <- strsplit(line, "=", fixed = TRUE)[[1]]
    key <- trimws(kv[1])
    key <- sub("^export[[:space:]]+", "", key)          # tolerate `export KEY=val`
    val <- trimws(paste(kv[-1], collapse = "="))
    # strip a single pair of surrounding quotes: KEY="val" / KEY='val'
    if (grepl('^".*"$', val) || grepl("^'.*'$", val)) val <- substr(val, 2, nchar(val) - 1)
    if (nzchar(key)) {
      args <- list(val); names(args) <- key
      do.call(Sys.setenv, args)
    }
  }
  invisible(TRUE)
}

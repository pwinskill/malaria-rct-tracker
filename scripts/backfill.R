#!/usr/bin/env Rscript
# Historical backfill. Reads inputs from BF_* environment variables (used by the
# GitHub Actions workflow) or from command-line flags, whichever is set.
#
# Examples (run from the project root):
#   Rscript scripts/backfill.R --days 3650                 # last ~10 years
#   Rscript scripts/backfill.R --start 2015-01-01 --end 2020-12-31
#   Rscript scripts/backfill.R --days 730 --mode llm --max-items 500
suppressMessages(library(malariarct))

args <- commandArgs(trailingOnly = TRUE)
getflag <- function(flag) {
  i <- which(args == flag)
  if (length(i) && i[1] < length(args)) args[i[1] + 1L] else ""
}
pick <- function(env_name, flag) {
  v <- Sys.getenv(env_name, "")
  if (nzchar(v)) v else getflag(flag)
}

days  <- pick("BF_DAYS",  "--days")
start <- pick("BF_START", "--start")
end   <- pick("BF_END",   "--end")
mode  <- pick("BF_MODE",  "--mode")
maxit <- pick("BF_MAX",   "--max-items")

run_backfill(
  days      = if (nzchar(days))  as.integer(days) else NULL,
  start     = if (nzchar(start)) start else NULL,
  end       = if (nzchar(end))   end   else NULL,
  mode      = if (nzchar(mode))  mode  else NULL,
  max_items = if (nzchar(maxit)) as.integer(maxit) else NULL
)

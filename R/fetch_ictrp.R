# WHO ICTRP -- optional, OFF by default.
#
# The public ICTRP web service is unreliable (crawling service intermittently
# unavailable; bulk access is via a request form). ClinicalTrials.gov + Europe
# PMC already cover the great majority of what we need, so this is a best-effort
# placeholder kept for a future integration. Returns an empty list and logs a note.

fetch_ictrp <- function(cfg, start_date, end_date) {
  message("[ictrp] placeholder source; returns no records ",
          "(enable + implement only if ClinicalTrials.gov coverage proves insufficient)")
  list()
}

"""WHO ICTRP — optional, OFF by default.

The public ICTRP web service is unreliable (crawling service intermittently
unavailable; bulk access is via a request form). ClinicalTrials.gov + Europe PMC
already cover the great majority of what we need, so this is a best-effort stub
kept as a placeholder for a future integration. Returns [] and logs a note.
"""


def fetch(cfg, start_date, end_date):
    print("[ictrp] source is a placeholder and returns no records "
          "(enable + implement only if ClinicalTrials.gov coverage proves insufficient)")
    return []

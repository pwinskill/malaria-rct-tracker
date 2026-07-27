"""One-time historical catch-up. Defaults to config.search.backfill_days (365).

Usage:
  python -m src.backfill              # last 365 days (config default)
  python -m src.backfill --days 730   # last 2 years
  python -m src.backfill --start 2015-01-01 --end 2025-12-31

Seeds state.json so the weekly run won't re-report anything found here.
"""
import argparse
import datetime as dt

from .pipeline import load_config, run


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--days", type=int, default=None)
    ap.add_argument("--start", type=str, default=None)
    ap.add_argument("--end", type=str, default=None)
    args = ap.parse_args()

    cfg = load_config()
    end = args.end or dt.date.today().isoformat()
    if args.start:
        start = args.start
    else:
        days = args.days or cfg["search"].get("backfill_days", 365)
        start = (dt.date.fromisoformat(end) - dt.timedelta(days=days)).isoformat()

    print(f"BACKFILL {start} .. {end}")
    n = run(cfg, start, end, run_type="backfill")
    print(f"DONE: {n} record(s) added by backfill.")


if __name__ == "__main__":
    main()

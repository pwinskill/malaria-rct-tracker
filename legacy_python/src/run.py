"""Weekly run. Looks back `weekly_lookback_days` from today.

Usage:  python -m src.run
"""
import datetime as dt

from .pipeline import load_config, run


def main():
    cfg = load_config()
    days = cfg["search"].get("weekly_lookback_days", 10)
    end = dt.date.today()
    start = end - dt.timedelta(days=days)
    n = run(cfg, start.isoformat(), end.isoformat(), run_type="weekly")
    print(f"DONE: {n} new malaria RCT record(s) this week.")


if __name__ == "__main__":
    main()

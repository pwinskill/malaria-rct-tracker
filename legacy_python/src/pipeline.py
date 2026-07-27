"""Shared orchestration used by both the weekly run and the backfill."""
import datetime as dt
import os

import yaml

from . import (dedupe, extract, fetch_clinicaltrials, fetch_europepmc,
               fetch_ictrp, fetch_pubmed, normalize, render_brief, store)

_FETCHERS = {
    "pubmed": fetch_pubmed.fetch,
    "clinicaltrials": fetch_clinicaltrials.fetch,
    "europepmc": fetch_europepmc.fetch,
    "ictrp": fetch_ictrp.fetch,
}


def load_config(path=None):
    path = path or os.path.join(os.path.dirname(os.path.dirname(__file__)), "config.yaml")
    with open(path, encoding="utf-8") as f:
        return yaml.safe_load(f)


def _load_dotenv():
    """Minimal .env loader so local runs pick up keys without extra deps."""
    root = os.path.dirname(os.path.dirname(__file__))
    p = os.path.join(root, ".env")
    if not os.path.exists(p):
        return
    with open(p, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                k, v = line.split("=", 1)
                os.environ.setdefault(k.strip(), v.strip())


def run(cfg, start_date, end_date, run_type):
    """Fetch -> normalise -> dedupe -> extract -> store -> render. Returns count."""
    _load_dotenv()
    today = dt.date.today().isoformat()

    raw = []
    for name, conf in cfg["sources"].items():
        if not conf.get("enabled"):
            continue
        print(f"[{name}] fetching {start_date} .. {end_date}")
        try:
            recs = _FETCHERS[name](cfg, start_date, end_date)
            print(f"[{name}] {len(recs)} candidate(s)")
            raw.extend(recs)
        except Exception as exc:  # noqa: BLE001 - one bad source shouldn't kill the run
            print(f"[{name}] ERROR: {exc}")

    for r in raw:
        normalize.normalize(r)
    merged = dedupe.merge_within_batch(raw)

    state = store.load_state(cfg)
    new = dedupe.split_new(merged, state)
    print(f"{len(merged)} unique, {len(new)} new after dedupe against state")

    for r in new:
        r["first_seen"] = today
        r["run_type"] = run_type
    extract.enrich(new, cfg)

    store.append_records(cfg, new)
    state.setdefault("reported", []).extend(r["id"] for r in new)
    store.save_state(cfg, state)

    brief_path = render_brief.prepend(cfg, new, today)
    print(f"brief updated: {brief_path}")
    return len(new)

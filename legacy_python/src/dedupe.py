"""Merge duplicate hits across sources and drop already-reported items."""

# When the same trial appears from multiple sources, prefer richer fields in
# this source order (later sources fill gaps only).
_SOURCE_PRIORITY = {"clinicaltrials": 0, "pubmed": 1, "europepmc": 2, "ictrp": 3}


def _merge(primary, other):
    for k, v in other.items():
        if not primary.get(k) and v:
            primary[k] = v
    return primary


def merge_within_batch(records):
    """Collapse records that share a canonical id."""
    by_id = {}
    order = sorted(records, key=lambda r: _SOURCE_PRIORITY.get(r.get("source"), 9))
    for rec in order:
        rid = rec["id"]
        if rid in by_id:
            by_id[rid] = _merge(by_id[rid], rec)
        else:
            by_id[rid] = dict(rec)
    return list(by_id.values())


def split_new(records, state):
    """Return only records whose id has not been reported before."""
    seen = set(state.get("reported", []))
    return [r for r in records if r["id"] not in seen]

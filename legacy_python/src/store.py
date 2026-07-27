"""Read/write the dataset: state.json, trials.csv, trials.jsonl."""
import csv
import json
import os

from .record import CSV_FIELDS, FIELDS


def _paths(cfg):
    out = cfg["output"]
    data = out["data_dir"]
    return {
        "state": os.path.join(data, out["state_file"]),
        "csv": os.path.join(data, out["trials_csv"]),
        "jsonl": os.path.join(data, out["trials_jsonl"]),
    }


def load_state(cfg):
    p = _paths(cfg)["state"]
    if os.path.exists(p):
        with open(p, encoding="utf-8") as f:
            return json.load(f)
    return {"reported": []}


def save_state(cfg, state):
    p = _paths(cfg)["state"]
    os.makedirs(os.path.dirname(p), exist_ok=True)
    with open(p, "w", encoding="utf-8") as f:
        json.dump(state, f, indent=2, ensure_ascii=False)


def append_records(cfg, records):
    """Append new records to CSV + JSONL, creating headers as needed."""
    if not records:
        return
    p = _paths(cfg)
    os.makedirs(cfg["output"]["data_dir"], exist_ok=True)

    csv_new = not os.path.exists(p["csv"])
    with open(p["csv"], "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CSV_FIELDS, extrasaction="ignore")
        if csv_new:
            w.writeheader()
        for r in records:
            w.writerow({k: r.get(k, "") for k in CSV_FIELDS})

    with open(p["jsonl"], "a", encoding="utf-8") as f:
        for r in records:
            f.write(json.dumps({k: r.get(k, "") for k in FIELDS}, ensure_ascii=False) + "\n")

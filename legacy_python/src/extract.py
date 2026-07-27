"""Fill Tier-1 gaps and Tier-2 fields from the abstract.

Three modes (config.extraction.mode):
  llm   - Anthropic API, best quality (needs ANTHROPIC_API_KEY)
  rules - regex heuristics only, no key, $0
  off   - do nothing, Tier-1 metadata as-is
"""
import json
import os
import re

# Fields the extractor is allowed to populate (never overwrites non-empty ones).
_LLM_FIELDS = [
    "place", "design", "comparator", "population", "n_total", "primary_outcome",
    "impact_summary", "effect_metric", "effect_estimate", "effect_ci", "p_value",
    "follow_up", "species", "transmission_setting", "age_range", "safety_summary",
    "funder",
]

_PROMPT = """You extract structured data from the abstract of a malaria \
randomized controlled trial. Return ONLY a JSON object with these keys \
(use "" when not stated, never guess):
place, design, comparator, population, n_total, primary_outcome, impact_summary, \
effect_metric, effect_estimate, effect_ci, p_value, follow_up, species, \
transmission_setting, age_range, safety_summary, funder.

Guidance: impact_summary = one plain sentence on the main result. \
effect_metric/estimate/ci = the primary effect (e.g. protective efficacy 75%, \
incidence rate ratio 0.58, CI "0.45-0.74"). n_total = total participants/clusters. \
species = "P. falciparum", "P. vivax", or "mixed". Keep values short.

Title: {title}
Abstract: {abstract}
"""


def enrich(records, cfg):
    mode = cfg.get("extraction", {}).get("mode", "off")
    if mode == "off" or not records:
        return records
    if mode == "rules":
        for r in records:
            _rules(r)
        return records
    # mode == "llm"
    cap = cfg["extraction"].get("max_items_per_run", 200)
    client, model = _client(cfg)
    if client is None:  # no key -> degrade gracefully to rules
        print("[extract] no API key found; falling back to rule-based extraction")
        for r in records:
            _rules(r)
        return records
    for r in records[:cap]:
        if not (r.get("abstract") or "").strip():
            _rules(r)
            continue
        try:
            _llm_one(r, client, model)
        except Exception as exc:  # noqa: BLE001
            print(f"[extract] LLM failed for {r.get('id')}: {exc}; using rules")
            _rules(r)
    return records


def _client(cfg):
    key = os.environ.get(cfg["extraction"].get("api_key_env", "ANTHROPIC_API_KEY"), "")
    if not key:
        return None, None
    try:
        import anthropic
    except ImportError:
        print("[extract] anthropic package not installed; using rules")
        return None, None
    return anthropic.Anthropic(api_key=key), cfg["extraction"].get("model")


def _llm_one(rec, client, model):
    prompt = _PROMPT.format(title=rec.get("title", ""),
                            abstract=(rec.get("abstract") or "")[:6000])
    msg = client.messages.create(
        model=model, max_tokens=700,
        messages=[{"role": "user", "content": prompt}],
    )
    text = "".join(b.text for b in msg.content if getattr(b, "type", "") == "text")
    m = re.search(r"\{.*\}", text, re.DOTALL)
    if not m:
        return
    data = json.loads(m.group(0))
    for f in _LLM_FIELDS:
        val = str(data.get(f, "") or "").strip()
        if val and not rec.get(f):
            rec[f] = val


# --------------------------------------------------------------- rule fallback
_EFF = re.compile(r"(efficacy|effectiveness)\D{0,15}(\d{1,3}(?:\.\d+)?)\s?%", re.I)
_CI = re.compile(r"95%\s?CI[^0-9]*([0-9.]+)\D+([0-9.]+)", re.I)
_P = re.compile(r"\bp\s?[<=]\s?(0?\.\d+)", re.I)
_N = re.compile(r"\bn\s?=\s?([\d,]{2,7})", re.I)


def _rules(rec):
    txt = rec.get("abstract") or rec.get("title") or ""
    if not rec.get("impact_summary") and txt:
        # first sentence mentioning a result-ish word
        for sent in re.split(r"(?<=[.!?])\s+", txt):
            if re.search(r"efficac|reduc|incidence|prevalence|no differ|"
                         r"significan|hazard|risk ratio", sent, re.I):
                rec["impact_summary"] = sent.strip()[:300]
                break
    m = _EFF.search(txt)
    if m and not rec.get("effect_estimate"):
        rec["effect_metric"] = rec.get("effect_metric") or "efficacy (%)"
        rec["effect_estimate"] = m.group(2) + "%"
    m = _CI.search(txt)
    if m and not rec.get("effect_ci"):
        rec["effect_ci"] = f"{m.group(1)}-{m.group(2)}"
    m = _P.search(txt)
    if m and not rec.get("p_value"):
        rec["p_value"] = m.group(1)
    m = _N.search(txt)
    if m and not rec.get("n_total"):
        rec["n_total"] = m.group(1).replace(",", "")
    return rec

"""The canonical trial record schema shared across the pipeline.

One dict per trial. Column order below is also the CSV column order.
Tier 1 = always populated from source metadata/abstracts.
Tier 2 = populated by the extraction step (LLM or rules) when available.
Meta   = provenance/bookkeeping.
"""

# Tier 1 — core, from source metadata + abstract
TIER1 = [
    "id",                 # canonical dedupe key (doi / pmid / nct), see normalize.py
    "title",
    "source",             # pubmed | clinicaltrials | europepmc | ictrp
    "source_id",          # PMID / NCT number / PMCID etc.
    "url",
    "date",               # publication or last-updated date (YYYY-MM-DD)
    "place",              # country / site(s)
    "design",             # e.g. cluster-randomized, phase 3, double-blind
    "status",             # ongoing | completed | results posted | published
    "interventions_raw",  # free-text intervention description
    "intervention_class", # normalised vocab (see normalize.py)
    "comparator",
    "population",         # e.g. children <5, pregnant women, all ages
    "n_total",            # total enrolled
    "primary_outcome",
    "impact_summary",     # human-readable 1-2 sentence headline result
]

# Tier 2 — extracted when present
TIER2 = [
    "effect_metric",      # e.g. protective efficacy, incidence rate ratio, hazard ratio
    "effect_estimate",    # point estimate (number or short string)
    "effect_ci",          # 95% CI as text, e.g. "0.42-0.71"
    "p_value",
    "follow_up",          # e.g. "12 months"
    "species",            # P. falciparum | P. vivax | mixed
    "transmission_setting",  # endemicity / seasonality
    "age_range",
    "safety_summary",
    "funder",
]

# Meta — bookkeeping
META = [
    "abstract",           # kept for extraction; dropped from the CSV view
    "first_seen",         # ISO date this item first entered the dataset
    "run_type",           # weekly | backfill
]

FIELDS = TIER1 + TIER2 + META

# Columns written to the human-facing CSV (abstract omitted for readability)
CSV_FIELDS = [f for f in FIELDS if f != "abstract"]


def make_record(**kwargs):
    """Return a record dict with every field present (empty string default)."""
    rec = {f: "" for f in FIELDS}
    rec.update({k: v for k, v in kwargs.items() if k in rec})
    return rec

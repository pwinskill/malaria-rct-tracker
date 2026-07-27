"""Canonical IDs and controlled-vocabulary normalisation.

Keeps a raw text field AND a normalised class so the dataset stays sliceable.
"""
import re

# Controlled vocabulary for intervention class. Ordered: first match wins.
_INTERVENTION_RULES = [
    ("vaccine", r"\b(vaccine|rts,?s|r21|matrix-m|pfspz|immuni[sz])"),
    ("monoclonal antibody", r"\b(monoclonal|mab\b|cis43|l9ls|antibody)"),
    ("chemoprevention", r"\b(smc|iptp|ipti|iptc|chemoprevention|seasonal malaria chemoprevention|"
                        r"intermittent preventive)"),
    ("treatment/ACT", r"\b(artemisinin|artesunate|act\b|acts\b|coartem|lumefantrine|"
                      r"amodiaquine|dihydroartemisinin|piperaquine|primaquine|tafenoquine|"
                      r"chloroquine|treatment)"),
    ("ITN/LLIN", r"\b(bed ?net|bednet|itn\b|llin\b|insecticide-treated|pyrethroid|"
                 r"pbo net|dual active)"),
    ("IRS", r"\b(indoor residual|irs\b|spraying)"),
    ("larval source management", r"\b(larvicid|larval source|biolarvicid)"),
    ("spatial repellent", r"\b(spatial repellent|repellent)"),
    ("endectocide", r"\b(ivermectin|endectocide)"),
    ("diagnostic", r"\b(rdt\b|rapid diagnostic|diagnostic|point-of-care|g6pd test)"),
    ("gene drive/GMM", r"\b(gene drive|genetically modified mosquito|wolbachia|sterile insect)"),
]

_SPECIES = [
    ("P. falciparum", r"\bfalciparum\b|\bp\.?\s?f\b"),
    ("P. vivax", r"\bvivax\b|\bp\.?\s?v\b"),
]


def canonical_id(rec):
    doi = (rec.get("_doi") or "").strip().lower()
    if doi:
        return f"doi:{doi}"
    src, sid = rec.get("source", ""), str(rec.get("source_id", "")).strip()
    if src == "clinicaltrials" and sid:
        return f"nct:{sid.lower()}"
    if sid:
        return f"pmid:{sid}" if src in ("pubmed", "europepmc") else f"{src}:{sid}"
    # last resort: normalised title
    return "title:" + re.sub(r"\W+", "", (rec.get("title") or "").lower())[:80]


def classify_intervention(text):
    t = (text or "").lower()
    hits = [name for name, pat in _INTERVENTION_RULES if re.search(pat, t)]
    return "; ".join(dict.fromkeys(hits)) if hits else ""


def guess_species(text):
    t = (text or "").lower()
    hits = [name for name, pat in _SPECIES if re.search(pat, t)]
    if len(hits) > 1:
        return "mixed"
    return hits[0] if hits else ""


def normalize(rec):
    rec["id"] = canonical_id(rec)
    blob = " ".join(str(rec.get(f, "")) for f in
                    ("title", "interventions_raw", "abstract"))
    if not rec.get("intervention_class"):
        rec["intervention_class"] = classify_intervention(blob)
    if not rec.get("species"):
        rec["species"] = guess_species(blob)
    return rec

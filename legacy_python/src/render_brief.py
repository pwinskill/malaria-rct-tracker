"""Prepend a dated, human-readable section to the markdown brief."""
import os

_MARKER = "<!-- New weekly entries are inserted directly below this line -->"

_HEADER = """# Malaria RCT Brief

Automated weekly scan for new malaria randomized controlled trials.
Sources: PubMed, ClinicalTrials.gov, Europe PMC (incl. medRxiv preprints and
Cochrane review metadata). Most recent week on top. The structured dataset lives
in data/trials.csv.

---

{marker}
""".format(marker=_MARKER)


def _entry(r):
    def line(label, key):
        v = r.get(key, "")
        return f"- **{label}:** {v}\n" if v else ""

    impact = r.get("impact_summary", "") or "(no result stated yet)"
    effect = " ".join(x for x in [r.get("effect_metric", ""),
                                   r.get("effect_estimate", ""),
                                   (f"(95% CI {r['effect_ci']})" if r.get("effect_ci") else "")]
                       if x)
    md = f"### {r.get('title', '(untitled)')}\n"
    src_id = (r.get("source", "") + (f" ({r.get('source_id')})" if r.get("source_id") else "")).strip()
    md += f"- **Source / ID:** {src_id}\n" if src_id else ""
    md += line("Place", "place")
    md += line("Design", "design")
    md += line("Status", "status")
    md += line("Intervention(s)", "interventions_raw")
    md += line("Class", "intervention_class")
    md += line("Population", "population")
    md += line("N", "n_total")
    md += f"- **Impact:** {impact}\n"
    if effect:
        md += f"- **Effect:** {effect}\n"
    md += line("Species", "species")
    md += line("Follow-up", "follow_up")
    md += f"- **Link:** {r.get('url', '')}\n"
    return md


def prepend(cfg, records, week_label):
    out = cfg["output"]
    path = os.path.join(out["outputs_dir"], out["brief_filename"])
    os.makedirs(out["outputs_dir"], exist_ok=True)

    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            body = f.read()
    else:
        body = _HEADER

    if records:
        section = [f"## Week of {week_label}\n"]
        section += [_entry(r) for r in records]
        block = "\n".join(section)
    else:
        block = f"## Week of {week_label}\n\nNo new malaria RCTs found this week.\n"

    if _MARKER in body:
        body = body.replace(_MARKER, _MARKER + "\n\n" + block, 1)
    else:
        body = _HEADER.replace(_MARKER, _MARKER + "\n\n" + block, 1)

    with open(path, "w", encoding="utf-8") as f:
        f.write(body)
    return path

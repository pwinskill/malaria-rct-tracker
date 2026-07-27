"""PubMed via NCBI E-utilities.

Strategy: esearch to get PMIDs in the date window, then efetch (XML) for
title/abstract/date/DOI. We deliberately combine the curated RCT publication
type with a free-text 'randomi[sz]ed' clause, because the [pt] tag lags a few
days/weeks behind brand-new articles.
"""
import os
import xml.etree.ElementTree as ET

from . import _http
from .record import make_record

ESEARCH = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
EFETCH = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

TERM = (
    '(malaria) AND '
    '("randomized controlled trial"[pt] OR randomized[tiab] OR randomised[tiab]) AND '
    '(trial[tiab] OR randomized controlled trial[pt])'
)


def _key(cfg):
    env = cfg["sources"]["pubmed"].get("api_key_env", "")
    return os.environ.get(env, "") if env else ""


def _common(cfg):
    p = {"db": "pubmed", "tool": cfg.get("tool_name", "malaria-rct-tracker"),
         "email": cfg.get("contact_email", "")}
    k = _key(cfg)
    if k:
        p["api_key"] = k
    return p


def fetch(cfg, start_date, end_date):
    """Return Tier-1 partial records for the date window [start, end]."""
    params = _common(cfg)
    params.update({
        "term": TERM, "retmode": "json", "retmax": 500, "sort": "date",
        "datetype": "pdat", "mindate": start_date, "maxdate": end_date,
    })
    r = _http.get(ESEARCH, params=params)
    ids = r.json().get("esearchresult", {}).get("idlist", [])
    if not ids:
        return []

    records = []
    for i in range(0, len(ids), 100):  # efetch in batches of 100
        batch = ids[i:i + 100]
        fp = _common(cfg)
        fp.update({"id": ",".join(batch), "retmode": "xml", "rettype": "abstract"})
        xml = _http.get(EFETCH, params=fp).text
        records.extend(_parse(xml))
    return records


def _text(node):
    return "".join(node.itertext()).strip() if node is not None else ""


def _parse(xml):
    out = []
    try:
        root = ET.fromstring(xml)
    except ET.ParseError:
        return out
    for art in root.findall(".//PubmedArticle"):
        pmid = _text(art.find(".//PMID"))
        title = _text(art.find(".//ArticleTitle"))
        abstract = " ".join(
            _text(a) for a in art.findall(".//Abstract/AbstractText")
        ).strip()
        journal = _text(art.find(".//Journal/Title"))
        # date: prefer ArticleDate, fall back to PubDate year
        y = _text(art.find(".//ArticleDate/Year")) or _text(art.find(".//PubDate/Year"))
        m = _text(art.find(".//ArticleDate/Month")) or _text(art.find(".//PubDate/Month")) or "01"
        d = _text(art.find(".//ArticleDate/Day")) or "01"
        date = _norm_date(y, m, d)
        doi = ""
        for aid in art.findall(".//ArticleId"):
            if aid.get("IdType") == "doi":
                doi = _text(aid)
        ptypes = [_text(p) for p in art.findall(".//PublicationType")]
        design = "randomized controlled trial" if any(
            "randomized" in p.lower() for p in ptypes) else ""
        out.append(make_record(
            title=title, source="pubmed", source_id=pmid,
            url=f"https://pubmed.ncbi.nlm.nih.gov/{pmid}/",
            date=date, design=design, status="published",
            primary_outcome="", abstract=abstract,
            funder="", interventions_raw="",
            # stash doi/journal in fields we have; doi drives dedupe in normalize
            comparator="", place="",
        ) | {"_doi": doi, "_journal": journal})
    return out


_MONTHS = {m: f"{i:02d}" for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"], 1)}


def _norm_date(y, m, d):
    if not y:
        return ""
    m = _MONTHS.get(m[:3], m if m.isdigit() else "01")
    d = d if d.isdigit() else "01"
    return f"{y}-{int(m):02d}-{int(d):02d}"

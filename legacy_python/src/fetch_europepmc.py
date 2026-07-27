"""Europe PMC REST search.

One robust endpoint that also folds in medRxiv preprints and Cochrane review
metadata. resultType=core returns abstracts. We page with cursorMark.
"""
from . import _http
from .record import make_record

BASE = "https://www.ebi.ac.uk/europepmc/webservices/rest/search"


def fetch(cfg, start_date, end_date):
    query = (
        'malaria AND (randomized OR randomised) AND trial '
        f'AND (FIRST_PDATE:[{start_date} TO {end_date}])'
    )
    params = {
        "query": query, "format": "json", "resultType": "core",
        "pageSize": 100, "cursorMark": "*",
    }
    out, seen_cursor, pages = [], set(), 0
    while pages < 50:
        data = _http.get(BASE, params=params).json()
        for res in data.get("resultList", {}).get("result", []):
            out.append(_parse(res))
        cursor = data.get("nextCursorMark")
        pages += 1
        if not cursor or cursor in seen_cursor:
            break
        seen_cursor.add(cursor)
        params["cursorMark"] = cursor
    return out


def _parse(res):
    pmid = res.get("pmid", "")
    doi = res.get("doi", "")
    src = res.get("source", "")
    ext_id = res.get("id", "")
    journal = res.get("journalInfo", {}).get("journal", {}).get("title", "") \
        or res.get("bookOrReportDetails", {}).get("publisher", "")
    is_preprint = src == "PPR"
    is_cochrane = "cochrane database syst rev" in (journal or "").lower()

    if doi:
        url = f"https://doi.org/{doi}"
    elif pmid:
        url = f"https://europepmc.org/article/MED/{pmid}"
    else:
        url = f"https://europepmc.org/article/{src}/{ext_id}"

    status = "preprint" if is_preprint else ("cochrane review" if is_cochrane else "published")

    return make_record(
        title=res.get("title", ""), source="europepmc",
        source_id=pmid or ext_id, url=url,
        date=res.get("firstPublicationDate", ""),
        design="", status=status,
        abstract=res.get("abstractText", ""),
    ) | {"_doi": doi, "_journal": journal}

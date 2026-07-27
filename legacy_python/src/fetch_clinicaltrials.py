"""ClinicalTrials.gov API v2.

Registered interventional, randomized malaria trials, filtered to those whose
record was last updated inside the window (catches new registrations AND newly
posted results). We fetch full study JSON and parse protocolSection, which is
more robust than guessing the field-projection names.
"""
from . import _http
from .record import make_record

BASE = "https://clinicaltrials.gov/api/v2/studies"


def fetch(cfg, start_date, end_date):
    advanced = (
        "AREA[StudyType]INTERVENTIONAL AND "
        "AREA[DesignAllocation]RANDOMIZED AND "
        f"AREA[LastUpdatePostDate]RANGE[{start_date},{end_date}]"
    )
    params = {
        "query.cond": "malaria",
        "filter.advanced": advanced,
        "pageSize": 100,
        "sort": "LastUpdatePostDate:desc",
    }
    out, token, pages = [], None, 0
    while pages < 50:  # hard safety bound
        if token:
            params["pageToken"] = token
        data = _http.get(BASE, params=params).json()
        for study in data.get("studies", []):
            rec = _parse(study)
            if rec:
                out.append(rec)
        token = data.get("nextPageToken")
        pages += 1
        if not token:
            break
    return out


def _g(d, *path, default=""):
    for k in path:
        if not isinstance(d, dict):
            return default
        d = d.get(k)
        if d is None:
            return default
    return d


def _parse(study):
    ps = study.get("protocolSection", {})
    nct = _g(ps, "identificationModule", "nctId")
    if not nct:
        return None
    title = _g(ps, "identificationModule", "briefTitle")
    status = _g(ps, "statusModule", "overallStatus")
    date = _g(ps, "statusModule", "lastUpdatePostDateStruct", "date")
    has_results = bool(_g(ps, "statusModule", "resultsFirstPostDateStruct", "date"))

    design = _g(ps, "designModule", "designInfo", "allocation")
    phases = _g(ps, "designModule", "phases", default=[])
    model = _g(ps, "designModule", "designInfo", "interventionModel")
    design_str = ", ".join(x for x in [design, model] + list(phases) if x)

    interventions = _g(ps, "armsInterventionsModule", "interventions", default=[])
    iv_names = "; ".join(i.get("name", "") for i in interventions if i.get("name"))

    n = _g(ps, "designModule", "enrollmentInfo", "count")
    locations = _g(ps, "contactsLocationsModule", "locations", default=[])
    countries = sorted({loc.get("country", "") for loc in locations if loc.get("country")})
    place = ", ".join(countries)

    sponsor = _g(ps, "sponsorCollaboratorsModule", "leadSponsor", "name")
    age_min = _g(ps, "eligibilityModule", "minimumAge")
    age_max = _g(ps, "eligibilityModule", "maximumAge")
    age = " to ".join(x for x in [age_min, age_max] if x)
    conditions = ", ".join(_g(ps, "conditionsModule", "conditions", default=[]))

    outcome = ""
    primaries = _g(ps, "outcomesModule", "primaryOutcomes", default=[])
    if primaries:
        outcome = primaries[0].get("measure", "")

    status_label = "results posted" if has_results else status.lower() if status else ""

    return make_record(
        title=title, source="clinicaltrials", source_id=nct,
        url=f"https://clinicaltrials.gov/study/{nct}",
        date=date, place=place, design=design_str, status=status_label,
        interventions_raw=iv_names, n_total=str(n) if n else "",
        primary_outcome=outcome, age_range=age, funder=sponsor,
        abstract=conditions,  # no abstract; conditions give extractor a hint
    ) | {"_doi": "", "_journal": ""}

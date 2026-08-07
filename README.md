# Malaria RCT Tracker

An **R package** that scans for **new randomized controlled trials (RCTs) relating to
malaria**, screens them for eligibility the way a systematic review would, extracts a
structured record for each, and keeps a growing dataset plus a human-readable brief.
Designed to run itself in GitHub Actions.

## How it works

The pipeline is deliberately built like a systematic review — a **sensitive search** feeding
a **precise eligibility screen** — because no search query alone can tell *"malaria is the
trial's purpose"* from *"malaria is mentioned in passing"*.

```
fetch (sensitive, malaria-scoped RCT search)
  -> deduplicate across sources & identifiers
  -> SCREEN   (LLM eligibility: include/exclude + reason)   <- precision
  -> EXTRACT  (LLM structured extraction on included only)  <- quality
  -> store
```

**1. Search** (favours recall; precision comes later):

- **PubMed** (NCBI E-utilities) — malaria scoped to MeSH/title/abstract, with a
  Cochrane-style sensitivity-maximising RCT filter. Carries recall.
- **ClinicalTrials.gov** (API v2) — interventional, randomized malaria registrations,
  incl. newly-posted results. Adds registered/ongoing trials and structured phase.
- **Europe PMC** — tighter query; its unique value is **medRxiv preprints**.

**2. Screen** — each candidate is judged by an LLM (systematic-review style): *is this an
RCT whose primary purpose is a malaria intervention, and not an early-phase study?* Every
candidate gets a recorded decision + reason. This removes non-trials (reviews, modelling
papers, commentaries) **and** real trials that only touch malaria tangentially (iron,
dengue, nutrition trials). Excluded candidates are logged for audit.

**3. Extract** — included records get structured extraction (LLM with a strict JSON schema,
or a regex fallback), including pulling `place` from the title.

Outputs:

- `data/trials.csv` — one row per included trial (the database you'll use).
- `data/trials.jsonl` — full records incl. abstracts (your extraction-review surface).
- `data/screened_out.jsonl` — excluded candidates + the reason (audit trail).
- `data/state.json` — dedupe memory, so nothing is screened or reported twice.
- `outputs/Malaria_RCT_Brief.md` — readable digest, newest week on top.
- `docs/index.html` — interactive explorer (see below); regenerated every run.

## Scope (what counts as in)

- **RCTs only** — randomized (incl. cluster-randomized). Reviews, observational and
  modelling studies are screened out.
- **Malaria as the trial's primary purpose** — drugs, vaccines, monoclonal antibodies,
  vector control (ITNs/IRS/spatial repellents/larviciding), chemoprevention (SMC/IPTp),
  diagnostics, elimination strategies.
- **All phases except early-phase** — Phase II/III/IV, "Not applicable" (typical for
  vector-control and cluster trials), and unstated phase are kept; **Phase I / first-in-human
  / dose-finding** are excluded. Phase is recorded in the `phase` column so you can slice further.
- **Results-only** (default) — the dataset holds trials that have **reported results**.
  Study protocols, bare registrations, and ongoing/recruiting trials with no posted results
  are excluded (published RCTs, results preprints, and ClinicalTrials.gov entries *with*
  results posted stay in). Flip `screening.include_ongoing: true` in `config.yaml` to also
  keep the forward-looking registry pipeline. Adjust `R/screen.R` if your scope differs.

## Extracted fields

**Identity / screening:** title, source & ID, journal, url, `record_type`
(trial/preprint/registration/protocol), `screening_decision` + `screening_reason` +
`screening_confidence`, `phase`.

**Dates** (kept unambiguous — no single overloaded `date` column):

- `publication_date` — when the paper was published (PubMed/Europe PMC). Never overwritten by a registry edit.
- `registry_updated` — ClinicalTrials.gov last-update-posted date (a registry edit, not a publication).
- `trial_start` / `trial_completion` — when the trial actually ran (ClinicalTrials.gov structured dates, or pulled from the abstract by the extractor).
- `first_seen` — when the record first entered *your* dataset (the run date).

**Tier 1:** place, design, status, intervention(s) + normalised class, comparator,
population, N, primary outcome, impact summary.

**Tier 2:** effect metric / estimate / 95% CI, p-value, follow-up, species, transmission
setting, age range, safety summary, funder. Plus `extracted_by` (which model/rules filled it).

## Setup on a new machine

Needs R (>= 4.1). Clone the repo, then install the package + dependencies:

```r
install.packages(c("httr2", "jsonlite", "xml2", "yaml"))   # runtime deps
devtools::load_all(".")        # work with it live  (or install.packages(".", repos = NULL, type = "source"))
```

Copy `.env.example` to `.env` (git-ignored) and add your key(s):

- `ANTHROPIC_API_KEY` — needed for `screening.mode: llm` and `extraction.mode: llm`. A
  **pay-as-you-go** key from https://console.anthropic.com (separate from a Claude.ai
  subscription). A dedicated key in a spend-limited workspace is recommended.
- `NCBI_API_KEY` — optional, free, raises PubMed rate limits.

## Running it

Always run from the **project root**.

```r
library(malariarct)      # or devtools::load_all(".")

run_weekly()                                  # weekly run (last ~10 days)
run_backfill(days = 3650)                     # last 10 years
run_backfill(start = "2015-01-01", end = "2020-12-31")
run_backfill(days = 3650, mode = "rules")     # extraction via regex only (screening still runs)
```

Or from a shell:

```bash
Rscript scripts/run_weekly.R
Rscript scripts/backfill.R --start 2015-01-01 --end 2020-12-31
```

Do a backfill **once** to seed the dataset locally, commit `data/`, then let the weekly
GitHub job take over. Re-runs are safe and cheap: already-decided records are never
re-screened or re-reported.

**Long runs are checkpointed and resumable.** The dataset is written to disk after every
`checkpoint_every` candidates (default 50, in `config.yaml`), so if a run is interrupted —
you stop it, the machine sleeps, the network drops, R crashes — all completed work is kept.
Just run the same command again and it **resumes**, skipping everything already decided and
processing only the remainder. So a 10-year backfill can be stopped and restarted freely, and
you never pay twice for the same record. A record is only ever re-processed if it was
**deferred** — either beyond the per-run screening cap, or because a transient API failure
(timeout, overloaded) meant it couldn't be screened this time; those retry on the next run
rather than being silently downgraded to a lower-quality rules decision.

After an interrupted run you *can* (but rarely need to) tidy up with `reconcile_dataset()`,
which removes any duplicate row a crash-at-the-wrong-instant might have left and rebuilds
`state.json` from what's actually stored.

## Models & cost

Two model tiers (set in `config.yaml`):

- **Screening** — `claude-haiku-4-5` (cheap; one short call per candidate).
- **Extraction** — `claude-sonnet-5` (stronger; runs only on the smaller included set).

A one-off 10-year backfill is roughly **$30–50**; the weekly job is pennies. For a free run,
set both `mode: rules`/`off`. **Cap real spend in the Anthropic Console** (prepaid credit
with auto-reload off, or a workspace spend limit) — that's the hard ceiling.

## Reviewing extraction quality

`data/trials.jsonl` has the source abstract next to every extracted field:

```r
library(jsonlite)
d <- stream_in(file("data/trials.jsonl"))
d[, c("title", "phase", "place", "abstract", "impact_summary", "effect_estimate", "extracted_by")]
```

`data/screened_out.jsonl` lets you audit what was *excluded* and why — spot-check for
false exclusions.

**Encoding.** The files are UTF-8 (abstracts contain `≥`, `–`, accented place names, etc.).
In R they read correctly with `readr::read_csv()` or `jsonlite::stream_in()`. **Excel** ignores
UTF-8 on a plain double-click and shows mojibake like `â‰¥` — open via *Data → From Text/CSV*
and pick **65001: Unicode (UTF-8)**, or read in R and export with `writexl::write_xlsx()`.

## Interactive explorer

Every run also writes a self-contained **`docs/index.html`** — an interactive explorer for
the whole dataset. It's a single file with the data embedded inline — no server, no build
step, no external assets — so you just open it:

```r
render_explorer()                 # rebuild it any time from the stored dataset
browseURL("docs/index.html")      # open it in your browser
```

What's in it:

- **Summary cards**, a *trials-over-time* chart (toggle publication vs. trial-start date) and a
  *by-intervention-class* chart. Each chart states how much of the current selection it actually
  plots — the trial-start view covers well under half the dataset, and that should be visible
  rather than read as a real decline.
- **Evidence gap matrix** — intervention class against **country / outcome family / population
  band / publication era**. The point of it is the *empty* cells, which are drawn as explicit
  hatched blanks rather than white space. Click any cell to filter the table to it. Counts are
  trials, not evidence quality, and a trial with several classes or outcomes appears in each of
  its cells, so cells don't sum to the total.
- **Where trials happen** — countries ranked by trial count. An optional per-burden view exists
  but is **off by default**; see [Burden denominators](#burden-denominators-optional-off-by-default).
- **Trial start to publication** — the lag distribution, with a median. Covers the ~40% of
  records carrying both dates.
- **Activity timeline** — a GitHub-contribution-style density grid, intervention class down,
  year across, shaded by how many trials were **running** that year (a trial occupies every
  year between its start and completion, so a column is concurrent activity, not new trials).
  Click a cell to filter to it. Two honest caveats are shown on the chart and matter:
  `trial_start` is only ~41% filled **and its coverage is uneven by class** — 50% for ITN/LLIN,
  29% for vaccine — so each row carries its own `n/N`, and a sparse row may mean missing dates
  rather than no research. The recent years are also **right-censored**: a trial that started
  recently usually hasn't published, so it isn't in the dataset at all, and the fall-off on the
  right is an artefact. A **Published** toggle switches to publication year (98.6% filled) —
  near-complete, but it shows when evidence landed, not when the work was done.
- **Filters**: free-text search (multi-word narrows, it doesn't match the raw phrase), plus
  intervention class / phase / species / country / outcome family / population / record type,
  over a sortable table.
  The country facet is built from the normalised `countries` column, not from splitting the
  free-text `place` — see [Geography](#geography).
- **Click any row for the full record.** The table shows 8 columns; the drawer shows all 39
  fields grouped as the schema groups them, with links rebuilt from the record `id`
  (DOI / PubMed / ClinicalTrials.gov). Fields the extractor didn't fill are shown as
  *not reported* rather than hidden — with `effect_ci` at 52% and `funder` at 17%, an absent
  value is itself information.
- **Shareable URLs.** Every filter, the sort, and the open trial live in the address bar, so a
  view can be pasted into an email or a protocol and reopens exactly. Back closes the drawer.
- **Extraction coverage panel** (under the filters): per-field fill rates *for the current
  selection*, plus one-click flags for records the extractor struggled with — estimate without
  a CI, N that is really a cluster count, no publication date, no country, no class, rules-only
  extraction. Check this before you use any field as an analysis variable.
- **Export** the filtered set as **BibTeX** or **RIS**. (For CSV, just use `data/trials.csv`
  directly — you have R.)

Because it's a plain static file, it works offline and could later be served with **GitHub Pages**
(Settings → Pages → Deploy from branch → `main` → `/docs`). Note that on a **private** repo,
GitHub Pages is **public** unless you're on a paid plan — so publishing it would expose the
embedded dataset. It's left unpublished by default; keep it local, or enable Pages deliberately.

## Burden denominators (optional, off by default)

The geography chart can show **trials per unit of malaria burden** instead of raw trial counts.
Raw counts largely restate where research infrastructure is; the normalised view shows where the
evidence base is thin *relative to where malaria actually is*, and it recomputes under whatever
filter is active — so "per-burden coverage of vaccine trials" is answerable, not just the global
picture.

**It ships off.** There is no `data/burden.csv` in the repo, so the toggle is hidden and the chart
shows plain counts. Nothing to maintain unless you want the view. To turn it on, create the file:

```
country,cases,year,source
Nigeria,66800000,2022,WHO World Malaria Report 2023
Uganda,12700000,2022,WHO World Malaria Report 2023
```

Then `render_explorer()`. No re-screening, no extraction, no cost.

Notes if you do populate it:

- `country` must match the controlled names in the `countries` column (see [Geography](#geography)).
- Countries **absent from the file are omitted** from the normalised view — never treated as zero
  burden — and the chart states how much of the current selection it could cover.
- Only *relative* burden matters to the ranking, so **share-of-global-cases works as well as
  absolute counts and ages far more slowly** — WHO's per-country shares move by a fraction of a
  percentage point a year, while absolute estimates move with global totals and methodology
  revisions. Record the `year` either way so staleness is visible.
- Half-populating it is worse than leaving it off: the chart looks complete unless the note is read.

## Geography

`place` is whatever the extractor read off the abstract — `"Uganda"`, but also
`"India (Gujarat: Kheda, Vadodara, Panchmahal districts)"` and `"Africa (five African
countries)"`. It's kept verbatim for display, and two **derived** columns are stored
alongside it for slicing:

- **`countries`** — controlled country names, `"; "`-joined (`"Burkina Faso; Mali"`).
- **`region`** — a multi-country region (`"Africa"`, `"Southeast Asia"`) when no country is
  named, so region-level records stay visible instead of vanishing from a country facet.

Two further controlled columns are derived the same way, and drive the gap matrix:

- **`outcome_family`** — what the trial measured (`therapeutic efficacy`, `clinical incidence`,
  `infection prevalence`, `entomological`, `mortality`, `pregnancy/birth`, `anaemia`,
  `immunogenicity`, `safety`, `coverage/cost`). Taken from `primary_outcome` — the pre-specified
  endpoint — falling back to `effect_metric`/`impact_summary` only when that names nothing
  recognisable. **84% coverage.** It exists so intervention can be crossed against outcome; it is
  *not* a licence to compare effect sizes, since two trials in one cell can still differ in
  endpoint definition, comparator and follow-up.
- **`population_band`** — `pregnant women`, `infants (<1y)`, `children <5`, `school-age children`,
  `children (age unspecified)`, `adults`, `all ages`. **67% coverage.**

Both come from an ordered gazetteer in `R/normalize.R`. Rules are applied in order and each
match is **consumed** before the next rule is tried, which is what keeps the containment cases
right: *Papua New Guinea* is claimed and removed before the bare *Guinea* rule sees the string,
likewise *South Sudan* before *Sudan* and *South Africa* before the *Africa* region. Spelling
variants collapse (both DRC forms, both apostrophes in *Côte d'Ivoire*, *The Gambia*/*Gambia*).

This is regex over text you already have — **no LLM, no API calls, no cost** — so it's always
safe to re-run. After editing the rules:

```r
reclassify_store()   # re-derive intervention_class + countries/region for every stored record
```

which rewrites `trials.csv` / `trials.jsonl` and regenerates the brief and explorer. Run it
after **any** schema change that adds a derived column, too — a plain append can't rewrite the
CSV header.

## Deleting a record after manual review

If you review the dataset and want to remove a record — a false positive the screen
let through — the **wrong** way is to just delete its row from `trials.csv`. That works
only by accident (its id is still in `state.json`, which blocks re-adding), and it comes
straight back the day you reset state or run a fresh backfill.

The **right** way is the durable blocklist. From R:

```r
exclude_records("doi:10.1016/j.xyz.2025.01.002", reason = "not actually a malaria trial")
```

That does two things: appends the id to `data/exclusions.txt` **and** removes the row from
`trials.csv` / `trials.jsonl`. You can pass several ids at once (`exclude_records(c("pmid:123", "nct:nct0456"))`).

`data/exclusions.txt` is a plain, hand-editable list (one id per line, `#` comments allowed) —
you can also just open it and paste ids in. The pipeline reads it **before screening on every
run** and drops anything matching, by id **or any identity key** — so an excluded trial stays
out even if it later reappears under a different identifier, and even after a full re-backfill.

Use the `id` column (e.g. `doi:…`, `pmid:…`, `nct:…`) as the key. Commit `data/exclusions.txt`
along with the rest of `data/` so the exclusion is honoured on other machines and by the
GitHub Actions runs.

## Automating with GitHub Actions

- **`.github/workflows/weekly.yml`** — Monday scan, commits results back.
- **`.github/workflows/backfill.yml`** — manual "Run workflow" deep backfill (days / start /
  end / mode / max_items inputs).
- **`.github/workflows/check.yml`** — runs the test suite on code changes.

Setup: push to GitHub; add `ANTHROPIC_API_KEY` (and optionally `NCBI_API_KEY`) as an Actions
secret; Settings → Actions → General → allow **Read and write** workflow permissions. Works
on private repos (uses your free Actions minutes; this job uses very few).

## Concurrency & conflicts

`data/` is committed, so anything that writes it is a writer: your local runs, the weekly
cron, and the manual backfill workflow. If two write at once (e.g. you run a local backfill
while the Monday job fires), you get a git merge conflict on the `data/` files — and because
each run de-dupes against *its own* `state.json`, a trial in the overlapping window can be
appended twice.

**Best practice — keep a single writer.** The cleanest setup is to let **GitHub own the
dataset**: run backfills via the **manual `backfill.yml` workflow**, not locally. Both CI
workflows share a `concurrency` group, so they never overlap, and the weekly job does
`git pull --rebase` before pushing. Use local runs for testing (on throwaway data) or, if you
do write locally, **`git pull` first and `git push` immediately after** — or pause the weekly
workflow while a long local backfill runs.

**If a conflict happens anyway**, two things make it painless:

- `.gitattributes` sets a **union merge** on `trials.csv` / `trials.jsonl` / `screened_out.jsonl` /
  `exclusions.txt`, so both sides' rows are kept automatically instead of raising a conflict.
- After resolving the merge (take either side of `state.json` — it gets rebuilt), run:

  ```r
  reconcile_dataset()
  ```

  which removes any duplicate rows and rebuilds `state.json` to match what's actually stored
  (plus the screened-out log). `seen_ids` is only ever unioned, so nothing is forgotten.

## Tests

```r
testthat::test_local()
```

## Project layout

```
DESCRIPTION  NAMESPACE  config.yaml     # package metadata + all settings
R/
  http.R  llm.R  record.R              # HTTP helper; Anthropic client; record schema
  fetch_pubmed.R  fetch_clinicaltrials.R  fetch_europepmc.R  fetch_ictrp.R
  normalize.R  dedupe.R  screen.R  extract.R  store.R
  render_brief.R  render_explorer.R    # markdown brief; interactive HTML explorer
  config.R  pipeline.R  utils.R        # config/.env; orchestration; helpers
inst/           explorer_template.html # the explorer page template (data injected in)
scripts/        run_weekly.R  backfill.R
tests/testthat/                        # unit tests
data/           trials.csv  trials.jsonl  screened_out.jsonl  state.json
outputs/        Malaria_RCT_Brief.md
docs/           index.html             # self-contained interactive explorer
.github/workflows/                     # weekly.yml, backfill.yml, check.yml
legacy_python/                         # the old Python version (safe to delete)
```

## Notes / limits

- Precision comes from the screen, not the query — if you see a wrong inclusion/exclusion,
  tune the eligibility rules in `R/screen.R`, not the search.
- Phase is a drug/vaccine concept; vector-control trials are "Not applicable" phase and are
  deliberately kept (see Scope).
- One source failing (timeout etc.) is logged and skipped; the run completes with the others.
- Extraction never overwrites a value a source already provided; it only fills gaps.
- Keep this repo **outside** cloud-synced folders (OneDrive/Dropbox); git + file-sync fight over `.git`.
```

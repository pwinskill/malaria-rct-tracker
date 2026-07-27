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
re-screened or re-reported (only "deferred" ones — beyond the per-run screening cap — retry).

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
the whole dataset: summary cards, a *trials-over-time* chart (toggle publication vs. trial-start
date), a *by-intervention-class* chart, free-text search, and filters (class / phase / species /
country / record type) over a sortable table. It's a single file with the data embedded inline —
no server, no build step, no external assets — so you just open it:

```r
render_explorer()                 # rebuild it any time from the stored dataset
browseURL("docs/index.html")      # open it in your browser
```

Because it's a plain static file, it works offline and could later be served with **GitHub Pages**
(Settings → Pages → Deploy from branch → `main` → `/docs`). Note that on a **private** repo,
GitHub Pages is **public** unless you're on a paid plan — so publishing it would expose the
embedded dataset. It's left unpublished by default; keep it local, or enable Pages deliberately.

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

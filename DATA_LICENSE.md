# Data licence and provenance

Covers everything under `data/` and `outputs/`, and the explorer at
`docs/index.html`. The code is MIT — see [LICENSE.md](LICENSE.md).

## Licence

The dataset is released under
[Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

Use it, change it, build on it, including commercially. Just credit the source.

The bibliographic facts here — that a trial exists, its identifiers, when it
ran — are not ours to license and in most jurisdictions are not copyrightable
at all. CC BY 4.0 applies to the part that *is* a creative act: the selection
of which trials qualify, and the structured fields derived for each one.

## What this data is

Machine-generated and unvalidated. Specifically:

- Candidate trials come from a deliberately sensitive search of PubMed,
  ClinicalTrials.gov and Europe PMC.
- **An LLM decides eligibility.** Every include/exclude call is a model's
  judgement, recorded with its reason in `screening_reason`.
- **An LLM extracts every field.** Population, outcome, effect estimate,
  funder and the rest are a model's reading of an abstract, not a transcription
  of a validated source. `extracted_by` records what filled each record.
- **No human has reviewed any of it**, and no accuracy audit against a
  reference set has been carried out. False inclusions, false exclusions and
  misread fields should all be assumed present at an unknown rate.

Do not use it as a citable source, and do not use it as the evidence base for a
systematic review without checking every record you rely on. It is a way of
finding trials quickly, not a substitute for reading them.

## Checking a record

Every record carries `url` plus a prefixed canonical `id` (`doi:`, `pmid:` or
`nct:`), so each one resolves to the publisher or registry entry it came from.
That entry is the authority; this dataset is not.

Excluded candidates are kept in `data/screened_out.jsonl` with the reason for
each, so the screen can be audited in both directions rather than only where it
said yes.

## Abstracts

Source abstracts are **not** stored or redistributed. They are fetched, used in
memory to screen and extract, and dropped. Abstracts are publisher copyright;
reasoning over one is not the same as republishing a corpus of them. Follow the
link on any record to read it at the source.

## Attribution

> Malaria RCT Tracker — https://github.com/pwinskill/malaria-rct-tracker
> LLM-screened and LLM-extracted; unvalidated.

Please keep the second line. The limitation is the most important thing about
this dataset, and it should travel with it.

# Canonical identifiers and controlled-vocabulary normalisation.
#
# Every record keeps a raw text field AND a normalised class so the dataset
# stays sliceable. Identity is multi-key: a record can carry a DOI, one or more
# NCT numbers, and one or more PMIDs, and any shared key links two records
# (see dedupe.R). This is what lets a ClinicalTrials.gov registration and its
# later PubMed publication collapse into a single trial.

# Controlled vocabulary for intervention class. A trial may match several
# classes (e.g. a vaccine given alongside chemoprevention), so ALL matches are
# returned, joined by "; ". Order below only affects the order they appear in.
.INTERVENTION_RULES <- list(
  c("vaccine",              "\\b(vaccine|rts,?s|r21|matrix-m|pfspz|circumsporozoite|immuni[sz])"),
  c("monoclonal antibody",  "\\b(monoclonal|\\bmab\\b|cis43|l9ls|antibody)"),
  # Chemoprevention split into WHO strategy subtypes. Each is a distinct label;
  # the generic "chemoprevention" is a catch-all suppressed in
  # classify_intervention() whenever a specific subtype also matched.
  c("SMC",                  "\\bsmc\\b|seasonal malaria chemoprevention|seasonal chemoprevention"),
  c("IPTp",                 paste0("\\biptp\\b|iptp[- ]?sp|intermittent preventive treatment in preg|",
                                   "intermittent preventive treatment.{0,15}pregnan")),
  c("PMC",                  paste0("\\b(pmc|ipti)\\b|perennial malaria chemoprevention|",
                                   "intermittent preventive treatment in infan|",
                                   "intermittent preventive treatment.{0,15}infan")),
  c("IPTsc",                paste0("\\biptsc\\b|intermittent preventive treatment.{0,20}school|",
                                   "school-?age.{0,15}chemoprevention")),
  c("PDMC",                 "\\bpdmc\\b|post[- ]?discharge.{0,15}malaria chemoprevention|post[- ]?discharge.{0,15}chemoprevention"),
  c("MDA",                  "\\bmda\\b|mass drug administration"),
  c("chemoprevention",      "\\b(iptc|chemoprevention|chemoprophylaxis)\\b|intermittent preventive treatment"),
  c("treatment/ACT",        paste0("\\b(artemisinin|artesunate|act\\b|acts\\b|coartem|lumefantrine|",
                                   "amodiaquine|dihydroartemisinin|piperaquine|primaquine|tafenoquine|",
                                   "chloroquine)")),
  c("ITN/LLIN",             paste0("\\b(bed ?net|bednet|itn\\b|llin\\b|insecticide-treated|",
                                   "pyrethroid|pbo net|dual active)")),
  c("IRS",                  "\\b(indoor residual|irs\\b|spraying)"),
  c("larval source management", "\\b(larvicid|larval source|biolarvicid)"),
  c("spatial repellent",    "\\b(spatial repellent|repellent)"),
  c("endectocide",          "\\b(ivermectin|endectocide)"),
  c("diagnostic",           "\\b(rdt\\b|rapid diagnostic|diagnostic|point-of-care|g6pd test)"),
  c("gene drive/GMM",       "\\b(gene drive|genetically modified mosquito|wolbachia|sterile insect)")
)

.SPECIES_RULES <- list(
  c("P. falciparum", "\\bfalciparum\\b|\\bp\\.?\\s?f\\b"),
  c("P. vivax",      "\\bvivax\\b|\\bp\\.?\\s?v\\b")
)

# All normalised identity keys for a record, e.g. c("doi:10.1/x", "nct:nct01",
# "pmid:123"). Non-numeric source ids (e.g. Europe PMC preprint ids) are NOT
# labelled as pmids.
id_keys <- function(rec) {
  keys <- character(0)

  doi <- tolower(trimws(rec$.doi %||% ""))
  if (nzchar(doi)) keys <- c(keys, paste0("doi:", doi))

  ncts <- character(0)
  if (identical(rec$source, "clinicaltrials") && has_text(rec$source_id)) {
    ncts <- c(ncts, as.character(rec$source_id))
  }
  ncts <- c(ncts, rec$.ncts %||% character(0))
  for (nct in ncts) {
    nct <- tolower(trimws(nct))
    if (nzchar(nct)) keys <- c(keys, paste0("nct:", nct))
  }

  sid <- trimws(as.character(rec$source_id %||% ""))
  if ((rec$source %||% "") %in% c("pubmed", "europepmc") && grepl("^[0-9]+$", sid)) {
    keys <- c(keys, paste0("pmid:", sid))
  }
  for (p in rec$.pmids %||% character(0)) {
    p <- trimws(as.character(p))
    if (grepl("^[0-9]+$", p)) keys <- c(keys, paste0("pmid:", p))
  }

  unique(keys)
}

# The single canonical id stored in the `id` column: prefer DOI, then NCT, then
# PMID, then source:id, then a normalised title.
canonical_id <- function(rec) {
  keys <- id_keys(rec)
  if (length(keys)) {
    for (pref in c("doi:", "nct:", "pmid:")) {
      hit <- keys[startsWith(keys, pref)]
      if (length(hit)) return(hit[1])
    }
    return(keys[1])
  }
  src <- rec$source %||% ""
  sid <- trimws(as.character(rec$source_id %||% ""))
  if (nzchar(sid)) return(paste0(src, ":", sid))
  paste0("title:", substr(gsub("[^a-z0-9]", "", tolower(rec$title %||% "")), 1, 80))
}

# Specific chemoprevention subtypes; the generic "chemoprevention" label is
# dropped when any of these matched, so an SMC trial reads "SMC", not
# "SMC; chemoprevention".
.CHEMO_SUBTYPES <- c("SMC", "IPTp", "PMC", "IPTsc", "PDMC")

classify_intervention <- function(text) {
  t <- tolower(text %||% "")
  hits <- character(0)
  for (rule in .INTERVENTION_RULES) {
    if (grepl(rule[2], t, perl = TRUE)) hits <- c(hits, rule[1])
  }
  if (any(.CHEMO_SUBTYPES %in% hits)) hits <- setdiff(hits, "chemoprevention")
  if (!length(hits)) return("")
  paste(unique(hits), collapse = "; ")
}

guess_species <- function(text) {
  t <- tolower(text %||% "")
  hits <- character(0)
  for (rule in .SPECIES_RULES) {
    if (grepl(rule[2], t, perl = TRUE)) hits <- c(hits, rule[1])
  }
  if (length(hits) > 1) return("mixed")
  if (length(hits)) return(hits[1])
  ""
}

# Set id, intervention_class and species on a record (in place, returned).
normalize_record <- function(rec) {
  rec$id <- canonical_id(rec)
  blob <- paste(
    rec$title %||% "", rec$interventions_raw %||% "",
    rec$abstract %||% "", rec$conditions %||% "",
    collapse = " "
  )
  if (!has_text(rec$intervention_class)) rec$intervention_class <- classify_intervention(blob)
  if (!has_text(rec$species))            rec$species <- guess_species(blob)
  rec
}

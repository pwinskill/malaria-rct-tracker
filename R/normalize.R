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

# --- outcome families ------------------------------------------------------
# What the trial actually measured, as a controlled multi-label vocabulary.
# Derived from `primary_outcome` first - that is the pre-specified endpoint and
# the only rigorous source - falling back to effect_metric/impact_summary only
# when the primary outcome names nothing recognisable.
#
# This exists so the gap matrix can cross intervention against outcome. It is
# NOT a licence to compare effect sizes across trials: two trials in the same
# cell can still have different endpoints, comparators and follow-up.
.OUTCOME_RULES <- list(
  c("therapeutic efficacy", paste0("\\bacpr\\b|adequate clinical and parasitolog|cure rate|",
                                   "treatment failure|recrudescen|parasite clearance|",
                                   "\\bpcr[- ]corrected|reinfection|recurrence")),
  # The compound forms come FIRST inside the alternation: PCRE is leftmost-first,
  # so a bare "mosquito" alternative would match and leave " mortality" behind for
  # the mortality rule to claim - reading vector mortality as human mortality.
  c("entomological",        paste0("(mosquito|vector|knock[- ]?down) mortalit|",
                                   "mosquito|anophel|biting rate|\\beir\\b|entomolog|",
                                   "vector densit|blood[- ]?feed|insecticide resistance|",
                                   "sporozoite rate|knock[- ]?down|landing catch")),
  c("pregnancy/birth",      paste0("birth ?weight|low birthweight|\\blbw\\b|placental|preterm|",
                                   "stillbirth|gestational|pregnancy outcome|maternal an")),
  c("mortality",            "mortalit|\\bdeath|survival|case fatality"),
  c("clinical incidence",   paste0("clinical malaria|malaria incidence|incidence of malaria|",
                                   "uncomplicated malaria|malaria episode|febrile episode|",
                                   "severe malaria|incidence rate of")),
  c("infection prevalence", paste0("parasit(a?emia|e prevalence)|infection prevalence|",
                                   "prevalence of.{0,20}(infection|parasit)|positivit|",
                                   "\\bpcr prevalence|parasite densit|gametocyt")),
  c("anaemia",              "an[ae]mia|h[ae]moglobin|\\bhb\\b"),
  c("immunogenicity",       "antibod|seroconver|immunogenic|\\bigg\\b|titre|titer|seroprevalen"),
  c("safety",               "adverse event|\\bsafety\\b|tolerabilit|\\bsae\\b|toxicit"),
  c("coverage/cost",        paste0("net use|coverage|adherence|uptake|cost[- ]effectiv|cost per|",
                                   "\\bicer\\b|acceptab|knowledge, attitude"))
)

classify_outcome <- function(primary, fallback = "") {
  hits <- .match_consume(tolower(primary %||% ""), .OUTCOME_RULES)$hits
  if (!length(hits)) hits <- .match_consume(tolower(fallback %||% ""), .OUTCOME_RULES)$hits
  paste(unique(hits), collapse = "; ")
}

# --- population bands ------------------------------------------------------
# Ordered most-specific-first and match-and-consume, so "children under 5" is
# claimed by the under-5 rule and never also counted as generic "children".
.POPULATION_RULES <- list(
  c("pregnant women",       "pregnan|\\biptp\\b|antenatal|primigrav|multigrav"),
  c("infants (<1y)",        "\\binfant|neonat|\\bnewborn|under (one|1) year|0[-\u2013]11 months|<1 ?year"),
  c("children <5",          paste0("under[- ]?fives?|under[- ]?5\\b|<5 ?year|under 5 year|",
                                   "6[-\u2013]59 months|preschool|pre-school|",
                                   "children (aged )?(under|<) ?5")),
  c("school-age children",  paste0("school[- ]?age|schoolchild|school children|",
                                   "5[-\u2013]1[0-9] ?year|6[-\u2013]1[0-9] ?year|adolescen")),
  c("children (age unspecified)", "child|p(a)?ediatric|\\bkids\\b"),
  c("adults",               "\\badults?\\b|\\bmen\\b|\\bwomen\\b|18 ?years and"),
  c("all ages",             "all ages|general population|community[- ]wide|whole population|entire population")
)

# Consuming a match removes only the text it matched, not the concept - so
# "children aged 6-59 months" leaves a bare "children" behind, and "pregnant
# women" leaves "women". Suppress the general band whenever a specific one of the
# same kind fired, exactly as classify_intervention() does for chemoprevention.
.POP_SPECIFIC_CHILD <- c("infants (<1y)", "children <5", "school-age children")
classify_population <- function(text) {
  hits <- .match_consume(tolower(text %||% ""), .POPULATION_RULES)$hits
  if (any(.POP_SPECIFIC_CHILD %in% hits)) hits <- setdiff(hits, "children (age unspecified)")
  if ("pregnant women" %in% hits) hits <- setdiff(hits, "adults")
  paste(hits, collapse = "; ")
}

# --- geography -------------------------------------------------------------
# `place` is free text written by the extractor: "Uganda", but also
# "India (Gujarat: Kheda, Vadodara, Panchmahal districts)" and "Africa (five
# African countries)". Splitting it on commas - which is what the explorer used
# to do - yields fragments like "Gourcy)" and "Kano State)", and splits real
# countries across spellings ("Democratic Republic of Congo" vs "...of the
# Congo", U+2019 vs U+0027 in "Cote d'Ivoire"). So we derive a controlled
# `countries` field instead and keep `place` purely for display.
#
# Rules are applied IN ORDER and each match is consumed from the text before the
# next rule is tried. That is what keeps the containment cases honest without a
# thicket of lookarounds: "Papua New Guinea" is matched and removed before the
# bare "Guinea" rule ever sees the string, likewise South Sudan before Sudan and
# South Africa before the Africa region. Order here is therefore load-bearing:
# most specific first.
.COUNTRY_RULES <- list(
  # --- names that contain another country name (must come first) ---
  c("Papua New Guinea",   "\\bpapua new guinea\\b|\\bpng\\b"),
  c("Equatorial Guinea",  "\\bequatorial guinea\\b|\\bbioko\\b"),
  c("Guinea-Bissau",      "\\bguinea[- ]bissau\\b"),
  c("Guinea",             "\\bguinea\\b"),
  c("Democratic Republic of the Congo",
                          "democratic republic of (the )?congo|\\bdr[c ]?congo\\b|\\bdrc\\b"),
  c("Republic of the Congo", "republic of (the )?congo|\\bcongo[- ]brazzaville\\b"),
  c("South Sudan",        "\\bsouth sudan\\b"),
  c("Sudan",              "\\bsudan\\b"),
  c("South Africa",       "\\bsouth african?\\b"),
  c("Central African Republic", "central african republic"),
  c("Dominican Republic", "dominican republic"),
  # --- everything else, alphabetical ---
  c("Afghanistan",        "\\bafghanistan\\b"),
  c("Angola",             "\\bangola\\b"),
  c("Bangladesh",         "\\bbangladesh\\b"),
  c("Benin",              "\\bbenin\\b"),
  c("Bhutan",             "\\bbhutan\\b"),
  c("Bolivia",            "\\bbolivia\\b"),
  c("Botswana",           "\\bbotswana\\b"),
  c("Brazil",             "\\bbrazil\\b"),
  c("Burkina Faso",       "\\bburkina( faso)?\\b"),
  c("Burundi",            "\\bburundi\\b"),
  c("Cambodia",           "\\bcambodia\\b"),
  c("Cameroon",           "\\bcameroon\\b"),
  c("Chad",               "\\bchad\\b"),
  c("China",              "\\bchina\\b|\\bchinese\\b"),
  c("Colombia",           "\\bcolombia\\b"),
  c("Comoros",            "\\bcomoros\\b|\\banjouan\\b"),
  c("Costa Rica",         "\\bcosta rica\\b"),
  # both apostrophes (U+0027 / U+2019), accented or not, plus the English name
  c("Cote d'Ivoire",      "c[o\u00f4]te ?d.?ivoire|ivory coast"),
  c("Djibouti",           "\\bdjibouti\\b"),
  c("Ecuador",            "\\becuador\\b"),
  c("Eritrea",            "\\beritrea\\b"),
  c("Eswatini",           "\\beswatini\\b|\\bswaziland\\b"),
  c("Ethiopia",           "\\bethiopian?\\b"),
  c("French Guiana",      "french guiana"),
  c("Gabon",              "\\bgabon\\b|\\blambar[e\u00e9]n[e\u00e9]\\b"),
  c("Gambia",             "\\b(the )?gambia\\b"),
  c("Ghana",              "\\bghana\\b"),
  c("Guatemala",          "\\bguatemala\\b"),
  c("Guyana",             "\\bguyana\\b"),
  c("Haiti",              "\\bhaiti\\b"),
  c("Honduras",           "\\bhonduras\\b"),
  c("India",              "\\bindia\\b"),   # not "Indian" - "Indian Ocean" is not India
  c("Indonesia",          "\\bindonesian?\\b|\\bsumba\\b|\\bpapua\\b"),
  c("Iran",               "\\biran\\b"),
  c("Kenya",              "\\bkenyan?\\b"),
  c("Laos",               "\\blaos\\b|\\blao pdr\\b|\\blao people"),
  c("Liberia",            "\\bliberia\\b"),
  c("Madagascar",         "\\bmadagascar\\b"),
  c("Malawi",             "\\bmalawi(an)?\\b"),
  c("Malaysia",           "\\bmalaysian?\\b|\\bsabah\\b|\\bsarawak\\b"),
  c("Mali",               "\\bmali\\b|\\bmalian\\b"),
  c("Mauritania",         "\\bmauritania\\b"),
  c("Mexico",             "\\bmexico\\b"),
  c("Mozambique",         "\\bmozambique\\b|\\bmozambican\\b"),
  c("Myanmar",            "\\bmyanmar\\b|\\bburma\\b|\\bburmese\\b"),
  c("Namibia",            "\\bnamibia\\b"),
  c("Nepal",              "\\bnepal\\b"),
  c("Nicaragua",          "\\bnicaragua\\b"),
  c("Nigeria",            "\\bnigerian?\\b"),
  c("Niger",              "\\bniger\\b"),
  c("Pakistan",           "\\bpakistan\\b"),
  c("Panama",             "\\bpanama\\b"),
  c("Peru",               "\\bperu\\b|\\bperuvian\\b"),
  c("Philippines",        "\\bphilippines\\b"),
  c("Rwanda",             "\\brwanda\\b"),
  c("Sao Tome and Principe", "s[a\u00e3]o tom[e\u00e9]"),
  c("Saudi Arabia",       "saudi arabia"),
  c("Senegal",            "\\bsenegal(ese)?\\b"),
  c("Sierra Leone",       "sierra leone"),
  c("Solomon Islands",    "solomon islands?"),
  c("Somalia",            "\\bsomalia\\b"),
  c("South Korea",        "south korea|republic of korea"),
  c("Sri Lanka",          "sri lanka"),
  c("Suriname",           "\\bsuriname\\b"),
  c("Tanzania",           "\\btanzanian?\\b|\\bzanzibar\\b|\\bbagamoyo\\b|\\bifakara\\b"),
  c("Thailand",           "\\bthailand\\b|\\bthai\\b"),
  c("Timor-Leste",        "timor[- ]leste|east timor"),
  c("Togo",               "\\btogo\\b"),
  c("Uganda",             "\\bugandan?\\b|\\btororo\\b|\\bjinja\\b"),
  c("Vanuatu",            "\\bvanuatu\\b"),
  c("Venezuela",          "\\bvenezuela\\b"),
  c("Vietnam",            "\\bvi[e\u00ea]t ?nam(ese)?\\b"),
  c("Yemen",              "\\byemen\\b"),
  c("Zambia",             "\\bzambian?\\b"),
  c("Zimbabwe",           "\\bzimbabwe\\b"),
  # non-endemic trial / challenge-study sites
  c("Australia",          "\\baustralian?\\b"),
  c("Belgium",            "\\bbelgium\\b"),
  c("Canada",             "\\bcanada\\b"),
  c("Denmark",            "\\bdenmark\\b"),
  c("France",             "\\bfrance\\b"),   # not "French" - cf. "French-speaking Africa"
  c("Germany",            "\\bgermany\\b|\\bgerman\\b|\\bt[u\u00fc]bingen\\b"),
  c("Japan",              "\\bjapan\\b"),
  c("Netherlands",        "\\bnetherlands\\b|\\bdutch\\b|\\bnijmegen\\b"),
  c("Spain",              "\\bspain\\b|\\bbarcelona\\b"),
  c("Sweden",             "\\bsweden\\b"),
  c("Switzerland",        "\\bswitzerland\\b|\\bswiss\\b"),
  c("United Kingdom",     "united kingdom|\\buk\\b|\\bengland\\b|\\bscotland\\b|\\boxford\\b|\\blondon\\b"),
  # deliberately NOT "American": it would swallow "Latin American" before the
  # region rules ever run, and "Pan American Health Organization" besides.
  c("United States",      "united states|\\busa\\b|\\bu\\.s\\.\\b")
)

# Multi-country descriptions that are NOT a country. Applied after (and only to
# the text left over by) the country rules, so "South Africa" never leaves a
# stray "Africa" behind. Kept as a separate field so a region-level record is
# visibly region-level rather than silently dropped from the country facet.
.REGION_RULES <- list(
  c("sub-Saharan Africa", "sub[- ]?saharan african?"),
  c("West Africa",        "\\bwest(ern)? african?\\b"),
  c("East Africa",        "\\beast(ern)? african?\\b"),
  c("Central Africa",     "\\bcentral african?\\b"),
  c("Southern Africa",    "\\bsouthern african?\\b"),
  c("Africa",             "\\bafrican?\\b"),
  c("Greater Mekong",     "greater mekong|mekong (sub)?region"),
  c("Southeast Asia",     "south[- ]?east asian?\\b"),
  c("South Asia",         "\\bsouth asian?\\b"),
  c("Sahel",              "\\bsahel(ian)?\\b"),
  c("Amazon",             "\\bamazon(ian)?\\b"),
  c("Latin America",      "latin america|south america"),
  c("Western Pacific",    "western pacific|asia[- ]pacific")
)

# Apply an ordered rule list, consuming each match so a later, more general rule
# cannot re-match text a specific rule already claimed. Returns the labels hit
# plus whatever text is left over.
.match_consume <- function(t, rules) {
  hits <- character(0)
  for (rule in rules) {
    if (grepl(rule[2], t, perl = TRUE)) {
      hits <- c(hits, rule[1])
      t <- gsub(rule[2], " ", t, perl = TRUE)
    }
  }
  list(hits = unique(hits), rest = t)
}

# Derive controlled `countries` / `region` from the free-text place (falling back
# to the title when place is empty or names nothing recognisable). Returns both
# as "; "-joined strings, empty when nothing matched.
normalize_place <- function(place, title = "") {
  classify <- function(text) {
    t <- tolower(text %||% "")
    got <- .match_consume(t, .COUNTRY_RULES)
    reg <- .match_consume(got$rest, .REGION_RULES)
    list(countries = paste(got$hits, collapse = "; "),
         region    = paste(reg$hits, collapse = "; "))
  }
  out <- classify(place)
  if (!nzchar(out$countries)) {
    from_title <- classify(title)
    # Only take the title's countries; a title-derived region is too weak a
    # signal to be worth recording ("malaria in Africa" says nothing).
    if (nzchar(from_title$countries)) out$countries <- from_title$countries
  }
  out
}

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

# Fields derived from text the EXTRACTOR fills in, so they cannot be computed at
# normalise time (which runs before screening). Applied to each record just
# before it is stored, and re-applied over the whole store by reclassify_store().
# Purely local and free - no API calls.
#
# NOT idempotent across a store round-trip, though: the blob below includes the
# abstract, and abstracts are never persisted. Re-running this over records read
# back from disk therefore classifies on strictly less text than the first pass
# had. reclassify_store() warns about exactly this.
derive_fields <- function(rec) {
  blob <- paste(rec$title %||% "", rec$interventions_raw %||% "",
                rec$abstract %||% "", rec$conditions %||% "", collapse = " ")
  rec$intervention_class <- classify_intervention(blob)
  geo <- normalize_place(rec$place %||% "", rec$title %||% "")
  rec$countries <- geo$countries
  rec$region    <- geo$region
  rec$outcome_family <- classify_outcome(
    rec$primary_outcome %||% "",
    paste(rec$effect_metric %||% "", rec$impact_summary %||% ""))
  rec$population_band <- classify_population(
    paste(rec$population %||% "", rec$age_range %||% "", rec$title %||% ""))
  rec
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

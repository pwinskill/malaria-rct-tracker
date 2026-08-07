test_that("canonical_id prefers DOI and lower-cases it", {
  rec <- new_record(source = "pubmed", source_id = "123", title = "x")
  rec$.doi <- "10.1/AbC"
  expect_equal(canonical_id(rec), "doi:10.1/abc")
})

test_that("canonical_id falls back NCT -> PMID -> source -> title", {
  ct <- new_record(source = "clinicaltrials", source_id = "NCT01")
  expect_equal(canonical_id(ct), "nct:nct01")

  pm <- new_record(source = "pubmed", source_id = "999")
  expect_equal(canonical_id(pm), "pmid:999")

  bare <- new_record(source = "europepmc", source_id = "PPR7")
  expect_equal(canonical_id(bare), "europepmc:PPR7")

  untitled <- new_record(source = "x", title = "A Big Trial!")
  expect_equal(canonical_id(untitled), "title:abigtrial")
})

test_that("id_keys bridges doi, nct and pmid on one record", {
  rec <- new_record(source = "pubmed", source_id = "123")
  rec$.doi <- "10.1/x"
  rec$.ncts <- "NCT01"
  ks <- id_keys(rec)
  expect_true(all(c("doi:10.1/x", "nct:nct01", "pmid:123") %in% ks))
})

test_that("non-numeric source ids are not mislabelled as pmids", {
  rec <- new_record(source = "europepmc", source_id = "PPR123")
  expect_false(any(startsWith(id_keys(rec), "pmid:")))
})

test_that("classify_intervention is multi-label", {
  cls <- classify_intervention("RTS,S vaccine given alongside SMC")
  expect_true(grepl("vaccine", cls))
  expect_true(grepl("SMC", cls))
})

test_that("bare 'treatment' no longer over-classifies as ACT", {
  expect_equal(classify_intervention("standard treatment of fever"), "")
  expect_true(grepl("treatment/ACT", classify_intervention("artesunate-amodiaquine")))
})

test_that("chemoprevention is split into WHO strategy subtypes", {
  expect_equal(classify_intervention("Seasonal malaria chemoprevention with SP+AQ"), "SMC")
  expect_equal(classify_intervention("IPTp-SP in pregnant women"), "IPTp")
  expect_equal(classify_intervention("perennial malaria chemoprevention (PMC) in infants"), "PMC")
  expect_equal(classify_intervention("intermittent preventive treatment in school children"), "IPTsc")
  expect_true(grepl("PDMC", classify_intervention("post-discharge malaria chemoprevention")))
})

test_that("a specific chemoprevention subtype suppresses the generic label", {
  # 'SMC ... chemoprevention' must read as SMC, not 'SMC; chemoprevention'
  cls <- classify_intervention("SMC seasonal malaria chemoprevention trial")
  expect_true(grepl("SMC", cls))
  expect_false(grepl("chemoprevention", cls))
})

test_that("classify_intervention output casing is stable (no case-split categories)", {
  expect_equal(classify_intervention("smc"), classify_intervention("SMC"))
  expect_equal(classify_intervention("IPTp"), classify_intervention("iptp"))
})

test_that("guess_species detects single and mixed", {
  expect_equal(guess_species("P. falciparum only"), "P. falciparum")
  expect_equal(guess_species("both P. falciparum and P. vivax"), "mixed")
  expect_equal(guess_species("no species named"), "")
})

# --- geography -------------------------------------------------------------

test_that("normalize_place ignores the parenthesised site lists in `place`", {
  # These are the strings that made comma-splitting produce "Gourcy)" and
  # "Kano State)" as country-filter options.
  expect_equal(normalize_place("India (Gujarat: Kheda, Vadodara, Panchmahal districts)")$countries, "India")
  expect_equal(normalize_place("Burkina Faso (Niangoloko, Gourcy)")$countries, "Burkina Faso")
  expect_equal(normalize_place("Nigeria (Madobi, Kano State)")$countries, "Nigeria")
})

test_that("normalize_place collapses spelling variants onto one country", {
  drc <- "Democratic Republic of the Congo"
  expect_equal(normalize_place("Democratic Republic of Congo")$countries, drc)
  expect_equal(normalize_place(drc)$countries, drc)
  expect_equal(normalize_place("DRC")$countries, drc)
  # straight vs curly apostrophe, accented or not, plus the English name
  expect_equal(normalize_place("Cote d'Ivoire")$countries, "Cote d'Ivoire")
  expect_equal(normalize_place("C\u00f4te d\u2019Ivoire")$countries, "Cote d'Ivoire")
  expect_equal(normalize_place("Ivory Coast")$countries, "Cote d'Ivoire")
  expect_equal(normalize_place("The Gambia")$countries, "Gambia")
})

test_that("a country name containing another country name wins", {
  # match-and-consume ordering: the specific rule fires and removes the text
  # before the general rule is ever tried
  expect_equal(normalize_place("Papua New Guinea")$countries, "Papua New Guinea")
  expect_equal(normalize_place("Equatorial Guinea")$countries, "Equatorial Guinea")
  expect_equal(normalize_place("Guinea-Bissau")$countries, "Guinea-Bissau")
  expect_equal(normalize_place("Guinea")$countries, "Guinea")
  expect_equal(normalize_place("South Sudan")$countries, "South Sudan")
  expect_equal(normalize_place("Sudan")$countries, "Sudan")
  # "Nigeria" must not also register as "Niger", nor South Africa as a region
  expect_equal(normalize_place("Nigeria")$countries, "Nigeria")
  expect_equal(normalize_place("Niger")$countries, "Niger")
  expect_equal(normalize_place("South Africa")$countries, "South Africa")
  expect_equal(normalize_place("South Africa")$region, "")
})

test_that("multi-country places yield every country", {
  expect_equal(normalize_place("Burkina Faso, Mali")$countries, "Burkina Faso; Mali")
})

test_that("region-level places are recorded as a region, not dropped", {
  a <- normalize_place("Africa (five African countries)")
  expect_equal(a$countries, "")
  expect_equal(a$region, "Africa")
  expect_equal(normalize_place("Southeast Asia (11 sites)")$region, "Southeast Asia")
  expect_equal(normalize_place("sub-Saharan Africa")$region, "sub-Saharan Africa")
})

test_that("normalize_place falls back to the title only when place names nothing", {
  expect_equal(normalize_place("", "A bed net trial in Uganda")$countries, "Uganda")
  # an explicit place is authoritative; the title must not add to it
  expect_equal(normalize_place("Kenya", "A trial in Uganda")$countries, "Kenya")
  expect_equal(normalize_place("", "")$countries, "")
})

test_that("derive_fields fills countries/region on a record", {
  rec <- derive_fields(new_record(title = "t", place = "Uganda and Kenya"))
  expect_equal(rec$countries, "Kenya; Uganda")
  expect_equal(rec$region, "")
})

# --- outcome families ------------------------------------------------------

test_that("classify_outcome recognises the main endpoint families", {
  expect_equal(classify_outcome("Adequate clinical and parasitological response at day 28"),
               "therapeutic efficacy")
  expect_equal(classify_outcome("Incidence of clinical malaria"), "clinical incidence")
  expect_equal(classify_outcome("Prevalence of P. falciparum parasitaemia"), "infection prevalence")
  expect_equal(classify_outcome("Low birth weight"), "pregnancy/birth")
  expect_equal(classify_outcome("Seroconversion and antibody titres"), "immunogenicity")
})

test_that("mosquito mortality is entomological, not human mortality", {
  # The compound alternative has to be matched before the bare "mosquito" one,
  # or "mosquito" is consumed and " mortality" is left for the mortality rule.
  expect_equal(classify_outcome("Mosquito mortality in experimental huts"), "entomological")
  expect_equal(classify_outcome("All-cause child mortality"), "mortality")
  # a trial measuring both still gets both
  expect_equal(classify_outcome("Mosquito mortality and child mortality"),
               "entomological; mortality")
})

test_that("classify_outcome falls back only when the primary outcome names nothing", {
  expect_equal(classify_outcome("", "vaccine efficacy against clinical malaria"),
               "clinical incidence")
  # a usable primary outcome wins; the fallback must not add to it
  expect_equal(classify_outcome("Mortality", "entomological inoculation rate"), "mortality")
})

# --- population bands ------------------------------------------------------

test_that("a specific population band suppresses the general one", {
  # consuming the match leaves the rest of the phrase behind, so "children aged
  # 6-59 months" would otherwise register as under-5 AND generic children
  expect_equal(classify_population("children aged 6-59 months"), "children <5")
  expect_equal(classify_population("school-age children 5-15 years"), "school-age children")
  expect_equal(classify_population("infants under 1 year"), "infants (<1y)")
  expect_equal(classify_population("children"), "children (age unspecified)")
  # "pregnant women" must not also count as "adults" on the strength of "women"
  expect_equal(classify_population("pregnant women"), "pregnant women")
})

test_that("genuinely mixed populations keep every band", {
  expect_equal(classify_population("adults and children"), "children (age unspecified); adults")
  expect_equal(classify_population("all ages, community-wide"), "all ages")
  expect_equal(classify_population("no population stated"), "")
})

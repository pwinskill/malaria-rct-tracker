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

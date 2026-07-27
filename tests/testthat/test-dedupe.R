test_that("a CT.gov registration and its PubMed publication merge via the NCT bridge", {
  ct <- normalize_record(new_record(
    source = "clinicaltrials", source_id = "NCT9",
    title = "Trial", status = "completed"
  ))
  pm <- new_record(source = "pubmed", source_id = "555", title = "Trial (published)")
  pm$.doi <- "10.9/z"
  pm$.ncts <- "NCT9"
  pm <- normalize_record(pm)

  merged <- merge_within_batch(list(ct, pm))
  expect_length(merged, 1)
  keys <- merged[[1]]$.all_keys
  expect_true("nct:nct9" %in% keys)
  expect_true("doi:10.9/z" %in% keys)
  expect_true("pmid:555" %in% keys)
})

test_that("distinct trials are not merged", {
  a <- normalize_record(new_record(source = "pubmed", source_id = "1", title = "A"))
  b <- normalize_record(new_record(source = "pubmed", source_id = "2", title = "B"))
  expect_length(merge_within_batch(list(a, b)), 2)
})

test_that("split_new suppresses items seen under any identifier, across runs", {
  pm <- new_record(source = "pubmed", source_id = "555")
  pm$.ncts <- "NCT9"
  merged <- merge_within_batch(list(normalize_record(pm)))

  state_fresh <- list(reported = character(0), seen_ids = character(0))
  expect_length(split_new(merged, state_fresh), 1)

  state_seen <- list(reported = character(0), seen_ids = "nct:nct9")
  expect_length(split_new(merged, state_seen), 0)
})

test_that("a real abstract wins over ClinicalTrials.gov conditions when merged", {
  ct <- normalize_record(new_record(
    source = "clinicaltrials", source_id = "NCT7", title = "Trial",
    conditions = "Malaria, Falciparum"))
  pm <- new_record(source = "pubmed", source_id = "42", title = "Trial (published)",
                   abstract = "A real abstract describing the randomized malaria trial results.")
  pm$.ncts <- "NCT7"
  pm <- normalize_record(pm)
  merged <- merge_within_batch(list(ct, pm))[[1]]
  expect_true(grepl("real abstract", merged$abstract))       # publication abstract kept
  expect_equal(merged$conditions, "Malaria, Falciparum")     # conditions preserved separately
})

test_that("merge fills empty fields from lower-priority sources without overwriting", {
  ct <- normalize_record(new_record(
    source = "clinicaltrials", source_id = "NCT5",
    title = "Registered title", place = "Kenya"
  ))
  pm <- new_record(source = "pubmed", source_id = "77", title = "Published title")
  pm$.ncts <- "NCT5"
  pm$abstract <- "Some abstract"
  pm <- normalize_record(pm)

  merged <- merge_within_batch(list(ct, pm))[[1]]
  expect_equal(merged$title, "Registered title")  # CT.gov wins (higher priority)
  expect_equal(merged$place, "Kenya")
  expect_equal(merged$abstract, "Some abstract")  # gap filled from PubMed
})

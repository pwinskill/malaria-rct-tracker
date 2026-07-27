test_that("ClinicalTrials.gov parsing extracts explicit trial dates, phase and results status", {
  study <- list(protocolSection = list(
    identificationModule = list(nctId = "NCT999", briefTitle = "A randomised malaria trial"),
    statusModule = list(
      overallStatus = "COMPLETED",
      lastUpdatePostDateStruct    = list(date = "2024-05-01"),
      startDateStruct             = list(date = "2019-06"),
      primaryCompletionDateStruct = list(date = "2021-12"),
      resultsFirstPostDateStruct  = list(date = "2023-01-15")),
    designModule = list(designInfo = list(allocation = "RANDOMIZED"), phases = list("PHASE3")),
    conditionsModule = list(conditions = list("Malaria"))
  ))
  rec <- .parse_ct_study(study)
  expect_equal(rec$registry_updated, "2024-05-01")   # last registry edit
  expect_equal(rec$trial_start, "2019-06")           # when the trial ran
  expect_equal(rec$trial_completion, "2021-12")
  expect_equal(rec$phase, "Phase 3")
  expect_equal(rec$status, "results posted")         # resultsFirstPostDate present
  expect_equal(rec$conditions, "Malaria")
  expect_false(nzchar(rec$publication_date))         # CT.gov never sets a publication date
})

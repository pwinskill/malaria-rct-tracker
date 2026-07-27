rec_with <- function(...) normalize_record(new_record(...))

test_that("screening off-mode includes everything", {
  recs <- list(rec_with(title = "anything"))
  out <- screen_records(recs, list(screening = list(mode = "off")))
  expect_equal(out[[1]]$screening_decision, "include")
})

test_that("rule-based screen keeps a malaria RCT", {
  r <- rec_with(title = "A randomized controlled trial of R21/Matrix-M malaria vaccine in children",
                abstract = "We randomly assigned children to R21 vaccine or control and measured malaria incidence.")
  out <- .screen_rules(r)
  expect_equal(out$screening_decision, "include")
})

test_that("rule-based screen drops a review", {
  r <- rec_with(title = "Primary hyperhidrosis: an updated review",
                abstract = "A narrative review of treatments.")
  expect_equal(.screen_rules(r)$screening_decision, "exclude")
})

test_that("rule-based screen drops a non-malaria RCT", {
  r <- rec_with(title = "Randomized trial of iron supplementation on child growth",
                abstract = "Children were randomly assigned to iron or placebo; growth was measured.")
  expect_equal(.screen_rules(r)$screening_decision, "exclude")
})

test_that("rule-based screen drops an early-phase study", {
  r <- rec_with(title = "A phase 1 first-in-human trial of a malaria monoclonal antibody",
                abstract = "Healthy adults were randomly assigned in this phase I dose-escalation study of malaria prophylaxis.")
  expect_equal(.screen_rules(r)$screening_decision, "exclude")
})

test_that("ClinicalTrials.gov phase codes normalise", {
  expect_equal(.ct_phase(list("PHASE3")), "Phase 3")
  expect_equal(.ct_phase(list("PHASE2", "PHASE3")), "Phase 2/3")
  expect_equal(.ct_phase(list("NA")), "Not applicable")
  expect_equal(.ct_phase(list("EARLY_PHASE1")), "Early Phase 1")
  expect_equal(.ct_phase(list()), "")
})

test_that("screen mapping: eligible -> include; fills phase/class only when empty", {
  rec <- new_record(source = "pubmed", source_id = "1", title = "x")
  out <- .screen_apply(rec, list(eligible = TRUE, record_type = "trial", phase = "Phase 3",
                                 intervention_class = "vaccine", reason = "malaria RCT", confidence = "high"))
  expect_equal(out$screening_decision, "include")
  expect_equal(out$phase, "Phase 3")
  expect_equal(out$intervention_class, "vaccine")
})

test_that("screen mapping preserves a fetcher's structural record_type", {
  rec <- new_record(source = "clinicaltrials", source_id = "NCT1", record_type = "registration")
  out <- .screen_apply(rec, list(eligible = TRUE, record_type = "trial", phase = "",
                                 reason = "r", confidence = "low"))
  expect_equal(out$record_type, "registration")   # not downgraded to "trial"
})

test_that("screen mapping refines a generic 'trial' record_type", {
  rec <- new_record(source = "pubmed", record_type = "trial")
  out <- .screen_apply(rec, list(eligible = FALSE, record_type = "review", phase = "",
                                 reason = "r", confidence = "low"))
  expect_equal(out$record_type, "review")
  expect_equal(out$screening_decision, "exclude")
})

test_that("results-only scope excludes study protocols", {
  r <- rec_with(title = "Study protocol for a two-arm cluster-randomised controlled trial of window screens against malaria",
                abstract = "This is a study protocol; no efficacy results are yet reported.")
  r$screening_decision <- "include"          # as if the model let it through
  expect_equal(.apply_scope(r, include_ongoing = FALSE)$screening_decision, "exclude")
})

test_that("results-only scope drops ongoing registrations but keeps results-posted ones", {
  ongoing <- rec_with(source = "clinicaltrials", record_type = "registration",
                      status = "recruiting", title = "A randomised trial of SMC in Mali")
  ongoing$screening_decision <- "include"
  expect_equal(.apply_scope(ongoing, FALSE)$screening_decision, "exclude")

  posted <- rec_with(source = "clinicaltrials", record_type = "registration",
                     status = "results posted", title = "A randomised trial of SMC in Mali")
  posted$screening_decision <- "include"
  expect_equal(.apply_scope(posted, FALSE)$screening_decision, "include")
})

test_that("include_ongoing keeps ongoing registrations but still drops protocol papers", {
  reg <- rec_with(source = "clinicaltrials", record_type = "registration",
                  status = "recruiting", title = "A randomised malaria trial")
  reg$screening_decision <- "include"
  expect_equal(.apply_scope(reg, TRUE)$screening_decision, "include")

  proto <- rec_with(record_type = "protocol", title = "Study protocol for a malaria RCT")
  proto$screening_decision <- "include"
  expect_equal(.apply_scope(proto, TRUE)$screening_decision, "exclude")
})

test_that("rule screen excludes a record already tagged as a review", {
  r <- rec_with(title = "Vaccines for preventing malaria",
                abstract = "We included randomized controlled trials of malaria vaccines.",
                record_type = "review")
  expect_equal(.screen_rules(r)$screening_decision, "exclude")
})

test_that("rule screen keeps a phase 3 trial that merely cites prior phase 1 work", {
  r <- rec_with(title = "A phase 3 randomised trial of the R21 malaria vaccine",
                abstract = "Following the phase 1 immunogenicity results, we randomly assigned children to R21 or control.")
  expect_equal(.screen_rules(r)$screening_decision, "include")
})

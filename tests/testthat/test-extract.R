test_that("apply_rules pulls efficacy, CI, p-value and N from an abstract", {
  rec <- new_record(
    abstract = "Vaccine efficacy was 75% (95% CI 60 to 85), p=0.001, among n=1,200 children."
  )
  rec <- apply_rules(rec)
  expect_equal(rec$effect_metric, "efficacy (%)")
  expect_equal(rec$effect_estimate, "75%")
  expect_equal(rec$effect_ci, "60-85")
  expect_equal(rec$p_value, "0.001")
  expect_equal(rec$n_total, "1200")
})

test_that("apply_rules picks a result-ish first sentence as impact_summary", {
  rec <- new_record(
    abstract = "Background text here. Incidence was reduced by half in the intervention arm."
  )
  rec <- apply_rules(rec)
  expect_true(grepl("Incidence was reduced", rec$impact_summary))
})

test_that("rules extract a place named in the title", {
  rec <- new_record(title = "Efficacy of the R21 vaccine in children in Burkina Faso")
  rec <- apply_rules(rec)
  expect_true(grepl("Burkina Faso", rec$place))
})

test_that("place matching drops substrings of longer country names", {
  expect_equal(.place_from_text("A cluster-randomised trial in Papua New Guinea"), "Papua New Guinea")
  p <- .place_from_text("conducted in the Democratic Republic of the Congo")
  expect_true(grepl("Democratic Republic of the Congo", p))
  expect_false(grepl("(^|, )Congo(,|$)", p))       # standalone 'Congo' dropped
})

test_that("extract mapping fills only empty fields and stamps extracted_by", {
  rec <- new_record(place = "Kenya")
  out <- .extract_apply(rec, list(place = "Uganda", n_total = "1500", impact_summary = "it worked"),
                        "claude-sonnet-5")
  expect_equal(out$place, "Kenya")                 # not overwritten
  expect_equal(out$n_total, "1500")                # filled
  expect_equal(out$extracted_by, "llm:claude-sonnet-5")
})

test_that("apply_rules never overwrites a value already present", {
  rec <- new_record(abstract = "efficacy was 40%", effect_estimate = "already-set")
  rec <- apply_rules(rec)
  expect_equal(rec$effect_estimate, "already-set")
})

test_that("enrich in 'off' mode is a no-op and 'rules' mode fills gaps", {
  recs <- list(new_record(abstract = "efficacy was 33%"))
  cfg_off <- list(extraction = list(mode = "off"))
  expect_equal(enrich(recs, cfg_off)[[1]]$effect_estimate, "")

  cfg_rules <- list(extraction = list(mode = "rules"))
  expect_equal(enrich(recs, cfg_rules)[[1]]$effect_estimate, "33%")
})

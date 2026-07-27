mk_expl_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-expl-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
                     brief_filename = "B.md", state_file = "state.json",
                     trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
                     screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt",
                     explorer_dir = file.path(d, "docs"), explorer_filename = "index.html"))
}

test_that("render_explorer writes a self-contained page with the data embedded", {
  cfg <- mk_expl_cfg()
  recs <- list(
    normalize_record(new_record(source = "pubmed", source_id = "1",
      title = "Vaccine RCT in Uganda", place = "Uganda", phase = "Phase 3",
      publication_date = "2023-05-01", impact_summary = "Efficacy was 55%.")),
    normalize_record(new_record(source = "clinicaltrials", source_id = "NCT9",
      title = "Bed net trial in Mali", place = "Mali", phase = "Not applicable",
      trial_start = "2021"))
  )
  p <- render_explorer(cfg, records = recs)
  expect_true(file.exists(p))

  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Vaccine RCT in Uganda", html, fixed = TRUE))
  expect_true(grepl("Bed net trial in Mali", html, fixed = TRUE))
  expect_false(grepl("__DATA__", html, fixed = TRUE))   # token was replaced
  expect_false(grepl("__META__", html, fixed = TRUE))
  # no external resources: nothing is fetched over the network
  expect_false(grepl("https?://[^\"']+\\.(js|css)", html))
})

test_that("render_explorer handles an empty dataset without erroring", {
  cfg <- mk_expl_cfg()
  p <- render_explorer(cfg, records = list())
  expect_true(file.exists(p))
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("const TRIALS = [];", html, fixed = TRUE))
})

test_that("render_explorer defaults to reading the stored dataset", {
  cfg <- mk_expl_cfg()
  append_records(cfg, list(normalize_record(new_record(
    source = "pubmed", source_id = "7", title = "Stored trial", abstract = "secret abstract"))))
  p <- render_explorer(cfg)              # no records arg -> reads trials.jsonl
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Stored trial", html, fixed = TRUE))
  expect_false(grepl("secret abstract", html, fixed = TRUE))   # abstract dropped from the payload
})

test_that("embedded JSON cannot break out of the <script> tag", {
  cfg <- mk_expl_cfg()
  rec <- normalize_record(new_record(source = "pubmed", source_id = "1",
    title = "Nasty </script><script>alert(1)</script> title"))
  p <- render_explorer(cfg, records = list(rec))
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # the injected data must not contain a raw closing tag; "<\/script>" is the safe form
  expect_false(grepl("</script><script>alert", html, fixed = TRUE))
  expect_true(grepl("<\\/script>", html, fixed = TRUE))
})

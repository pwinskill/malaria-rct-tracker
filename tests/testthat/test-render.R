make_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-brief-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(
    data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
    brief_filename = "Brief.md", state_file = "state.json",
    trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
    screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt"))
}

test_that("render_brief writes an entry with title, week (first_seen) and link", {
  cfg <- make_cfg()
  rec <- new_record(title = "My Trial", url = "http://example/1", impact_summary = "It worked",
                    source = "pubmed", source_id = "1", first_seen = "2025-01-06")
  p <- render_brief(cfg, list(rec))
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_true(grepl("My Trial", txt))
  expect_true(grepl("Week of 2025-01-06", txt))
  expect_true(grepl("http://example/1", txt))
})

test_that("newest first_seen section is on top", {
  cfg <- make_cfg()
  recs <- list(new_record(title = "Older", url = "u1", first_seen = "2025-01-01"),
               new_record(title = "Newer", url = "u2", first_seen = "2025-02-01"))
  p <- render_brief(cfg, recs)
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_lt(regexpr("Week of 2025-02-01", txt), regexpr("Week of 2025-01-01", txt))
})

test_that("render_brief defaults to the stored dataset and is self-healing", {
  cfg <- make_cfg()
  append_records(cfg, list(normalize_record(new_record(
    source = "pubmed", source_id = "9", title = "Stored trial", first_seen = "2025-03-03"))))
  p <- render_brief(cfg)                       # no records arg -> reads trials.jsonl
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_true(grepl("Stored trial", txt))
  expect_equal(lengths(regmatches(txt, gregexpr("## Week of", txt))), 1L)
})

test_that("an empty dataset yields a header-only brief (no filler section)", {
  cfg <- make_cfg()
  p <- render_brief(cfg, list())
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_false(grepl("## Week of", txt))
  expect_true(grepl("Malaria RCT Brief", txt))
})

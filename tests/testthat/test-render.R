make_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-brief-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(outputs_dir = file.path(d, "outputs"), brief_filename = "Brief.md"))
}

test_that("prepend_brief writes an entry with title, week and link", {
  cfg <- make_cfg()
  rec <- new_record(title = "My Trial", url = "http://example/1",
                    impact_summary = "It worked", source = "pubmed", source_id = "1")
  p <- prepend_brief(cfg, list(rec), "2025-01-06")
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_true(grepl("My Trial", txt))
  expect_true(grepl("Week of 2025-01-06", txt))
  expect_true(grepl("http://example/1", txt))
})

test_that("newest week is inserted on top", {
  cfg <- make_cfg()
  prepend_brief(cfg, list(new_record(title = "Older", url = "u1")), "2025-01-01")
  p <- prepend_brief(cfg, list(new_record(title = "Newer", url = "u2")), "2025-02-01")
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_lt(regexpr("Week of 2025-02-01", txt), regexpr("Week of 2025-01-01", txt))
})

test_that("an empty run does not add filler once the brief exists", {
  cfg <- make_cfg()
  prepend_brief(cfg, list(new_record(title = "Only", url = "u")), "2025-01-01")
  p <- prepend_brief(cfg, list(), "2025-02-01")
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_false(grepl("Week of 2025-02-01", txt))
  expect_equal(lengths(regmatches(txt, gregexpr("## Week of", txt))), 1L)
})

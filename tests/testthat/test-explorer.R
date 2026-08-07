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
  expect_false(grepl("</script><script>alert", html, fixed = TRUE))
  # jsonlite escapes "</" to "<\/" first, then .js_safe escapes the "<" itself
  expect_true(grepl("\\u003c\\/script>", html, fixed = TRUE))
  expect_true(grepl("\\u003cscript>alert(1)", html, fixed = TRUE))
})

test_that("a comment opener in the data cannot swallow the rest of the document", {
  # "<!--" inside script data flips the HTML tokenizer into script-data-escaped
  # state; a following "<script" escalates it, and from there the template's own
  # </script> stops closing the element and the page renders blank. jsonlite
  # escapes "</" but not this, which is why .js_safe() escapes every "<".
  cfg <- mk_expl_cfg()
  rec <- normalize_record(new_record(source = "pubmed", source_id = "1",
    title = "Trial <!-- <script> of doom"))
  p <- render_explorer(cfg, records = list(rec))
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<!--", html, fixed = TRUE))
  expect_true(grepl("\\u003c!-- \\u003cscript>", html, fixed = TRUE))
  # exactly one script element still closes where the template says it does
  expect_true(endsWith(trimws(html), "</html>"))
})

test_that("a record containing a template token cannot eat the real one", {
  # __DATA__ is injected first; a single-pass assemble is what stops the value
  # it injects from being rescanned when __META__ goes in.
  cfg <- mk_expl_cfg()
  rec <- normalize_record(new_record(source = "pubmed", source_id = "1",
                                     title = "Sneaky __META__ __DATA__ title"))
  p <- render_explorer(cfg, records = list(rec), generated = "2026-01-01")
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Sneaky __META__ __DATA__ title", html, fixed = TRUE))
  expect_true(grepl('"generated":"2026-01-01"', html, fixed = TRUE))   # real token still filled
})

test_that(".assemble fails loudly rather than half-rendering", {
  expect_error(.assemble("no tokens here", list("__DATA__" = "x")), "missing token")
  expect_error(.assemble("__DATA__ and __EXTRA__", list("__DATA__" = "x")), "unfilled token")
  # replaces EVERY occurrence, not just the first
  expect_equal(.assemble("__DATA__/__DATA__", list("__DATA__" = "v")), "v/v")
})

test_that("the written file is LF on every platform", {
  # Text-mode output emits CRLF on Windows and LF on the ubuntu runner; git then
  # stores a whole new ~230 KB blob each time the generating platform alternates.
  cfg <- mk_expl_cfg()
  p <- render_explorer(cfg, records = list(normalize_record(
    new_record(source = "pubmed", source_id = "1", title = "T"))))
  raw <- readBin(p, "raw", file.size(p))
  expect_false(any(raw == as.raw(13)))
})

test_that("the geography fields reach the page (the country facet reads them)", {
  cfg <- mk_expl_cfg()
  rec <- derive_fields(normalize_record(new_record(
    source = "pubmed", source_id = "1", title = "T",
    place = "Burkina Faso (Niangoloko, Gourcy)")))
  p <- render_explorer(cfg, records = list(rec))
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # place stays verbatim for display; countries is what the facet is built from
  expect_true(grepl('"place":"Burkina Faso (Niangoloko, Gourcy)"', html, fixed = TRUE))
  expect_true(grepl('"countries":"Burkina Faso"', html, fixed = TRUE))
  expect_true(grepl('"region":""', html, fixed = TRUE))
})

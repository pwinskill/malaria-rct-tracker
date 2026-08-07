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

test_that("burden denominators are read, and a missing file is not an error", {
  cfg <- mk_expl_cfg()
  ensure_dir(cfg$output$data_dir)
  expect_equal(.read_burden(cfg)$n, 0L)          # absent file -> empty, no error

  writeLines(c("# a comment line", "country,cases,year,source",
               "Nigeria,66800000,2022,WMR", "Uganda,12700000,2022,WMR",
               "Badrow,,2022,WMR", ",5,2022,WMR", "Zeroland,0,2022,WMR"),
             file.path(cfg$output$data_dir, "burden.csv"))
  b <- .read_burden(cfg)
  # rows with no country, no number, or a non-positive count are dropped rather
  # than becoming a zero denominator (which would divide to Infinity)
  expect_equal(b$n, 2L)
  expect_equal(b$cases[["Nigeria"]], 66800000)
  expect_equal(b$source, "WMR")
})

test_that("a malformed burden file degrades to no burden view", {
  cfg <- mk_expl_cfg()
  ensure_dir(cfg$output$data_dir)
  writeLines(c("not,a,burden,table", "1,2,3,4"),
             file.path(cfg$output$data_dir, "burden.csv"))
  expect_equal(.read_burden(cfg)$n, 0L)
  # and rendering still succeeds
  expect_true(file.exists(render_explorer(cfg, records = list())))
})

test_that("every CSV field is surfaced somewhere in the explorer", {
  # The page embeds all 39 fields; the table shows 8 and the drawer the rest. This
  # is the guard that adding a schema column doesn't silently leave it unreachable
  # (payload paid for, never displayed) - add it to COLS or FIELD_GROUPS.
  tmpl <- paste(readLines(.explorer_template(), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  missing <- CSV_FIELDS[!vapply(CSV_FIELDS,
    function(f) grepl(paste0('"', f, '"'), tmpl, fixed = TRUE), logical(1))]
  expect_equal(unname(missing), character(0))
})

test_that("the interactive scaffolding is present in the rendered page", {
  cfg <- mk_expl_cfg()
  p <- render_explorer(cfg, records = list(derive_fields(normalize_record(
    new_record(source = "pubmed", source_id = "1", title = "T", place = "Kenya")))))
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('<dialog id="drawer"', html, fixed = TRUE))   # detail drawer
  expect_true(grepl('id="covGrid"', html, fixed = TRUE))          # coverage panel
  expect_true(grepl('id="expBib"', html, fixed = TRUE))           # exports
  expect_true(grepl('id="expRis"', html, fixed = TRUE))
  expect_true(grepl('id="matrix"', html, fixed = TRUE))           # gap matrix
  expect_true(grepl('id="chartGeo"', html, fixed = TRUE))         # geography
  expect_true(grepl('id="chartLag"', html, fixed = TRUE))         # start-to-publication
  expect_true(grepl('id="density"', html, fixed = TRUE))          # activity timeline
})

test_that("the activity timeline has the dates it needs", {
  # The "running" view spans trial_start..trial_completion. Both must survive the
  # trip to the page, or every row silently falls back to the published view.
  cfg <- mk_expl_cfg()
  rec <- derive_fields(normalize_record(new_record(
    source = "pubmed", source_id = "1", title = "T", place = "Kenya",
    trial_start = "2015-03-01", trial_completion = "2018-06-30",
    publication_date = "2020-01-15")))
  p <- render_explorer(cfg, records = list(rec))
  html <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('"trial_start":"2015-03-01"', html, fixed = TRUE))
  expect_true(grepl('"trial_completion":"2018-06-30"', html, fixed = TRUE))
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

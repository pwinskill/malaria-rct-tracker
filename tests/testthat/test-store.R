# The abstract must never reach disk. It is publisher copyright: fine to reason
# over in memory, not ours to redistribute in a public repo. These tests guard
# the boundary at every write path, because a single leak is permanent once it
# is in git history.

mk_store_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-store-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
                     brief_filename = "B.md", state_file = "state.json",
                     trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
                     screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt"))
}

SECRET <- "UNIQUE-ABSTRACT-TEXT-THAT-MUST-NOT-BE-WRITTEN"

mk_rec <- function(id = "1")
  normalize_record(new_record(source = "pubmed", source_id = id,
                              title = "A randomized malaria vaccine trial",
                              url = paste0("https://pubmed.ncbi.nlm.nih.gov/", id, "/"),
                              abstract = SECRET))

test_that("append_records writes no abstract to JSONL or CSV", {
  cfg <- mk_store_cfg()
  append_records(cfg, list(mk_rec()))

  jsonl <- readLines(file.path(cfg$output$data_dir, "trials.jsonl"), warn = FALSE)
  csv   <- readLines(file.path(cfg$output$data_dir, "trials.csv"),   warn = FALSE)
  expect_false(any(grepl(SECRET, jsonl, fixed = TRUE)))
  expect_false(any(grepl(SECRET, csv,   fixed = TRUE)))
  expect_false(any(grepl('"abstract"', jsonl, fixed = TRUE)))
})

test_that("the abstract survives in memory even though it is never stored", {
  # The point of the field is screening and extraction; dropping it at the
  # write boundary must not break the in-memory record.
  r <- mk_rec()
  expect_equal(r$abstract, SECRET)
  expect_true(grepl(SECRET, record_context(r), fixed = TRUE))
})

test_that(".rewrite_store writes no abstract either", {
  cfg <- mk_store_cfg()
  append_records(cfg, list(mk_rec()))
  .rewrite_store(cfg, list(mk_rec()))

  jsonl <- readLines(file.path(cfg$output$data_dir, "trials.jsonl"), warn = FALSE)
  expect_false(any(grepl(SECRET, jsonl, fixed = TRUE)))
})

test_that("append_excluded writes no abstract", {
  cfg <- mk_store_cfg()
  append_excluded(cfg, list(mk_rec()))

  out <- readLines(file.path(cfg$output$data_dir, "screened_out.jsonl"), warn = FALSE)
  expect_false(any(grepl(SECRET, out, fixed = TRUE)))
})

test_that("a stored record still resolves to its source", {
  # Dropping the abstract is only defensible because the route back survives it.
  cfg <- mk_store_cfg()
  append_records(cfg, list(mk_rec()))
  stored <- .read_store(cfg)[[1]]

  expect_true(nzchar(stored$url))
  expect_true(nzchar(stored$id))
})

test_that("a record with no url is still resolvable from its id prefix", {
  # The explorer builds outbound links from the id prefix, so a missing url is
  # survivable - but an id with neither is a dead end, and a reader would have
  # no way to check the extraction against anything.
  cfg <- mk_store_cfg()
  r <- mk_rec(); r$url <- ""
  append_records(cfg, list(r))
  stored <- .read_store(cfg)[[1]]

  expect_match(stored$id, "^(doi|pmid|nct):")
})

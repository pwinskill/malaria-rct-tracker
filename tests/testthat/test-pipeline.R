# Tests for the checkpointed, resumable pipeline. All offline: fetchers and the
# LLM are mocked, so no network/API calls are made.

mk_pipe_cfg <- function(checkpoint_every = 50) {
  d <- file.path(tempdir(), paste0("mrct-pipe-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(
    checkpoint_every = checkpoint_every,
    sources = list(pubmed = list(enabled = TRUE), clinicaltrials = list(enabled = TRUE),
                   europepmc = list(enabled = TRUE), ictrp = list(enabled = FALSE)),
    screening  = list(mode = "off", model = "x", api_key_env = "ANTHROPIC_API_KEY",
                      max_items_per_run = 5000),
    extraction = list(mode = "off", model = "x", api_key_env = "ANTHROPIC_API_KEY",
                      max_items_per_run = 2000),
    output = list(data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
                  brief_filename = "B.md", state_file = "state.json",
                  trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
                  screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt",
                  explorer_dir = file.path(d, "docs"), explorer_filename = "index.html")
  )
}

pipe_cands <- function(n) lapply(seq_len(n), function(i)
  normalize_record(new_record(source = "pubmed", source_id = as.character(i),
                              title = paste("Malaria RCT", i))))

test_that(".checkpoint stores included records and remembers all decided ids", {
  cfg <- mk_pipe_cfg()
  inc <- merge_within_batch(list(normalize_record(new_record(source = "pubmed", source_id = "1", title = "A"))))
  exc <- merge_within_batch(list(normalize_record(new_record(source = "pubmed", source_id = "2", title = "B"))))
  st  <- .checkpoint(cfg, load_state(cfg), inc, exc)
  expect_equal(length(.read_store(cfg)), 1L)         # only included go to the store
  expect_true("pmid:1" %in% st$seen_ids)
  expect_true("pmid:2" %in% st$seen_ids)             # excluded is still remembered (won't re-screen)
  expect_true("pmid:1" %in% st$reported)
  expect_false("pmid:2" %in% st$reported)            # excluded is not "reported"
})

test_that("a crashed run keeps completed chunks and resumes with no lost or double work", {
  cfg <- mk_pipe_cfg(checkpoint_every = 2)
  cands <- pipe_cands(5)
  testthat::local_mocked_bindings(
    load_dotenv          = function(...) invisible(FALSE),
    fetch_pubmed         = function(cfg, s, e) cands,
    fetch_clinicaltrials = function(cfg, s, e) list(),
    fetch_europepmc      = function(cfg, s, e) list()
  )

  # run 1: blow up while extracting the 2nd chunk
  calls <- 0L
  testthat::local_mocked_bindings(enrich = function(records, cfg, max_items = NULL) {
    calls <<- calls + 1L
    if (calls == 2L) stop("boom")
    lapply(records, function(r) { r$extracted_by <- "off"; r })
  })
  expect_error(run_pipeline(cfg, "2020-01-01", "2020-12-31", "backfill"), "boom")
  expect_equal(length(.read_store(cfg)), 2L)         # first chunk survived the crash
  expect_length(load_state(cfg)$reported, 2L)

  # run 2: resume, no crash
  testthat::local_mocked_bindings(enrich = function(records, cfg, max_items = NULL)
    lapply(records, function(r) { r$extracted_by <- "off"; r }))
  run_pipeline(cfg, "2020-01-01", "2020-12-31", "backfill")
  ids <- vapply(.read_store(cfg), function(r) r$id, character(1))
  expect_equal(length(ids), 5L)                      # all five recovered
  expect_equal(anyDuplicated(ids), 0L)               # nothing added twice

  # the brief self-heals: records persisted by the crashed run 1 (and skipped by
  # split_new on run 2) still appear, because the brief is rebuilt from the store
  brief <- paste(readLines(file.path(cfg$output$outputs_dir, cfg$output$brief_filename),
                           warn = FALSE), collapse = "\n")
  for (i in 1:5) expect_true(grepl(paste("Malaria RCT", i), brief, fixed = TRUE))
})

test_that("the screening cap bounds LLM ATTEMPTS across chunks, not just successes", {
  cfg <- mk_pipe_cfg(checkpoint_every = 2)
  cfg$screening$mode <- "llm"; cfg$screening$max_items_per_run <- 2
  Sys.setenv(ANTHROPIC_API_KEY = "test-key")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"), add = TRUE)
  cands <- pipe_cands(5)
  calls <- 0L
  testthat::local_mocked_bindings(
    load_dotenv          = function(...) invisible(FALSE),
    fetch_pubmed         = function(cfg, s, e) cands,
    fetch_clinicaltrials = function(cfg, s, e) list(),
    fetch_europepmc      = function(cfg, s, e) list(),
    .screen_llm = function(rec, key, model, system_prompt) {
      calls <<- calls + 1L; rec$screening_decision <- "include"; rec$screening_reason <- "ok"; rec
    }
  )
  run_pipeline(cfg, "2020-01-01", "2020-12-31", "backfill")
  expect_equal(calls, 2L)                    # never attempts more than the cap
  expect_equal(length(.read_store(cfg)), 2L) # the other 3 deferred, retry next run
})

test_that("records already in the store are not re-screened even if state.json lagged", {
  cfg <- mk_pipe_cfg(); cfg$screening$mode <- "llm"
  Sys.setenv(ANTHROPIC_API_KEY = "test-key")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"), add = TRUE)
  rec <- normalize_record(new_record(source = "pubmed", source_id = "1", title = "Already stored"))
  append_records(cfg, list(rec))             # in the store, but state.json never saved
  calls <- 0L
  testthat::local_mocked_bindings(
    load_dotenv          = function(...) invisible(FALSE),
    fetch_pubmed         = function(cfg, s, e) list(rec),
    fetch_clinicaltrials = function(cfg, s, e) list(),
    fetch_europepmc      = function(cfg, s, e) list(),
    .screen_llm = function(rec, key, model, system_prompt) {
      calls <<- calls + 1L; rec$screening_decision <- "include"; rec
    }
  )
  run_pipeline(cfg, "2020-01-01", "2020-12-31", "backfill")
  expect_equal(calls, 0L)                     # store-fold made it seen; no double-charge
})

test_that("an LLM screening failure defers the record (retried next run, not stored)", {
  cfg <- mk_pipe_cfg(); cfg$screening$mode <- "llm"
  Sys.setenv(ANTHROPIC_API_KEY = "test-key")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"), add = TRUE)
  cands <- pipe_cands(3)
  testthat::local_mocked_bindings(
    load_dotenv          = function(...) invisible(FALSE),
    fetch_pubmed         = function(cfg, s, e) cands,
    fetch_clinicaltrials = function(cfg, s, e) list(),
    fetch_europepmc      = function(cfg, s, e) list(),
    .screen_llm = function(rec, key, model, system_prompt) {
      if (rec$source_id == "2") stop("timeout")      # record 2 fails
      rec$screening_decision <- "include"; rec$screening_reason <- "ok"; rec
    }
  )
  run_pipeline(cfg, "2020-01-01", "2020-12-31", "backfill")

  stored <- vapply(.read_store(cfg), function(r) r$source_id, character(1))
  expect_setequal(stored, c("1", "3"))               # record 2 deferred, not stored
  expect_false("pmid:2" %in% load_state(cfg)$seen_ids)  # so it will be retried next run
})

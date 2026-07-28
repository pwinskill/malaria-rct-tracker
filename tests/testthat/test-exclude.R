mk_excl_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-excl-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
                     brief_filename = "B.md", state_file = "state.json",
                     trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
                     screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt"))
}

test_that("exclude_records blocklists an id and purges it from the dataset", {
  cfg <- mk_excl_cfg()
  recs <- list(
    normalize_record(new_record(source = "pubmed", source_id = "1", title = "Keep me")),
    normalize_record(new_record(source = "pubmed", source_id = "2", title = "Remove me"))
  )
  append_records(cfg, recs)
  bad <- recs[[2]]$id

  n <- exclude_records(bad, reason = "not malaria", cfg = cfg)
  expect_equal(n, 1L)
  expect_true(bad %in% load_exclusions(cfg))          # blocklist persisted

  left <- jsonlite::stream_in(file(.jsonl_path(cfg)), verbose = FALSE)
  expect_equal(nrow(left), 1L)                         # purged from the store
  expect_equal(left$id, recs[[1]]$id)
})

test_that("load_exclusions strips comments and blank lines", {
  cfg <- mk_excl_cfg(); ensure_dir(cfg$output$data_dir)
  writeLines(c("# a comment", "", "pmid:9  # rejected", "doi:10.1/x"), .exclusions_path(cfg))
  expect_setequal(load_exclusions(cfg), c("pmid:9", "doi:10.1/x"))
})

test_that("a blocklisted key filters a candidate out (matches by identity key)", {
  cfg <- mk_excl_cfg(); ensure_dir(cfg$output$data_dir)
  r <- normalize_record(new_record(source = "pubmed", source_id = "7", title = "x"))
  writeLines(r$id, .exclusions_path(cfg))
  excl <- load_exclusions(cfg)
  merged <- merge_within_batch(list(r))
  kept <- Filter(function(x) !any(c(x$id, x$.all_keys %||% character(0)) %in% excl), merged)
  expect_length(kept, 0)
})

test_that("reconcile_dataset removes duplicate rows and rebuilds state", {
  cfg <- mk_excl_cfg()
  r <- normalize_record(new_record(source = "pubmed", source_id = "5", title = "Dup"))
  append_records(cfg, list(r))
  append_records(cfg, list(r))                       # same record twice = a merge duplicate
  expect_equal(length(readLines(.jsonl_path(cfg))), 2L)

  n <- reconcile_dataset(cfg)
  expect_equal(n, 1L)
  expect_equal(length(readLines(.jsonl_path(cfg))), 1L)   # deduped
  st <- load_state(cfg)
  expect_true(r$id %in% st$seen_ids)                 # state rebuilt from the store
  expect_true(r$id %in% st$reported)
})

test_that("reclassify_store re-derives intervention_class from the current vocabulary", {
  cfg <- mk_excl_cfg()
  cfg$output$explorer_dir <- file.path(cfg$output$data_dir, "docs")   # keep off the real docs/
  # stored with a stale free-text label, but the text implies SMC
  r <- normalize_record(new_record(source = "pubmed", source_id = "1",
         title = "Seasonal malaria chemoprevention trial in children",
         intervention_class = "Chemoprophylaxis"))
  append_records(cfg, list(r))
  expect_equal(.read_store(cfg)[[1]]$intervention_class, "Chemoprophylaxis")   # stale

  n <- reclassify_store(cfg)
  expect_equal(n, 1L)
  expect_equal(.read_store(cfg)[[1]]$intervention_class, "SMC")                # re-derived
})

# Tests for crash-safe state.json (atomic write + tolerant, self-healing load).

mk_state_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-state-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
                     brief_filename = "B.md", state_file = "state.json",
                     trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
                     screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt"))
}

test_that("save_state writes atomically and keeps the previous good copy as .bak", {
  cfg <- mk_state_cfg()
  save_state(cfg, list(reported = "a", seen_ids = "a"))
  save_state(cfg, list(reported = c("a", "b"), seen_ids = c("a", "b")))
  expect_true(file.exists(paste0(.state_path(cfg), ".bak")))
  expect_no_error(jsonlite::fromJSON(paste0(.state_path(cfg), ".bak")))   # .bak is valid
  expect_setequal(load_state(cfg)$seen_ids, c("a", "b"))                   # primary still current
})

test_that("a corrupt state.json is recovered from the .bak copy (no throw)", {
  cfg <- mk_state_cfg()
  save_state(cfg, list(reported = "a", seen_ids = "a"))          # .bak will hold this
  save_state(cfg, list(reported = c("a", "b"), seen_ids = c("a", "b")))
  writeLines("{ not valid json", .state_path(cfg))               # torn primary write
  st <- load_state(cfg)
  expect_setequal(st$seen_ids, c("a"))                           # fell back to .bak
})

test_that("if state.json (and .bak) are unreadable, dedupe memory rebuilds from the store", {
  cfg <- mk_state_cfg()
  append_records(cfg, list(normalize_record(new_record(source = "pubmed", source_id = "5", title = "x"))))
  writeLines("garbage", .state_path(cfg))                        # corrupt; no .bak exists
  st <- suppressWarnings(load_state(cfg))
  expect_true("pmid:5" %in% st$seen_ids)                         # recovered from trials.jsonl
})

test_that("a genuinely fresh project (no state, no store) loads empty state", {
  cfg <- mk_state_cfg()
  st <- load_state(cfg)
  expect_equal(st$seen_ids, character(0))
  expect_equal(st$reported, character(0))
})

test_that("save_state round-trips large id vectors without unboxing to a scalar", {
  cfg <- mk_state_cfg()
  ids <- paste0("pmid:", 1:200)
  save_state(cfg, list(reported = ids, seen_ids = ids))
  st <- load_state(cfg)
  expect_equal(length(st$seen_ids), 200L)
})

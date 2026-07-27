# Regression tests for the parallel-review fixes.

mk_fix_cfg <- function() {
  d <- file.path(tempdir(), paste0("mrct-fix-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
  list(output = list(data_dir = file.path(d, "data"), outputs_dir = file.path(d, "outputs"),
                     brief_filename = "B.md", state_file = "state.json",
                     trials_csv = "trials.csv", trials_jsonl = "trials.jsonl",
                     screened_out_file = "screened_out.jsonl", exclusions_file = "exclusions.txt"))
}

# --- C1: CT.gov reference PMIDs -------------------------------------------
test_that("only RESULT/DERIVED reference PMIDs become identity keys, not BACKGROUND", {
  study <- list(protocolSection = list(
    identificationModule = list(nctId = "NCT12345", briefTitle = "SMC cluster RCT in Mali"),
    referencesModule = list(references = list(
      list(pmid = "111", type = "BACKGROUND"),
      list(pmid = "222", type = "RESULT"),
      list(pmid = "333", type = "DERIVED")
    ))
  ))
  rec <- .parse_ct_study(study)
  expect_setequal(rec$.pmids, c("222", "333"))
  expect_false("111" %in% rec$.pmids)
})

test_that("an unrelated BACKGROUND citation no longer merges a foreign paper into the trial", {
  study <- list(protocolSection = list(
    identificationModule = list(nctId = "NCT100", briefTitle = "SMC cluster RCT in Mali"),
    referencesModule = list(references = list(list(pmid = "111", type = "BACKGROUND")))
  ))
  trial <- normalize_record(.parse_ct_study(study))
  paper <- normalize_record(new_record(source = "pubmed", source_id = "111",
             title = "Unrelated review of bednet economics",
             abstract = "A background paper, not this trial."))
  merged <- merge_within_batch(list(trial, paper))
  expect_length(merged, 2)   # they must stay separate
})

# --- C-M1: enrollment count formatting ------------------------------------
test_that("large enrollment counts are not stored in scientific notation", {
  study <- list(protocolSection = list(
    identificationModule = list(nctId = "NCT9", briefTitle = "Big vector-control trial"),
    designModule = list(enrollmentInfo = list(count = 100000))
  ))
  rec <- .parse_ct_study(study)
  expect_equal(rec$n_total, "100000")
})

# --- D-M1: off-mode keeps everything --------------------------------------
test_that("screening mode 'off' keeps even a protocol-titled record", {
  cfg <- list(screening = list(mode = "off"))
  rec <- normalize_record(new_record(source = "pubmed", source_id = "1",
           title = "Study protocol for a two-arm cluster-randomised controlled trial"))
  out <- screen_records(list(rec), cfg)
  expect_equal(out[[1]]$screening_decision, "include")
})

# --- D-M2: exclude_early_phase is a functional toggle ---------------------
test_that("exclude_early_phase changes both the prompt and the rule decision", {
  expect_true(grepl("NOT early-phase", .screen_system(FALSE, TRUE), fixed = TRUE))
  expect_true(grepl("Any phase is acceptable", .screen_system(FALSE, FALSE), fixed = TRUE))

  rec <- normalize_record(new_record(source = "pubmed", source_id = "9",
           title = "A phase 1 randomized placebo-controlled trial of a malaria vaccine",
           abstract = "Adults were randomly assigned to a malaria vaccine or placebo."))
  expect_equal(.screen_rules(rec, FALSE, TRUE)$screening_decision,  "exclude")  # early excluded
  expect_equal(.screen_rules(rec, FALSE, FALSE)$screening_decision, "include")  # early allowed
})

# --- D-M3: extraction parse-failure isn't stamped as an LLM success -------
test_that(".extract_apply on NULL leaves no bogus llm provenance", {
  rec <- new_record(source = "pubmed", source_id = "1", title = "x")
  out <- .extract_apply(rec, NULL, "claude-sonnet-5")
  expect_identical(out$extracted_by, "")
})

# --- C-m1: degenerate empty-title records do not collapse -----------------
test_that("two id-less, title-less records stay distinct", {
  r1 <- normalize_record(new_record(source = "", source_id = "", title = ""))
  r2 <- normalize_record(new_record(source = "", source_id = "", title = ""))
  expect_length(merge_within_batch(list(r1, r2)), 2)
})

# --- C-m2: month parsing is case-insensitive ------------------------------
test_that("parse_pubmed_date handles month case and numeric months", {
  expect_equal(parse_pubmed_date("2024", "MAR", "5"),   "2024-03-05")
  expect_equal(parse_pubmed_date("2024", "march", ""),  "2024-03-01")
  expect_equal(parse_pubmed_date("2024", "3", "1"),     "2024-03-01")
  expect_equal(parse_pubmed_date("2024", "Jan", "15"),  "2024-01-15")
  expect_equal(parse_pubmed_date("", "Jan", "1"),       "")
})

# --- C-m4: reconcile merges duplicates rather than dropping the richer row -
test_that("reconcile_dataset merges duplicate rows field-wise", {
  cfg <- mk_fix_cfg()
  a <- normalize_record(new_record(source = "pubmed", source_id = "5", title = "Dup", place = "Kenya"))
  b <- normalize_record(new_record(source = "pubmed", source_id = "5", title = "Dup", phase = "Phase 3"))
  append_records(cfg, list(a)); append_records(cfg, list(b))
  reconcile_dataset(cfg)
  kept <- .read_store(cfg)
  expect_length(kept, 1)
  expect_equal(kept[[1]]$place, "Kenya")     # kept from row a
  expect_equal(kept[[1]]$phase, "Phase 3")   # filled from row b
})

# --- C-m5: an id containing '#' is not truncated by comment stripping -----
test_that("load_exclusions keeps ids containing '#' but strips real comments", {
  cfg <- mk_fix_cfg(); ensure_dir(cfg$output$data_dir)
  writeLines(c("doi:10.1/x#frag", "pmid:9  # rejected", "# whole-line comment"),
             .exclusions_path(cfg))
  expect_setequal(load_exclusions(cfg), c("doi:10.1/x#frag", "pmid:9"))
})

# --- D-N2: .env quoting, export prefix, and ambient precedence ------------
test_that("load_dotenv strips quotes/export and overrides ambient env", {
  tmp <- tempfile(fileext = ".env")
  writeLines(c('ANTHROPIC_API_KEY="sk-quoted"', "export FOO=bar", "BAZ='q'"), tmp)
  Sys.setenv(ANTHROPIC_API_KEY = "ambient-should-lose")
  on.exit(Sys.unsetenv(c("ANTHROPIC_API_KEY", "FOO", "BAZ")), add = TRUE)
  load_dotenv(tmp)
  expect_equal(Sys.getenv("ANTHROPIC_API_KEY"), "sk-quoted")  # quotes stripped, .env wins
  expect_equal(Sys.getenv("FOO"), "bar")                       # 'export ' stripped
  expect_equal(Sys.getenv("BAZ"), "q")
})

# --- Coverage gap: llm.R JSON plumbing ------------------------------------
test_that(".parse_json_object is tolerant and llm_str coerces scalars", {
  expect_equal(.parse_json_object('{"a":1}')$a, 1)
  expect_equal(.parse_json_object('noise {"a":2} more')$a, 2)  # embedded block
  expect_null(.parse_json_object("no json here"))
  expect_null(.parse_json_object(""))
  expect_equal(llm_str(list(" x ")), "x")
  expect_equal(llm_str(NULL), "")
  expect_equal(llm_str(c("a", "b")), "a")
})

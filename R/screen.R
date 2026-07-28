# Eligibility screening - the precision stage.
#
# Every deduped candidate gets an include/exclude decision with a recorded
# reason, mimicking systematic-review title/abstract screening. This removes
# non-trials (reviews, modelling papers, commentaries), trials that only mention
# malaria in passing, AND - by default - records with no reported results yet
# (study protocols, bare registrations, ongoing/recruiting trials).
#
# Scope is controlled by config$screening$include_ongoing:
#   FALSE (default) - results-only: exclude protocols and no-result registrations
#   TRUE            - also keep the registry pipeline (ongoing/registered trials),
#                     but still exclude published study-protocol papers
#
# Modes (config$screening$mode): "llm" (Anthropic API) or "off" (keep all).
# With mode "llm" but NO key, a conservative rule-based screen is used instead.
# With a key present, a transient LLM failure (timeout, unparseable response)
# marks the record "deferred" - it is not stored and retries on the next run,
# rather than being silently downgraded to a lower-quality rules decision.

.SCREEN_SCHEMA <- list(
  type = "object",
  properties = list(
    eligible          = list(type = "boolean"),
    record_type       = list(type = "string",
                             enum = c("trial", "preprint", "registration",
                                      "protocol", "review", "other")),
    phase             = list(type = "string"),
    intervention_class = list(type = "string"),
    reason            = list(type = "string"),
    confidence        = list(type = "string", enum = c("low", "medium", "high"))
  ),
  required = c("eligible", "record_type", "phase", "reason", "confidence"),
  additionalProperties = FALSE
)

.screen_system <- function(include_ongoing = FALSE, exclude_early = TRUE) {
  incl <- if (include_ongoing)
    "- It is a randomized controlled trial with reported results, OR a registered / ongoing malaria RCT (participants randomly allocated; cluster-randomized counts).\n"
  else
    "- It is a randomized controlled trial (participants randomly allocated to compared groups; cluster-randomized counts) that HAS REPORTED OUTCOME RESULTS / findings.\n"
  excl <- if (include_ongoing)
    "- It is a published STUDY PROTOCOL paper (describes a planned trial's methods, no results yet) - even if it describes a valid RCT.\n"
  else
    paste0("- NO RESULTS YET: a study protocol, a bare trial registration, or a planned / ongoing / ",
           "recruiting trial that has not reported outcome results. A 'Study protocol for...' paper, ",
           "or one stating results are pending/ongoing, is EXCLUDED even though it describes a valid RCT.\n")
  # Phase policy is configurable (screening$exclude_early_phase). When off, all
  # phases including Phase I / first-in-human are eligible.
  incl_phase <- if (exclude_early)
    paste0("- It is NOT early-phase. Exclude Phase I, first-in-human, dose-escalation / safety-only. ",
           "Phase II, III, IV, 'not applicable' (vector-control/cluster trials) and unstated phase are fine.\n\n")
  else
    "- Any phase is acceptable, including Phase I / first-in-human.\n\n"
  excl_phase <- if (exclude_early) "- Early-phase, as above.\n\n" else ""
  paste0(
    "You screen biomedical records for a malaria randomized controlled trial (RCT) results ",
    "registry, applying systematic-review eligibility rules. Decide if the record is ELIGIBLE.\n\n",
    "INCLUDE (eligible=true) only if ALL hold:\n",
    incl,
    "- Its PRIMARY purpose is a malaria intervention in humans: prevention, treatment, a vaccine, ",
    "vector control (bed nets/ITNs, IRS, spatial repellents, larviciding), chemoprevention ",
    "(SMC/IPTp/IPTi), diagnosis, or an elimination strategy.\n",
    incl_phase,
    "EXCLUDE (eligible=false) if ANY hold:\n",
    excl,
    "- Not an RCT: observational/cohort/case-control, modelling or simulation study, systematic ",
    "review or meta-analysis, narrative review, commentary/editorial, methods paper.\n",
    "- Malaria is incidental, not the trial's focus (e.g. a nutrition, iron, HIV, TB, or dengue ",
    "trial that merely mentions malaria or measures it only as a minor secondary outcome).\n",
    excl_phase,
    "Return: eligible (bool); record_type (trial | preprint | registration | protocol | review | other); ",
    "phase ('Phase 3' / 'Not applicable' / '' if unstated); intervention_class (short category, or ''); ",
    "reason (one concise sentence naming the decisive factor); confidence (low|medium|high)."
  )
}

# Screen a list of records; sets screening_* / phase / record_type in place.
screen_records <- function(records, cfg, max_items = NULL) {
  mode <- cfg$screening$mode %||% "off"
  if (!length(records)) return(records)
  include_ongoing <- isTRUE(cfg$screening$include_ongoing)
  exclude_early   <- isTRUE(cfg$screening$exclude_early_phase %||% TRUE)

  # "off" means keep every candidate untouched - no LLM, no structural backstop.
  if (identical(mode, "off"))
    return(lapply(records, function(r) { r$screening_decision <- "include"; r }))

  key <- llm_key(cfg$screening)
  if (!nzchar(key)) {
    message("[screen] no API key found; using conservative rule-based screening")
    return(lapply(records, function(r)
      .apply_scope(.screen_rules(r, include_ongoing, exclude_early), include_ongoing)))
  }

  cap <- max_items %||% cfg$screening$max_items_per_run %||% 5000
  model <- cfg$screening$model %||% "claude-haiku-4-5"
  system_prompt <- .screen_system(include_ongoing, exclude_early)
  inc <- 0L
  for (i in seq_along(records)) {
    if (i > cap) { records[[i]]$screening_decision <- "deferred"; next }
    records[[i]] <- tryCatch(
      .screen_llm(records[[i]], key, model, system_prompt),
      error = function(e) {
        message(sprintf("[screen] LLM error for %s: %s; deferring (retries next run)",
                        records[[i]]$id, conditionMessage(e)))
        .screen_defer(records[[i]], sprintf("LLM error: %s", conditionMessage(e)))
      })
    records[[i]] <- .apply_scope(records[[i]], include_ongoing)
    if (identical(records[[i]]$screening_decision, "include")) inc <- inc + 1L
  }
  message(sprintf("[screen] %d of %d candidate(s) eligible", inc, length(records)))
  records
}

# Deterministic scope backstop: enforce the structural no-result exclusions
# regardless of what the LLM said, so a haiku slip can't admit a protocol or an
# ongoing registration. Only flips include -> exclude, never the reverse.
.PROTOCOL_TITLE <- "study protocol|protocol for (a|an|the)|trial protocol|: a protocol\\b|\\bprotocol:"

.apply_scope <- function(rec, include_ongoing) {
  if (!identical(rec$screening_decision, "include")) return(rec)
  rt <- rec$record_type %||% ""
  status <- tolower(rec$status %||% "")
  title <- tolower(rec$title %||% "")

  # published study-protocol papers are out in BOTH scopes
  is_protocol <- rt == "protocol" || grepl(.PROTOCOL_TITLE, title, perl = TRUE)
  # a registration with no posted results is "no results yet"
  is_ongoing_reg <- rt == "registration" && !grepl("result", status)

  reason <- if (is_protocol) "study protocol / no results yet"
            else if (!include_ongoing && is_ongoing_reg) "registration without posted results"
            else NA_character_
  if (!is.na(reason)) {
    rec$screening_decision <- "exclude"
    rec$screening_reason <- if (has_text(rec$screening_reason))
      sprintf("%s (%s)", reason, rec$screening_reason) else reason
  }
  rec
}

# Apply a parsed screen result to a record. Returns NULL if `data` is missing,
# signalling the caller to fall back. Split out from the network call for testing.
.screen_apply <- function(rec, data) {
  if (is.null(data)) return(NULL)
  rec$screening_decision   <- if (isTRUE(data$eligible)) "include" else "exclude"
  rec$screening_reason     <- llm_str(data$reason)
  rec$screening_confidence <- llm_str(data$confidence)
  if (!has_text(rec$phase))              rec$phase <- llm_str(data$phase)
  if (!has_text(rec$intervention_class)) rec$intervention_class <- llm_str(data$intervention_class)
  # keep a structural record_type from the fetcher; only refine a blank/generic one
  rt <- llm_str(data$record_type)
  if (nzchar(rt) && (rec$record_type %||% "") %in% c("", "trial")) rec$record_type <- rt
  rec
}

# Mark a record for retry on a future run: not stored, not added to seen_ids.
.screen_defer <- function(rec, reason) {
  rec$screening_decision <- "deferred"
  rec$screening_reason   <- reason
  rec$screening_confidence <- "low"
  rec
}

.screen_llm <- function(rec, key, model, system_prompt) {
  data <- llm_json(key, model, system_prompt, record_context(rec),
                   .SCREEN_SCHEMA, max_tokens = 400L)
  applied <- .screen_apply(rec, data)
  # unparseable/truncated response: defer and retry next run rather than commit
  # a rules decision the user didn't ask for.
  if (is.null(applied)) .screen_defer(rec, "screen: unparseable LLM response") else applied
}

# --------------------------------------------------------- rule-based fallback
# Conservative: keep only a clear malaria RCT with a randomization signal that
# is not early-phase (when exclude_early) and not a non-trial. This enforces the
# STRUCTURAL scope only; the results-only semantics ("no results yet / results
# pending") are enforced separately by `.apply_scope`, which the caller layers
# on top. `include_ongoing` is accepted for signature symmetry with the LLM path.
.RULE_MAL  <- paste0("malaria|plasmodium|falciparum|vivax|antimalarial|artemisinin|",
                     "\\bitn\\b|\\bllin\\b|bed ?net|\\birs\\b|indoor residual|chemoprevention|",
                     "\\bsmc\\b|\\biptp\\b|\\bipti\\b|sporozoite|rts,?s|\\br21\\b|pfspz")
.RULE_RCT  <- "random(i[sz]ed|ly (assigned|allocated))|placebo-controlled|controlled trial"
.RULE_NOT  <- paste0("\\b(review|meta-analysis|systematic review|modelling|modeling|",
                     "simulation study|commentary|editorial|cohort study|case-control|",
                     "cross-sectional|observational study)\\b")
.RULE_EARLY <- "phase (1|i)\\b|first-in-human|dose-escalation|dose escalation"

.screen_rules <- function(rec, include_ongoing = FALSE, exclude_early = TRUE) {
  ctx <- tolower(record_context(rec))
  # early-phase judged from the trial's OWN phase/design/title, not an incidental
  # mention of a prior phase-1 study in the abstract.
  phase_txt <- tolower(paste(rec$title %||% "", rec$phase %||% "", rec$design %||% ""))
  bad_type <- (rec$record_type %||% "") %in% c("review", "other")
  early_ok <- !exclude_early || !grepl(.RULE_EARLY, phase_txt, perl = TRUE)
  ok <- !bad_type &&
        grepl(.RULE_MAL, ctx, perl = TRUE) &&
        grepl(.RULE_RCT, ctx, perl = TRUE) &&
        early_ok &&
        !grepl(.RULE_NOT, tolower(rec$title %||% ""), perl = TRUE)
  rec$screening_decision   <- if (ok) "include" else "exclude"
  rec$screening_reason     <- "rule-based screen (no LLM)"
  rec$screening_confidence <- "low"
  rec
}

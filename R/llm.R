# Shared Anthropic Messages API client.
#
# One JSON-in/JSON-out call used by both the eligibility screen and the
# extractor. Uses structured outputs (output_config.format) so the model is
# constrained to the schema; falls back to tolerant JSON parsing if a response
# somehow isn't clean JSON. No SDK dependency - a plain httr2 POST.

.ANTHROPIC_URL <- "https://api.anthropic.com/v1/messages"

# Return the ANTHROPIC key (or "") given a config block that names the env var.
llm_key <- function(cfg_block) {
  Sys.getenv(cfg_block$api_key_env %||% "ANTHROPIC_API_KEY", "")
}

# POST one prompt, constrained to `schema`; returns a parsed named list or NULL.
# `thinking` (e.g. list(type = "disabled")) is sent verbatim when supplied - used
# to disable adaptive thinking on models where it is on by default, so the whole
# max_tokens budget is available for the JSON rather than being eaten by thinking.
llm_json <- function(key, model, system_prompt, user_prompt, schema,
                     max_tokens = 800L, thinking = NULL) {
  body <- list(
    model = model,
    max_tokens = max_tokens,
    system = system_prompt,
    messages = list(list(role = "user", content = user_prompt)),
    output_config = list(format = list(type = "json_schema", schema = schema))
  )
  if (!is.null(thinking)) body$thinking <- thinking
  req <- request(.ANTHROPIC_URL)
  req <- req_user_agent(req, USER_AGENT)
  req <- req_headers(req,
                     `x-api-key` = key,
                     `anthropic-version` = "2023-06-01",
                     `content-type` = "application/json")
  req <- req_body_json(req, body, auto_unbox = TRUE)
  req <- req_timeout(req, 60)
  req <- req_retry(req, max_tries = 3, is_transient = .http_transient)
  resp <- req_perform(req)
  js <- resp_body_json(resp, simplifyVector = FALSE)

  # refusals / non-text stops -> nothing usable
  text <- paste(vapply(js$content %||% list(), function(b) {
    if (identical(b$type, "text")) b$text %||% "" else ""
  }, character(1)), collapse = "")
  out <- .parse_json_object(text)
  if (is.null(out)) {
    # Make silent failures (truncation, refusal) observable rather than a quiet
    # downgrade to rules.
    message(sprintf("[llm] no usable JSON from %s (stop_reason=%s)",
                    model, js$stop_reason %||% "?"))
  }
  out
}

# Tolerant parse: try whole string, else grab the first {...} block.
.parse_json_object <- function(text) {
  if (!nzchar(trimws(text %||% ""))) return(NULL)
  out <- tryCatch(fromJSON(text, simplifyVector = TRUE), error = function(e) NULL)
  if (is.list(out)) return(out)
  m <- regmatches(text, regexpr("\\{.*\\}", text, perl = TRUE))
  if (length(m)) tryCatch(fromJSON(m, simplifyVector = TRUE), error = function(e) NULL) else NULL
}

# Coerce a possibly-list/NULL schema value to a trimmed scalar string.
llm_str <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  trimws(as.character(x)[1])
}

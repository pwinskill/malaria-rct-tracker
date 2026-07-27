# Small, polite HTTP helper built on httr2, with retries and backoff.
#
# httr2's req_retry() honours Retry-After on 429 automatically; we extend the
# transient set to cover 500/502/503/504 as well.

USER_AGENT <- "malaria-rct-tracker (research; contact via config.yaml)"

.http_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

.http_request <- function(url, query = list(), headers = list(), timeout = 60) {
  req <- request(url)
  req <- req_user_agent(req, USER_AGENT)
  req <- req_timeout(req, timeout)
  req <- req_retry(req, max_tries = 4, is_transient = .http_transient)
  if (length(query)) req <- do.call(req_url_query, c(list(req), query))
  if (length(headers)) req <- do.call(req_headers, c(list(req), headers))
  req
}

# GET returning parsed JSON (nested lists; no vector simplification, so nested
# navigation with `g()` / `[[` stays predictable).
http_get_json <- function(url, query = list(), headers = list()) {
  resp <- req_perform(.http_request(url, query, headers))
  resp_body_json(resp, simplifyVector = FALSE)
}

# GET returning the raw response body as a string (used for PubMed XML).
http_get_text <- function(url, query = list(), headers = list()) {
  resp <- req_perform(.http_request(url, query, headers))
  resp_body_string(resp)
}

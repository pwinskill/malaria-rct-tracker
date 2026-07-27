# Merge duplicate hits across sources/identifiers and drop already-seen items.

# When the same trial appears from multiple sources, prefer richer fields in
# this source order (later sources fill gaps only).
.SOURCE_PRIORITY <- list(clinicaltrials = 0L, pubmed = 1L, europepmc = 2L, ictrp = 3L)

# Fill empty fields of `primary` from `other` (never overwrites).
merge_records <- function(primary, other) {
  for (k in names(other)) {
    pv <- primary[[k]]
    ov <- other[[k]]
    empty <- is.null(pv) || length(pv) == 0 || (is.character(pv) && !nzchar(pv[1]))
    if (empty && !(is.null(ov) || length(ov) == 0)) primary[[k]] <- ov
  }
  primary
}

# Collapse records that share ANY identity key (DOI/NCT/PMID) using union-find.
# Records with no strong id fall back to their canonical id, so title-only
# duplicates still merge. Each output record gains a `.all_keys` field holding
# every identity key in its group, used for cross-run dedupe in split_new().
merge_within_batch <- function(records) {
  n <- length(records)
  if (n == 0) return(records)

  parent <- seq_len(n)
  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  union <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra != rb) parent[rb] <<- ra
  }

  keys_per <- vector("list", n)
  key_to_idx <- new.env(parent = emptyenv())
  for (i in seq_len(n)) {
    ks <- id_keys(records[[i]])
    if (!length(ks)) {
      # No strong id: fall back to the canonical id. But a record with an empty
      # title yields the degenerate key "title:", which would union all such
      # records into one - give those a unique per-record key instead.
      cid <- canonical_id(records[[i]])
      ks <- if (identical(cid, "title:")) paste0(".self:", i) else cid
    }
    keys_per[[i]] <- ks
    for (k in ks) {
      if (!is.null(key_to_idx[[k]])) union(key_to_idx[[k]], i) else key_to_idx[[k]] <- i
    }
  }

  roots <- vapply(seq_len(n), find, integer(1))
  out <- list()
  for (r in unique(roots)) {
    idxs <- which(roots == r)
    prio <- vapply(idxs, function(i) .SOURCE_PRIORITY[[records[[i]]$source]] %||% 9L, integer(1))
    idxs <- idxs[order(prio)]
    base <- records[[idxs[1]]]
    for (j in idxs[-1]) base <- merge_records(base, records[[j]])
    base$id <- canonical_id(base)
    base$.all_keys <- unique(unlist(keys_per[idxs]))
    out[[length(out) + 1L]] <- base
  }
  out
}

# Keep only records whose identity is entirely unseen. A record is a duplicate
# of something already in the dataset if ANY of its keys is in state$seen_ids.
split_new <- function(records, state) {
  seen <- unique(as.character(state$seen_ids %||% character(0)))
  if (!length(records)) return(records)
  keep <- vapply(records, function(r) {
    ks <- r$.all_keys %||% r$id
    !any(ks %in% seen)
  }, logical(1))
  records[keep]
}

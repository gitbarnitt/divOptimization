# R/normalizer.R
suggest_mapping <- function(cols, synonyms) {
  canon <- names(synonyms)
  lc <- tolower(gsub("[^a-z0-9]", "", cols))
  out <- setNames(rep(NA_character_, length(canon)), canon)
  for (c in canon) {
    # exact match
    hit <- which(lc == tolower(gsub("[^a-z0-9]", "", c)))
    if (length(hit)) { out[c] <- cols[hit[1]]; next }
    # synonym match
    for (syn in synonyms[[c]]) {
      hit <- which(lc == tolower(gsub("[^a-z0-9]", "", syn)))
      if (length(hit)) { out[c] <- cols[hit[1]]; break }
    }
  }
  out
}

normalize_species_summary <- function(df) {
  map <- suggest_mapping(names(df), SCHEMA_SYNONYMS)
  # rename found fields → canonical
  for (canon in names(map)) {
    from <- map[[canon]]
    if (!is.na(from) && from != canon) {
      # Only rename if the synonym exists and canonical doesn't
      if (from %in% names(df) && !(canon %in% names(df))) {
        df <- dplyr::rename(df, !!canon := !!rlang::sym(from))
      }
    }
  }
  # derive / coerce
  if (!"year_pair" %in% names(df) && all(c("year_baseline","year_changed") %in% names(df))) {
    df <- dplyr::mutate(df,
                        year_pair = sprintf("%d_%d", as.integer(.data$year_baseline), as.integer(.data$year_changed))
    )
  }
  if (!"sample_size" %in% names(df)) {
    df <- dplyr::mutate(df,
                        sample_size = dplyr::coalesce(.data$sample_size,
                                                      .data$actual_sample_size,
                                                      .data$request_sample_size,
                                                      .data$requested_sample_size)
    )
  }
  if ("mode" %in% names(df)) {
    df <- dplyr::mutate(df, mode = dplyr::recode(.data$mode, !!!MODE_SYNONYMS, .default = .data$mode))
  }
  # type coercions (integerish to int; bounds)
  col_names <- names(df)
  df <- df |>
    dplyr::mutate(
      year_baseline = if ("year_baseline" %in% col_names) as.integer(.data$year_baseline) else .data$year_baseline,
      year_changed  = if ("year_changed"  %in% col_names) as.integer(.data$year_changed)  else .data$year_changed,
      sample_size   = if ("sample_size"   %in% col_names) as.integer(.data$sample_size)   else .data$sample_size,
      replicate     = if ("replicate"     %in% col_names) as.integer(.data$replicate)     else .data$replicate
    )
  attr(df, "normalizer_log") <- tibble::tibble(
    canonical = names(map),
    from = unname(map),
    action = dplyr::if_else(is.na(unname(map)), "missing", "mapped/ok")
  )
  df
}

normalize_draws_index <- function(df) {
  df <- normalize_species_summary(df)
  if ("detected" %in% names(df) && is.numeric(df$detected)) {
    df$detected <- df$detected > 0
  }
  if (!"draw" %in% names(df)) df$draw <- 1L
  df
}

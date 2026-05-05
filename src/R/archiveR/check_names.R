# R/check_names.R
assert_fn <- function(name) {
  if (!exists(name, mode = "function")) stop("Missing function: ", name)
}

# ---- helpers that are safe to run inside a target (no tar_read) ----
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(readr); library(targets); library(tibble)
})

# Build/write registry from the manifest only (safe in target)
.build_pipeline_registry <- function() {
  man <- targets::tar_manifest()
  tibble(
    target    = man$name,
    command   = man$command,
    origin_fn = stringr::str_match(man$command, "^\\s*([A-Za-z0-9_.]+)\\s*\\(")[,2],
    mode      = dplyr::case_when(
      grepl("baseline", man$name) ~ "baseline",
      grepl("variable", man$name) ~ "variable",
      grepl("^sensitivity_results$", man$name) ~ "capped",
      TRUE ~ "other"
    )
  )
}
.write_pipeline_registry <- function(path = "meta/pipeline_registry.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  reg <- .build_pipeline_registry()
  readr::write_csv(reg, path)
  invisible(reg)
}

# Light column harmonizer - SIMPLIFIED to just handle year_pair and CI column cleanup
# All other names should be canonical from the source functions
.normalize_det <- function(df, mode = c("variable","baseline","capped")) {
  mode <- match.arg(mode)
  if (!is.data.frame(df) || !nrow(df)) return(df)
  
  # Derive year_pair if missing
  if (!"year_pair" %in% names(df) && all(c("year_baseline","year_changed") %in% names(df))) {
    df <- df %>% mutate(year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
  }
  
  # Clean up CI columns from joins (these .x/.y suffixes happen with join operations)
  if ("ci_lower.y" %in% names(df) && !"ci_lower" %in% names(df)) {
    df <- dplyr::rename(df, ci_lower = ci_lower.y)
  }
  if ("ci_upper.y" %in% names(df) && !"ci_upper" %in% names(df)) {
    df <- dplyr::rename(df, ci_upper = ci_upper.y)
  }
  
  df
}

# Minimal canonical columns we care about
.expected_cols <- list(
  full_detection_summary          = c("site","species","year_pair","requested_sample_size","mean_detection","ci_lower","ci_upper"),
  full_detection_summary_baseline = c("site","species","year_pair","actual_sample_size","mean_detection","ci_lower","ci_upper"),
  community_detection             = c("site","year_pair","requested_sample_size","cwm_mean","ci_lower","ci_upper"),
  community_detection_baseline    = c("site","year_pair","actual_sample_size","cwm_mean","ci_lower","ci_upper")
)

# Validate columns for objects PASSED IN (no tar_read)
.validate_key_outputs_inmem <- function(
    full_detection_summary           = NULL,
    community_detection              = NULL,
    full_detection_summary_baseline  = NULL,
    community_detection_baseline     = NULL
) {
  problems <- character()
  
  if (!is.null(full_detection_summary)) {
    x <- .normalize_det(full_detection_summary, "variable")
    need <- .expected_cols$full_detection_summary
    if (!all(need %in% names(x))) problems <- c(problems, "full_detection_summary columns not canonical after normalization.")
  }
  
  if (!is.null(full_detection_summary_baseline)) {
    x <- .normalize_det(full_detection_summary_baseline, "baseline")
    need <- .expected_cols$full_detection_summary_baseline
    if (!all(need %in% names(x))) problems <- c(problems, "full_detection_summary_baseline columns not canonical after normalization.")
  }
  
  if (!is.null(community_detection)) {
    x <- community_detection
    # Derive year_pair if needed
    if (!"year_pair" %in% names(x) && all(c("year_baseline","year_changed") %in% names(x))) {
      x <- x %>% mutate(year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
    }
    # Just check the columns exist - don't rename
    need <- .expected_cols$community_detection
    if (!all(need %in% names(x))) problems <- c(problems, "community_detection columns not canonical.")
  }
  
  if (!is.null(community_detection_baseline)) {
    x <- community_detection_baseline
    # Derive year_pair if needed
    if (!"year_pair" %in% names(x) && all(c("year_baseline","year_changed") %in% names(x))) {
      x <- x %>% mutate(year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
    }
    # Just check the columns exist - don't rename
    need <- .expected_cols$community_detection_baseline
    if (!all(need %in% names(x))) problems <- c(problems, "community_detection_baseline columns not canonical.")
  }
  
  if (length(problems)) stop(paste(problems, collapse = "\n"))
  invisible(TRUE)
}

# ---- replace your check with a version that avoids tar_read inside target ----
check_pipeline_symbols <- function(
    full_detection_summary           = NULL,
    community_detection              = NULL,
    full_detection_summary_baseline  = NULL,
    community_detection_baseline     = NULL
) {
  # 0) original function-existence assertions
  fns <- c(
    "run_sample_size_sensitivity",
    "run_sample_size_sensitivity_variable",
    "run_baseline_full_available",
    "read_draws_index",
    "summarize_species_detection_with_uncertainty",
    "evaluate_community_weighted_detection"
  )
  invisible(lapply(fns, assert_fn))
  
  # 1) write/refresh registry (manifest-only; safe)
  try(.write_pipeline_registry("meta/pipeline_registry.csv"), silent = TRUE)
  
  # 2) validate columns for any objects passed in
  .validate_key_outputs_inmem(
    full_detection_summary          = full_detection_summary,
    community_detection             = community_detection,
    full_detection_summary_baseline = full_detection_summary_baseline,
    community_detection_baseline    = community_detection_baseline
  )
  
  TRUE
}

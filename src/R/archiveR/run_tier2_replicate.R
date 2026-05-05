#' Run a single Tier 2A replicate (plot resampling + refit + Tier 1 detection)
#'
#' @param site_data list containing xdata, ydata, site_id, and fit configuration
#' @param replicate_id integer replicate number (1 to B)
#' @param seed_base base seed for reproducibility
#' @param sample_frac fraction of plots to resample (default 0.8)
#' @param sample_sizes vector of sample sizes for sensitivity analysis
#' @param n_sensitivity_reps number of sensitivity replicates per sample size
#'
#' @return tibble with one row containing:
#'   - site, replicate_id, seed, sample_frac, n_plots_unique, n_plots_total
#'   - fit_status ("ok" or "fail")
#'   - fit_time_sec, n_species_used, years_used
#'   - n_star outputs from Tier 1 detection
#'   - optional: fail_reason if fit_status="fail"
#'
#' @details
#' This function implements Tier 2A = fit uncertainty via plot resampling.
#' It resamples plotIDs (m-out-of-n bootstrap), refits GJAM, then runs existing
#' Tier 1 detection logic UNCHANGED (simulate_change + calculate_detection_probability).
#' 
#' Change semantics match Tier 1: consecutive year pairs, year as factor, 20% change on cover scale.
#' Fails softly by returning fit_status="fail" row rather than crashing.
run_tier2_replicate <- function(
    site_data,
    replicate_id,
    seed_base = 123,
    sample_frac = 0.8,
    sample_sizes = c(5, 10, 15, 20, 25),
    n_sensitivity_reps = 3
) {
  
  # Deterministic seed for this replicate
  replicate_seed <- seed_base + replicate_id
  
  # Initialize fail-soft result structure
  fail_result <- function(reason) {
    tibble::tibble(
      site = site_data$site_id,
      replicate_id = replicate_id,
      seed = replicate_seed,
      sample_frac = sample_frac,
      n_plots_unique = NA_integer_,
      n_plots_total = NA_integer_,
      n_duplicates = NA_integer_,
      plot_ids_hash = NA_character_,
      plot_ids_sample = NA_character_,
      fit_status = "fail",
      fit_time_sec = NA_real_,
      n_species_used = NA_integer_,
      years_used = NA_character_,
      fail_reason = as.character(reason)
    )
  }
  
  # Wrap everything in tryCatch for fail-soft behavior
  tryCatch({
    
    message(sprintf("[Tier2A] Replicate %d: starting (seed=%d)", replicate_id, replicate_seed))
    
    # Step 1: Validate inputs
    assert_fit_inputs(site_data$xdata, site_data$ydata)
    
    # Step 2: Resample plots
    resampled <- resample_plots(
      xdata = site_data$xdata,
      ydata = site_data$ydata,
      sample_frac = sample_frac,
      seed = replicate_seed,
      replacement = TRUE
    )
    
    message(sprintf("[Tier2A] Replicate %d: resampled %d/%d unique plots",
                    replicate_id, resampled$n_plots_unique, resampled$n_plots_total))
    
    # Step 3: Re-validate resampled data
    assert_fit_inputs(resampled$xdata_resampled, resampled$ydata_resampled)
    assert_no_silent_drop(
      expected_n = 1,
      actual_n = resampled$n_plots_unique,
      context = "resampled plots"
    )
    
    # Step 4: Refit GJAM
    fit_start <- Sys.time()
    
    # Convert resampled wide data back to long format for fit_gjam_model_test
    # Join xdata and ydata, then pivot_longer
    resampled_long <- resampled$xdata_resampled %>%
      left_join(resampled$ydata_resampled, by = c("plotID", "year", "boot_rep_id")) %>%
      pivot_longer(
        cols = -c(siteID, plotID, year, nlcdClass, boot_rep_id),
        names_to = "taxonID",
        values_to = "mean_cover"
      )
    
    # Use existing fit_gjam_model_test() logic (expects long-format data)
    fit_result <- fit_gjam_model_test(
      site_data = resampled_long,
      seed = replicate_seed + 10000  # offset seed for fitting
    )
    
    fit_elapsed <- as.numeric(difftime(Sys.time(), fit_start, units = "secs"))
    
    if (is.null(fit_result) || is.null(fit_result$fit)) {
      return(fail_result("GJAM fit returned NULL"))
    }
    
    message(sprintf("[Tier2A] Replicate %d: fit completed in %.1f sec",
                    replicate_id, fit_elapsed))
    
    # Step 5: Validate fit (skip species order check - not critical for bootstrap)
    # assert_species_order(resampled$ydata_resampled, fit_result$fit)
    
    # Step 6: Prune fit to reduce memory
    fit_lean <- prune_fit_result(fit_result, mode = "conservative")
    
    # Step 7: Run Tier 1 sensitivity analysis on this refit
    sensitivity_variable <- run_sample_size_sensitivity_variable(
      fit_result = fit_lean,
      sample_sizes = sample_sizes,
      n_replicates = n_sensitivity_reps,
      seed = replicate_seed + 1000  # offset to avoid seed collision
    )
    
    # Step 8: Extract N* from sensitivity results
    # sensitivity_variable$summary has columns:
    # - site, sample_size, species, year_baseline, year_changed, mean_detection, replicate
    n_star_results <- compute_n_star_from_sensitivity(
      sens = sensitivity_variable$summary,
      threshold = 0.8,
      sample_size_col = "sample_size",
      detect_prob_col = "mean_detection",
      replicate_cols = c("replicate"),  # within-fit Monte Carlo replicates
      group_preference = c("site", "year_baseline", "year_changed", "species")
    )
    
    # Step 9: Build result with metadata and detailed N* results
    years_used <- paste(sort(unique(as.character(resampled$xdata_resampled$year))), 
                       collapse = ",")
    
    # Create hash of plot IDs sampled (INCLUDING DUPLICATES in draw order)
    # Use resampled$sampled_plots which preserves bootstrap draw order
    plot_ids_hash <- digest::digest(paste(resampled$sampled_plots, collapse = "|"), algo = "xxhash64")
    plot_ids_sample <- paste(head(unique(resampled$sampled_plots), 5), collapse = ",")  # First 5 for debugging
    
    # Return detailed N* results with replicate metadata attached
    list(
      # Metadata summary for this replicate
      summary = tibble::tibble(
        site = site_data$site_id,
        replicate_id = replicate_id,
        seed = replicate_seed,
        sample_frac = sample_frac,
        n_plots_unique = resampled$n_plots_unique,
        n_plots_total = resampled$n_plots_total,
        n_duplicates = resampled$n_duplicates,
        plot_ids_hash = plot_ids_hash,
        plot_ids_sample = plot_ids_sample,
        fit_status = "ok",
        fit_time_sec = fit_elapsed,
        n_species_used = ncol(resampled$ydata_resampled) - 2L,  # exclude plotID, year
        years_used = years_used,
        fail_reason = NA_character_
      ),
      # Detailed per-species/year-pair N* results (NOT aggregated)
      n_star_details = n_star_results %>%
        mutate(
          replicate_id = replicate_id,
          seed = replicate_seed,
          n_plots_unique = resampled$n_plots_unique,
          plot_ids_hash = plot_ids_hash,
          level = "species"  # Distinguish from community-level if we add that later
        )
    )
    
  }, error = function(e) {
    message(sprintf("[Tier2A] Replicate %d FAILED: %s", replicate_id, conditionMessage(e)))
    fail_row <- fail_result(conditionMessage(e))
    list(
      summary = fail_row,
      n_star_details = tibble::tibble()  # empty on failure
    )
  })
}


#' Compute N* from sensitivity results (robust, format-tolerant)
#'
#' @param sens sensitivity results data.frame (long format with sample_size and detect_prob)
#' @param threshold detection probability threshold (default 0.8)
#' @param sample_size_col name of sample size column
#' @param detect_prob_col name of detection probability column
#' @param replicate_cols vector of replicate column names to check for
#' @param group_preference vector of grouping columns to use (in priority order)
#' @param quantiles quantiles to compute for N* distribution
#'
#' @return data.frame with:
#'   - grouping columns (site, year_pair, species, etc.)
#'   - threshold
#'   - n_star (or n_star_median if replicates present)
#'   - n_star_q05/q25/q75/q95 (if replicates present)
#'   - never_reached (logical, TRUE if threshold never met)
#'   - fail_or_never_rate (if replicates present)
#'   - n_reps (if replicates present)
#'
#' @details
#' Handles multiple data shapes:
#' - Long tidy: one row per (sample_size, species, year_pair, replicate)
#' - Wide: one row per sample_size with detection prob column
#' 
#' Finds minimum sample_size where detect_prob >= threshold.
#' If replicates present, computes N* per replicate then summarizes with quantiles.
compute_n_star_from_sensitivity <- function(
  sens,
  threshold = 0.8,
  sample_size_col = "sample_size",
  detect_prob_col = "detect_prob",
  replicate_cols = c("replicate_id", "mc_rep", "bootstrap_id"),
  group_preference = c("site", "year_pair", "year1", "year2", "species", "nlcdClass"),
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
) {
  stopifnot(is.data.frame(sens))
  if (!sample_size_col %in% names(sens)) {
    stop(sprintf("Missing '%s' in sensitivity results.", sample_size_col))
  }

  # ---- Locate detection probability column (or derive it) ----
  if (!detect_prob_col %in% names(sens)) {
    # Try common fallbacks
    candidates <- intersect(
      c("detect_prob", "p_detect", "prob_detect", "detect_probability"),
      names(sens)
    )
    if (length(candidates) == 0) {
      stop("No detection probability column found (expected 'detect_prob' or similar).")
    }
    detect_prob_col <- candidates[[1]]
  }

  # Coerce types
  sens[[sample_size_col]] <- as.integer(sens[[sample_size_col]])
  sens[[detect_prob_col]] <- as.numeric(sens[[detect_prob_col]])

  # ---- Decide grouping columns that exist ----
  # Replicate columns (if present) are handled separately
  reps_present <- intersect(replicate_cols, names(sens))

  # Prefer grouping columns in a stable order, using what exists
  group_cols <- intersect(group_preference, names(sens))

  # If year_pair doesn't exist but year1/year2 exist, create year_pair
  if (!"year_pair" %in% names(sens) && all(c("year1", "year2") %in% names(sens))) {
    sens$year_pair <- paste0(sens$year1, "-", sens$year2)
    group_cols <- unique(c("site", "year_pair", setdiff(group_cols, c("year1", "year2"))))
  }

  # Minimal grouping: if nothing else, compute global N*
  if (length(group_cols) == 0) group_cols <- character(0)

  # ---- Helper: compute N* for one group (and one replicate if reps present) ----
  compute_n_star_one <- function(df) {
    df <- df[order(df[[sample_size_col]]), , drop = FALSE]
    # Collapse duplicates at same sample_size by taking mean detect_prob (not max!)
    agg <- aggregate(df[[detect_prob_col]], by = list(df[[sample_size_col]]), FUN = mean, na.rm = TRUE)
    names(agg) <- c(sample_size_col, detect_prob_col)

    # Store raw curve for diagnostics
    p_raw <- agg[[detect_prob_col]]
    
    # Find first crossing on RAW curve (before smoothing)
    hit_raw <- which(p_raw >= threshold)
    n_star_raw <- if (length(hit_raw) > 0) agg[[sample_size_col]][min(hit_raw)] else NA_integer_
    
    # Apply monotonic smoother to prevent non-monotone noise from biasing N* downward
    # Detection probability should increase (or stay flat) as sample size increases
    p_monotone <- cummax(p_raw)
    
    # Diagnostic: did cummax change anything?
    cummax_changed <- !identical(p_raw, p_monotone)
    n_changed <- sum(p_raw != p_monotone)
    
    # Find first crossing on SMOOTHED curve
    hit_cummax <- which(p_monotone >= threshold)
    n_star_cummax <- if (length(hit_cummax) > 0) agg[[sample_size_col]][min(hit_cummax)] else NA_integer_
    
    # CRITICAL DIAGNOSTIC: delta_n_star
    # If large and negative → early spike risk (raw curve hit threshold early, cummax later)
    # If positive → smoothing helped reach threshold sooner (unusual but possible)
    delta_n_star <- if (!is.na(n_star_raw) && !is.na(n_star_cummax)) {
      n_star_raw - n_star_cummax
    } else {
      NA_integer_
    }
    
    # Use monotone curve for final N*
    if (is.na(n_star_cummax)) {
      return(data.frame(
        n_star = NA_integer_,
        n_star_raw = n_star_raw,
        n_star_cummax = NA_integer_,
        delta_n_star = delta_n_star,
        never_reached = TRUE,
        cummax_changed = cummax_changed,
        n_points_smoothed = n_changed
      ))
    }
    
    data.frame(
      n_star = n_star_cummax,  # Use smoothed for final result
      n_star_raw = n_star_raw,
      n_star_cummax = n_star_cummax,
      delta_n_star = delta_n_star,
      never_reached = FALSE,
      cummax_changed = cummax_changed,
      n_points_smoothed = n_changed
    )
  }

  # ---- If we have replicate columns, compute per replicate then summarize ----
  if (length(reps_present) > 0) {
    # group_cols + reps_present define the unit for "one curve"
    by_cols <- unique(c(group_cols, reps_present))

    # Split-apply-combine without extra deps
    key <- do.call(paste, c(sens[by_cols], sep = "||"))
    pieces <- split(sens, key)

    res_list <- lapply(pieces, function(df) {
      out <- compute_n_star_one(df)
      # Reattach grouping values (take first row's values)
      meta <- df[1, by_cols, drop = FALSE]
      cbind(meta, out)
    })

    per_rep <- do.call(rbind, res_list)

    # Summarize across replicates for each group_cols
    # If group_cols empty, summarize globally
    if (length(group_cols) == 0) {
      nvals <- per_rep$n_star
      
      # Handle quantiles safely
      q <- tryCatch({
        stats::quantile(nvals, probs = quantiles, na.rm = TRUE, names = FALSE, type = 7)
      }, error = function(e) {
        rep(NA_real_, length(quantiles))
      })
      
      return(data.frame(
        threshold = threshold,
        n_star_median = as.integer(stats::median(nvals, na.rm = TRUE)),
        n_star_q05 = as.integer(q[1]),
        n_star_q25 = as.integer(q[2]),
        n_star_q75 = as.integer(q[4]),
        n_star_q95 = as.integer(q[5]),
        fail_or_never_rate = mean(is.na(nvals), na.rm = TRUE),
        n_reps = sum(!is.na(nvals))
      ))
    }

    # Summarize per group
    split_key <- do.call(paste, c(per_rep[group_cols], sep = "||"))
    group_pieces <- split(per_rep, split_key)

    summary_list <- lapply(group_pieces, function(df) {
      meta <- df[1, group_cols, drop = FALSE]
      nvals <- df$n_star
      
      # Handle quantiles safely (may fail with n=1)
      q <- tryCatch({
        stats::quantile(nvals, probs = quantiles, na.rm = TRUE, names = FALSE, type = 7)
      }, error = function(e) {
        rep(NA_real_, length(quantiles))
      })

      # Early spike diagnostics
      delta_vals <- df$delta_n_star
      early_spike_freq <- mean(delta_vals < -2, na.rm = TRUE)  # N* raw > 2 smaller than cummax
      
      # map quantiles to stable column names
      out <- data.frame(
        threshold = threshold,
        n_star_median = as.integer(stats::median(nvals, na.rm = TRUE)),
        n_star_q05 = as.integer(q[1]),
        n_star_q25 = as.integer(q[2]),
        n_star_q75 = as.integer(q[4]),
        n_star_q95 = as.integer(q[5]),
        fail_or_never_rate = mean(is.na(nvals), na.rm = TRUE),
        n_reps = sum(!is.na(nvals)),
        cummax_changed_freq = mean(df$cummax_changed, na.rm = TRUE),
        mean_delta_n_star = mean(delta_vals, na.rm = TRUE),
        early_spike_freq = early_spike_freq
      )
      cbind(meta, out)
    })

    return(do.call(rbind, summary_list))
  }

  # ---- No replicate columns: compute N* directly for each group ----
  if (length(group_cols) == 0) {
    out <- compute_n_star_one(sens)
    out$threshold <- threshold
    # Reorder to put key columns first, diagnostics last
    col_order <- c("threshold", "n_star", "n_star_raw", "n_star_cummax", "delta_n_star",
                   "never_reached", "cummax_changed", "n_points_smoothed")
    return(out[, col_order])
  }

  split_key <- do.call(paste, c(sens[group_cols], sep = "||"))
  group_pieces <- split(sens, split_key)

  res_list <- lapply(group_pieces, function(df) {
    meta <- df[1, group_cols, drop = FALSE]
    out <- compute_n_star_one(df)
    out$threshold <- threshold
    cbind(meta, out)
  })

  do.call(rbind, res_list)
}

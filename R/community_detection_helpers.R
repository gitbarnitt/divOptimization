# R/community_detection_helpers.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

.require_cols <- function(df, cols, where = "data") {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(sprintf("Missing required columns in %s: %s",
                 where, paste(missing, collapse = ", ")), call. = FALSE)
  }
}

# 1) Average replicates per site × sample_size × year_pair
cwm_average_replicates <- function(community_df) {
  .require_cols(
    community_df,
    c("site", "sample_size", "replicate", "weighted_detection"),
    "community_df"
  )
  
  # Ensure year_pair exists (fallback if upstream ever omits it)
  if (!"year_pair" %in% names(community_df)) {
    .require_cols(community_df, c("year_baseline", "year_changed"), "community_df")
    community_df <- community_df %>%
      mutate(year_pair = paste0(year_baseline, "_", year_changed))
  }
  
  has_ci <- all(c("ci_lower", "ci_upper") %in% names(community_df))
  
  out <- community_df %>%
    mutate(
      ci_lower = if (has_ci) as.numeric(ci_lower) else NA_real_,
      ci_upper = if (has_ci) as.numeric(ci_upper) else NA_real_
    ) %>%
    group_by(site, sample_size, year_pair) %>%
    summarise(
      mean_detection = mean(weighted_detection, na.rm = TRUE),
      n_replicates   = dplyr::n_distinct(replicate),
      ci_lower_mean  = if (has_ci) mean(ci_lower, na.rm = TRUE) else NA_real_,
      ci_upper_mean  = if (has_ci) mean(ci_upper, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  
  if (!has_ci) out <- select(out, -ci_lower_mean, -ci_upper_mean)
  out
}

# 2) Plot all year_pairs (one panel per site)
cwm_plot_by_yearpair <- function(rep_avg_df, sites = NULL, threshold = 0.8) {
  .require_cols(rep_avg_df, c("site", "sample_size", "year_pair", "mean_detection"), "rep_avg_df")
  if (!is.null(sites)) rep_avg_df <- dplyr::filter(rep_avg_df, site %in% sites)
  
  has_ci <- all(c("ci_lower_mean", "ci_upper_mean") %in% names(rep_avg_df))
  
  p <- ggplot(rep_avg_df, aes(x = sample_size, y = mean_detection, color = year_pair, group = year_pair)) +
    { if (has_ci) geom_ribbon(aes(ymin = ci_lower_mean, ymax = ci_upper_mean, fill = year_pair),
                              alpha = 0.15, color = NA) } +
    geom_line(size = 0.8) +
    geom_point(size = 1.8) +
    geom_hline(yintercept = threshold, linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = sort(unique(rep_avg_df$sample_size))) +
    facet_wrap(~ site, scales = "free_x") +
    labs(
      title = "Community-weighted mean detection by year pair",
      x = "Sample size (plots)", y = "Detection probability"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  if (has_ci) p <- p + guides(color = guide_legend(title = "Year pair"),
                              fill  = guide_legend(title = "Year pair"))
  p
}

# 3) Summarise across year_pairs per site × sample_size
cwm_summarise_across_yearpairs <- function(rep_avg_df) {
  .require_cols(rep_avg_df, c("site", "sample_size", "year_pair", "mean_detection"), "rep_avg_df")
  
  rep_avg_df %>%
    group_by(site, sample_size) %>%
    summarise(
      detection_mean = mean(mean_detection, na.rm = TRUE),
      detection_sd   = sd(mean_detection, na.rm = TRUE),
      ci_lower       = quantile(mean_detection, 0.025, na.rm = TRUE),
      ci_upper       = quantile(mean_detection, 0.975, na.rm = TRUE),
      n_year_pairs   = dplyr::n_distinct(year_pair),
      .groups = "drop"
    )
}

# 4) Plot mean across year_pairs (one panel per site)
cwm_plot_mean_across_yearpairs <- function(site_summary_df, sites = NULL, threshold = 0.8) {
  .require_cols(site_summary_df, c("site", "sample_size", "detection_mean"), "site_summary_df")
  if (!is.null(sites)) site_summary_df <- dplyr::filter(site_summary_df, site %in% sites)
  
  has_ci <- all(c("ci_lower", "ci_upper") %in% names(site_summary_df))
  
  ggplot(site_summary_df, aes(x = sample_size, y = detection_mean, group = site)) +
    { if (has_ci) geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15) } +
    geom_line(size = 0.9) +
    geom_point(size = 2) +
    geom_hline(yintercept = threshold, linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = sort(unique(site_summary_df$sample_size))) +
    facet_wrap(~ site, scales = "free_x") +
    labs(
      title = "Community-weighted mean detection (mean across year pairs)",
      x = "Sample size (plots)", y = "Detection probability"
    ) +
    theme_minimal(base_size = 12)
}

# 5) Site-level target table (Rule-H style)
cwm_site_target_table <- function(rep_avg_df, threshold = 0.8, pct_yearpairs = 0.75) {
  .require_cols(rep_avg_df, c("site", "sample_size", "year_pair", "mean_detection"), "rep_avg_df")
  
  pass_table <- rep_avg_df %>%
    mutate(pass = mean_detection >= threshold) %>%
    group_by(site, sample_size) %>%
    summarise(
      prop_yearpairs_passing = mean(pass, na.rm = TRUE),
      mean_at_size           = mean(mean_detection, na.rm = TRUE),
      sd_at_size             = sd(mean_detection, na.rm = TRUE),
      .groups = "drop"
    )
  
  targets <- pass_table %>%
    arrange(site, sample_size) %>%
    group_by(site) %>%
    summarise(
      target_sample_size = {
        idx <- which(prop_yearpairs_passing >= pct_yearpairs)
        if (length(idx)) min(sample_size[idx]) else NA_real_
      },
      .groups = "drop"
    )
  
  targets %>%
    left_join(pass_table, by = c("site", "target_sample_size" = "sample_size")) %>%
    transmute(
      site,
      target_sample_size,
      passes_rule = !is.na(target_sample_size),
      prop_yearpairs_passing = ifelse(is.na(prop_yearpairs_passing), 0, prop_yearpairs_passing),
      mean_detection_at_target = mean_at_size,
      sd_detection_at_target   = sd_at_size
    )
}

# 6) Convenience wrapper
cwm_build_all_summaries <- function(community_detection_df, threshold = 0.8, pct_yearpairs = 0.75) {
  rep_avg  <- cwm_average_replicates(community_detection_df)
  site_avg <- cwm_summarise_across_yearpairs(rep_avg)
  site_tbl <- cwm_site_target_table(rep_avg, threshold = threshold, pct_yearpairs = pct_yearpairs)
  list(rep_avg = rep_avg, site_avg = site_avg, site_tbl = site_tbl)
}

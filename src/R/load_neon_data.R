


#' Load and Preprocess NEON Plant Diversity Data
#'
#' @description
#' Loads NEON plant percent cover data from RDS file and preprocesses it for
#' GJAM modeling. Handles subplot-to-plot aggregation, bout averaging, and
#' site filtering based on environment variables.
#'
#' Prior to aggregation, rare species are identified at the site level and
#' reclassified as "OTHER" following Clark et al. (2014, 2017). A species
#' is considered rare if it occurs in fewer than 3 plots at a site. The
#' reclassification happens at the subplot level so that OTHER cover is
#' aggregated on exactly the same scale as all other taxa.
#'
#' @param path Character path to RDS file containing raw NEON plant data
#' @param min_plots Integer. Minimum number of plots a species must occupy
#'   at a site to be retained as an individual taxon. Species below this
#'   threshold are aggregated into "OTHER". Default is 3.
#'
#' @return
#' Data frame with columns:
#'   - siteID: NEON site identifier
#'   - plotID: Plot identifier
#'   - year: Sampling year (integer)
#'   - taxonID: Species/taxon identifier (includes "OTHER" for aggregated rare species)
#'   - nlcdClass: NLCD land cover class
#'   - mean_cover: Mean percent cover across subplots and bouts
#'   - sd_cover: Standard deviation of cover
#'   - n_subplots: Total number of subplots sampled
#'   - n_bouts: Number of bouts sampled
#'
#' @details
#' **Site Filtering:**
#'
#' If SITE_ID environment variable is set, filters data to that site only.
#' This is used for multi-site orchestration where each site is processed
#' separately.
#'
#' **Quality Filtering:**
#'
#' - Retains only targetTaxaPresent == "Y"
#' - Excludes plots with samplingImpractical issues
#' - Fills missing percentCover with 0
#'
#' **Rare Species Filtering:**
#'
#' Species occurring in fewer than `min_plots` plots at a site are
#' reclassified as "OTHER" at the subplot level before any aggregation.
#' This follows Clark et al. (2014) who applied a minimum plot count for
#' species inclusion and aggregated rare species into an OTHER category.
#' The Clark et al. (2017) GJAM vignette notes that species with
#' insufficient occurrence data "will contribute little to the model fit,
#' while degrading performance."
#'
#' Reclassification at the subplot level ensures OTHER cover is computed
#' on the same scale as all other taxa (summed within subplot, then
#' averaged across subplots and bouts).
#'
#' **Subplot Aggregation:**
#'
#' Accounts for missing subplots by using expected subplot count:
#'   - Years <= 2018: 8 subplots expected
#'   - Years > 2018: 6 subplots expected
#'
#' Missing subplots are treated as zeros when computing mean and variance.
#'
#' **Bout Averaging:**
#'
#' Averages mean_cover and sd_cover across bouts within each plot-year-species
#' combination. Sums n_subplots across bouts.
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
load_neon_data <- function(path, min_plots = 3L) {
  
  # --------------------------------------------------------------------------
  # 1. Load Raw Data and Apply Site Filter (if specified)
  # --------------------------------------------------------------------------
  
  raw <- readRDS(path)
  
  # Filter to specific site if SITE_ID env var is set (for MULTISITE mode)
  site_filter <- Sys.getenv("SITE_ID", "")
  if (site_filter != "") {
    message("Filtering data to site: ", site_filter)
    if (!"siteID" %in% names(raw)) {
      stop("SITE_ID filter requested but data has no 'siteID' column")
    }
    raw <- raw %>% dplyr::filter(siteID == site_filter)
    if (nrow(raw) == 0) {
      stop("No data found for SITE_ID: ", site_filter)
    }
    message("  Retained ", nrow(raw), " rows for ", site_filter)
  }
  
  # --------------------------------------------------------------------------
  # 2. Apply Quality Filters and Define Expected Subplot Counts
  # --------------------------------------------------------------------------
  
  filtered <- raw %>%
    dplyr::select(siteID, plotID, year, boutNumber, taxonID, nlcdClass,
                  subplotID, percentCover, samplingImpractical, targetTaxaPresent) %>%
    dplyr::filter(targetTaxaPresent == "Y") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(is.na(samplingImpractical) | samplingImpractical == "OK") %>%
    dplyr::mutate(
      n_subplots_expected = dplyr::if_else(year <= 2018L, 8L, 6L),
      percentCover = tidyr::replace_na(percentCover, 0)
    )
  
  # --------------------------------------------------------------------------
  # 2b. Rare Species Filtering — Reclassify as "OTHER" (Clark et al. 2014)
  # --------------------------------------------------------------------------
  #
  # Determine rarity at the site level: how many distinct plots does each
  # species occur in (with non-zero cover) across all years?
  # Species in fewer than min_plots are reclassified as "OTHER" at the
  # subplot level before any aggregation, so OTHER cover is on the same
  # scale as all individually tracked taxa.
  #
  # This step does NOT remove any data — it only relabels taxonID.
  # --------------------------------------------------------------------------
  
  # Identify species with non-zero cover in enough plots
  species_plot_counts <- filtered %>%
    dplyr::filter(percentCover > 0) %>%
    dplyr::group_by(siteID, taxonID) %>%
    dplyr::summarise(
      n_plots = dplyr::n_distinct(plotID),
      .groups = "drop"
    )
  
  # Flag species that meet the minimum plot threshold
  species_keep <- species_plot_counts %>%
    dplyr::filter(n_plots >= min_plots)
  
  # Log filtering summary per site
  filter_summary <- species_plot_counts %>%
    dplyr::group_by(siteID) %>%
    dplyr::summarise(
      total_species = dplyr::n(),
      retained       = sum(n_plots >= min_plots),
      to_other       = sum(n_plots < min_plots),
      .groups = "drop"
    )
  
  message("\n--- Rare species filtering (min_plots = ", min_plots, ") ---")
  for (i in seq_len(nrow(filter_summary))) {
    row <- filter_summary[i, ]
    message(sprintf("  %s: %d species retained, %d -> OTHER (of %d total)",
                    row$siteID, row$retained, row$to_other, row$total_species))
  }
  message("---\n")
  
  # Reclassify rare species as OTHER at the subplot level
  # Create a lookup of siteID + taxonID combinations to keep
  keep_lookup <- species_keep %>%
    dplyr::select(siteID, taxonID)
  
  filtered <- filtered %>%
    dplyr::left_join(
      keep_lookup %>% dplyr::mutate(.keep_flag = TRUE),
      by = c("siteID", "taxonID")
    ) %>%
    dplyr::mutate(
      taxonID = dplyr::if_else(is.na(.keep_flag), "OTHER", taxonID)
    ) %>%
    dplyr::select(-.keep_flag)
  
  # --------------------------------------------------------------------------
  # 3. Aggregate to Bout Level (Sum Cover Across Subplots)
  # --------------------------------------------------------------------------
  
  # Per-bout sums and sums of squares over observed subplots
  # Note: multiple rare species within the same subplot/bout are now all
  # labeled "OTHER" and will be summed together here — this is correct,
  # as it gives total rare species cover per subplot before averaging.
  bout_agg <- filtered %>%
    dplyr::group_by(siteID, plotID, year, boutNumber, taxonID, nlcdClass, n_subplots_expected) %>%
    dplyr::summarise(
      sum_cover   = sum(percentCover, na.rm = TRUE),
      sum_sq      = sum(percentCover * percentCover, na.rm = TRUE),
      n_obs       = dplyr::n(),                # observed subplots
      .groups     = "drop"
    )
  
  # --------------------------------------------------------------------------
  # 4. Compute Bout-Level Mean and SD (Accounting for Missing Subplots)
  # --------------------------------------------------------------------------
  
  # Mean/SD over expected subplots *including zeros for missing*
  # mean = sum / Nexp
  # var  = (sum_sq / Nexp) - mean^2   (zeros contribute 0 to sum_sq)
  per_bout_summary <- bout_agg %>%
    dplyr::mutate(
      n_subplots = n_subplots_expected,
      mean_cover = sum_cover / n_subplots_expected,
      var_cover  = (sum_sq / n_subplots_expected) - (mean_cover * mean_cover),
      sd_cover   = sqrt(pmax(var_cover, 0))
    ) %>%
    dplyr::select(siteID, plotID, year, boutNumber, taxonID, nlcdClass,
                  mean_cover, sd_cover, n_subplots)
  
  # --------------------------------------------------------------------------
  # 5. Average Across Bouts (Plot-Year-Taxon Level)
  # --------------------------------------------------------------------------
  
  # Average across bouts (your previous behavior)
  final_summary <- per_bout_summary %>%
    dplyr::group_by(siteID, plotID, year, taxonID, nlcdClass) %>%
    dplyr::summarise(
      mean_cover = mean(mean_cover, na.rm = TRUE),
      sd_cover   = mean(sd_cover,  na.rm = TRUE),
      n_subplots = sum(n_subplots, na.rm = TRUE),
      n_bouts    = dplyr::n_distinct(boutNumber),
      .groups    = "drop"
    )
  
  final_summary
}

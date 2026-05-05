# generate_figures.R
# Simple script to generate figures from parquet outputs
# Run this after targets::tar_make() completes successfully

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)
})

# Parameters
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", "outputs")
EXAMPLE_SITE <- Sys.getenv("EXAMPLE_SITE", "JERC")
TOP_N_SPECIES <- as.integer(Sys.getenv("TOP_N_SPECIES", "6"))
MIN_SAMPLE_SIZE <- as.integer(Sys.getenv("MIN_SAMPLE_SIZE", "5"))
THRESHOLD <- 0.8

# Create figures directory
fig_dir <- file.path(OUTPUT_DIR, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Read data
cat("Reading parquet files...\n")
species_summary <- read_parquet(file.path(OUTPUT_DIR, "species_summary.parquet"))
relative_cover <- read_parquet(file.path(OUTPUT_DIR, "relative_cover.parquet"))
draws_index <- read_parquet(file.path(OUTPUT_DIR, "draws_index.parquet"))

cat(sprintf("Loaded %d species summary rows\n", nrow(species_summary)))
cat(sprintf("Loaded %d relative cover rows\n", nrow(relative_cover)))

# Check column names
cat("\nSpecies summary columns:", paste(names(species_summary), collapse=", "), "\n")
cat("Relative cover columns:", paste(names(relative_cover), collapse=", "), "\n")

# Normalize relative_cover column names if needed
if ("siteID" %in% names(relative_cover) && !"site" %in% names(relative_cover)) {
  relative_cover <- rename(relative_cover, site = siteID)
}
if ("taxonID" %in% names(relative_cover) && !"species" %in% names(relative_cover)) {
  relative_cover <- rename(relative_cover, species = taxonID)
}

# Get available sites
sites <- sort(unique(species_summary$site))
cat(sprintf("\nAvailable sites: %s\n", paste(sites, collapse=", ")))

# Use first site if EXAMPLE_SITE not in data
if (!(EXAMPLE_SITE %in% sites)) {
  EXAMPLE_SITE <- sites[1]
  cat(sprintf("Using first site: %s\n", EXAMPLE_SITE))
}

# --- Figure 1: Species-level detection ---
cat("\n1. Generating species-level detection plot...\n")

# Get top species by relative cover
top_species <- relative_cover %>%
  filter(site == EXAMPLE_SITE) %>%
  group_by(species) %>%
  summarise(mean_rc = mean(relative_cover, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_rc)) %>%
  slice_head(n = TOP_N_SPECIES) %>%
  pull(species)

cat(sprintf("Top %d species: %s\n", TOP_N_SPECIES, paste(top_species, collapse=", ")))

var_sum <- species_summary %>%
  filter(
    site == EXAMPLE_SITE, 
    mode == "sensitivity_variable", 
    species %in% top_species, 
    sample_size >= MIN_SAMPLE_SIZE
  ) %>%
  group_by(species, year_pair, sample_size) %>%
  summarise(
    mean_det = mean(mean_detection, na.rm = TRUE),
    lo = if ("ci_lower" %in% names(cur_data_all())) mean(ci_lower, na.rm = TRUE) else NA_real_,
    hi = if ("ci_upper" %in% names(cur_data_all())) mean(ci_upper, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

p1 <- ggplot(var_sum, aes(sample_size, mean_det, color = species, group = species)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, alpha = 0.5) +
  geom_hline(yintercept = THRESHOLD, linetype = 2) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~ year_pair, scales = "free_x") +
  labs(
    x = "Sample size (plots)", 
    y = "Detection probability",
    title = sprintf("Species-level detection (%s)", EXAMPLE_SITE)
  ) +
  theme_bw()

ggsave(file.path(fig_dir, "01_species_level_detection.png"), p1, width = 10, height = 6, dpi = 300)
cat("  Saved: 01_species_level_detection.png\n")

# --- Figure 2: Community-weighted detection by year pair ---
cat("\n2. Generating CWM detection by year pair...\n")

rc_mean <- relative_cover %>%
  filter(site == EXAMPLE_SITE) %>%
  group_by(species) %>%
  summarise(w = mean(relative_cover, na.rm = TRUE), .groups = "drop")

rep_avg_var <- species_summary %>%
  filter(site == EXAMPLE_SITE, mode == "sensitivity_variable", sample_size >= MIN_SAMPLE_SIZE) %>%
  left_join(rc_mean, by = "species") %>%
  filter(!is.na(w)) %>%
  group_by(year_pair, sample_size, replicate) %>%
  summarise(mean_detection = sum(mean_detection * w) / sum(w), .groups = "drop") %>%
  group_by(year_pair, sample_size) %>%
  summarise(mean_detection = mean(mean_detection, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(rep_avg_var, aes(x = sample_size, y = mean_detection, color = year_pair, group = year_pair)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = THRESHOLD, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = sort(unique(rep_avg_var$sample_size))) +
  labs(
    x = "Sample size (plots)", 
    y = "CWM detection", 
    color = "Year pair",
    title = sprintf("Community-weighted detection (%s)", EXAMPLE_SITE)
  ) +
  theme_bw()

ggsave(file.path(fig_dir, "02_cwm_by_year_pair.png"), p2, width = 10, height = 6, dpi = 300)
cat("  Saved: 02_cwm_by_year_pair.png\n")

# --- Figure 3: Mean across year pairs ---
cat("\n3. Generating mean across year pairs...\n")

site_avg_var <- species_summary %>%
  filter(site == EXAMPLE_SITE, mode == "sensitivity_variable", sample_size >= MIN_SAMPLE_SIZE) %>%
  left_join(rc_mean, by = "species") %>%
  filter(!is.na(w)) %>%
  group_by(year_pair, sample_size, replicate) %>%
  summarise(cwm = sum(mean_detection * w) / sum(w), .groups = "drop") %>%
  group_by(sample_size) %>%
  summarise(detection_mean = mean(cwm, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(site_avg_var, aes(x = sample_size, y = detection_mean, group = 1)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = THRESHOLD, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = sort(unique(site_avg_var$sample_size))) +
  labs(
    x = "Sample size (plots)", 
    y = "CWM detection",
    title = sprintf("Mean across year pairs (%s)", EXAMPLE_SITE)
  ) +
  theme_bw()

ggsave(file.path(fig_dir, "03_mean_across_year_pairs.png"), p3, width = 8, height = 6, dpi = 300)
cat("  Saved: 03_mean_across_year_pairs.png\n")

# --- Figure 4: Decision metric across all sites ---
cat("\n4. Generating decision metric across all sites...\n")

pair_means <- species_summary %>%
  filter(mode == "sensitivity_variable") %>%
  group_by(site, year_pair, sample_size, replicate) %>%
  summarise(md = mean(mean_detection, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, year_pair, sample_size) %>%
  summarise(md = mean(md, na.rm = TRUE), .groups = "drop")

frac_tbl <- pair_means %>%
  mutate(hit = md >= THRESHOLD) %>%
  group_by(site, sample_size) %>%
  summarise(frac_pairs = mean(hit), .groups = "drop")

p4 <- ggplot(frac_tbl, aes(x = sample_size, y = frac_pairs, group = site)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.7, size = 1.8) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = sort(unique(frac_tbl$sample_size))) +
  labs(
    x = "Sample size (plots)", 
    y = "Fraction of year pairs ≥ 0.8",
    title = "Decision metric by site"
  ) +
  theme_bw()

ggsave(file.path(fig_dir, "04_decision_metric_all_sites.png"), p4, width = 10, height = 6, dpi = 300)
cat("  Saved: 04_decision_metric_all_sites.png\n")

# --- Create summary table ---
cat("\n5. Creating summary table...\n")

summary_table <- species_summary %>%
  filter(mode == "sensitivity_variable") %>%
  group_by(site, sample_size) %>%
  summarise(
    mean_detection = mean(mean_detection, na.rm = TRUE),
    n_species = n_distinct(species),
    n_year_pairs = n_distinct(year_pair),
    .groups = "drop"
  ) %>%
  arrange(site, sample_size)

readr::write_csv(summary_table, file.path(OUTPUT_DIR, "summary_table.csv"))
cat("  Saved: summary_table.csv\n")

cat("\n=== All figures generated successfully! ===\n")
cat(sprintf("Figures saved to: %s\n", fig_dir))
cat(sprintf("Summary table saved to: %s\n", file.path(OUTPUT_DIR, "summary_table.csv")))

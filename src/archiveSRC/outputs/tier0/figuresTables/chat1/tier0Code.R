# ============================================================
# Tier0: Figure + Table builder (year-pair contrasts)
# Writes to: outputs/tier0/figuresTables/
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(arrow)
  library(ggplot2)
  library(readr)
  library(forcats)
  library(glue)
  library(DiagrammeR)
})

tier0_dir <- "C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0"
out_dir   <- file.path(tier0_dir, "figuresTables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

POWER_TARGET <- 0.8

find_one <- function(dir, pattern_regex) {
  x <- list.files(dir, full.names = TRUE)
  hit <- x[str_detect(tolower(basename(x)), tolower(pattern_regex))]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

read_parquet_safe <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  arrow::read_parquet(path) %>% as_tibble()
}

infer_site <- function(site_dir, file_path) {
  nm <- basename(file_path)
  m <- str_match(nm, "([A-Z]{4})")
  if (!is.na(m[1,2])) return(m[1,2])
  basename(site_dir)
}

gg_save <- function(p, filename, w = 9, h = 6) {
  ggsave(filename = filename, plot = p, width = w, height = h, dpi = 300)
}

# ---- Discover sites ----
site_dirs <- list.dirs(tier0_dir, recursive = FALSE, full.names = TRUE)
site_dirs <- site_dirs[basename(site_dirs) != "figuresTables"]
message("Found site folders: ", length(site_dirs))

# ---- Read all sites ----
site_tables <- map(site_dirs, function(sd) {
  pc_path <- find_one(sd, "power_curve.*\\.parquet$")
  ns_path <- find_one(sd, "n_star_power.*\\.parquet$")
  
  pc <- read_parquet_safe(pc_path)
  ns <- read_parquet_safe(ns_path)
  
  if (!is.null(pc)) pc <- pc %>% mutate(site = infer_site(sd, pc_path))
  if (!is.null(ns)) ns <- ns %>% mutate(site = infer_site(sd, ns_path))
  
  list(site_dir = sd, power_curve = pc, n_star_power = ns)
})

power_curve_all <- site_tables %>% map("power_curve") %>% compact() %>% bind_rows()
n_star_all      <- site_tables %>% map("n_star_power") %>% compact() %>% bind_rows()

# Save combined tables
if (nrow(power_curve_all) > 0) {
  write_parquet(power_curve_all, file.path(out_dir, "tier0_power_curve_ALL_SITES.parquet"))
  write_csv(power_curve_all,    file.path(out_dir, "tier0_power_curve_ALL_SITES.csv"))
}
if (nrow(n_star_all) > 0) {
  write_parquet(n_star_all, file.path(out_dir, "tier0_n_star_power_ALL_SITES.parquet"))
  write_csv(n_star_all,    file.path(out_dir, "tier0_n_star_power_ALL_SITES.csv"))
}

message("Rows: power_curve_all = ", nrow(power_curve_all), " | n_star_all = ", nrow(n_star_all))

# ============================================================
# 1) Conceptual workflow diagram (Tier0-correct)
# ============================================================
library(ggplot2)
library(grid)

out_dir <- "C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0/figuresTables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

png_path <- file.path(out_dir, "tier0_workflow_yearpair_concept.png")

# ---- Box layout (manual, stable, readable) ----
boxes <- data.frame(
  id = LETTERS[1:8],
  label = c(
    "Inputs\nNEON plot–year cover data\n+ covariates",
    "Fit baseline GJAM once\n(per site)\nPosterior: B, Σ",
    "Posterior draws\n{B⁽ˢ⁾, Σ⁽ˢ⁾}",
    "Choose year-pair\n(t → t+1)",
    "Impose change\nbetween years",
    "Detect change\nPr(effect > threshold) > 0.8\n(binary_detect)",
    "Repeat across\nplot subsamples (K)\n+ replicates",
    "Aggregate outputs\npower_curve.parquet\nn_star_power.parquet"
  ),
  x = c(1, 2.5, 4, 4, 5.5, 7, 8.5, 10),
  y = c(2, 2, 2, 1, 1, 1.5, 1.5, 1.5)
)

# ---- Arrows between boxes ----
arrows <- data.frame(
  x    = c(1.8, 3.3, 4.0, 4.8, 6.3, 7.8, 5.5),
  y    = c(2.0, 2.0, 1.6, 1.0, 1.5, 1.5, 2.0),
  xend = c(2.3, 3.8, 7.0, 5.3, 7.3, 9.5, 7.0),
  yend = c(2.0, 2.0, 1.5, 1.0, 1.5, 1.5, 1.5)
)

p <- ggplot() +
  # boxes
  geom_rect(
    data = boxes,
    aes(
      xmin = x - 0.9, xmax = x + 0.9,
      ymin = y - 0.35, ymax = y + 0.35
    ),
    fill = "grey95",
    color = "grey30"
  ) +
  # labels
  geom_text(
    data = boxes,
    aes(x = x, y = y, label = label),
    size = 3.3,
    lineheight = 1
  ) +
  # arrows
  geom_segment(
    data = arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.15, "inches")),
    linewidth = 0.6,
    color = "grey30"
  ) +
  coord_cartesian(xlim = c(0.5, 10.8), ylim = c(0.6, 2.5)) +
  theme_void() +
  labs(
    title = "Tier0 workflow: year-pair detectability analysis",
    subtitle = "Single-site baseline GJAM → posterior simulation → detection power vs plot sample size"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

ggsave(
  filename = png_path,
  plot = p,
  width = 13,
  height = 5,
  dpi = 300
)

png_path


# ============================================================
# 2) Conceptual posterior-vs-threshold teaching figure
#    Shows Pr(effect > threshold) and the 0.8 decision
# ============================================================
set.seed(1)
threshold <- 0.10  # conceptual effect threshold; adjust if you want it to match your Tier0 tau
effect_draws <- tibble(effect = rnorm(5000, mean = 0.14, sd = 0.08))
p_exceed <- mean(effect_draws$effect > threshold)

p_concept <- ggplot(effect_draws, aes(x = effect)) +
  geom_histogram(bins = 70) +
  geom_vline(xintercept = threshold, linetype = 2, linewidth = 1) +
  annotate("text", x = threshold, y = Inf, vjust = 1.5, hjust = -0.05,
           label = glue("threshold = {threshold}")) +
  labs(
    title = "Conceptual: posterior for year-pair effect vs detection threshold",
    subtitle = glue("Pr(effect > threshold) = {round(p_exceed, 3)}  |  Detect if > {POWER_TARGET}"),
    x = "Effect (year-pair contrast)",
    y = "Posterior draw count"
  ) +
  theme_minimal(base_size = 12)

gg_save(p_concept, file.path(out_dir, "tier0_concept_posterior_effect_threshold.png"), w = 10, h = 5)

# ============================================================
# 3) Figures from outputs
# ============================================================

# ---- Power curves: use column 'power' exactly ----
if (nrow(power_curve_all) > 0 && all(c("site", "sample_size", "power") %in% names(power_curve_all))) {
  
  pc_summary <- power_curve_all %>%
    mutate(sample_size = as.integer(sample_size)) %>%
    group_by(site, sample_size) %>%
    summarize(
      p50 = median(power, na.rm = TRUE),
      p10 = quantile(power, 0.10, na.rm = TRUE),
      p90 = quantile(power, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  p_all_sites <- ggplot(pc_summary, aes(x = sample_size, y = p50, group = site)) +
    geom_hline(yintercept = POWER_TARGET, linetype = 2) +
    geom_line(alpha = 0.25) +
    labs(
      title = "Tier0 power curves (all sites, summarized)",
      x = "Plot sample size (K)",
      y = "Power (Pr[detection])",
      caption = glue("Dashed line = power target {POWER_TARGET}. Lines = site median across year-pairs/replicates strata in outputs.")
    ) +
    theme_minimal(base_size = 12)
  
  gg_save(p_all_sites, file.path(out_dir, "tier0_power_curves_all_sites.png"), w = 10, h = 6)
  
  p_facet <- ggplot(pc_summary, aes(x = sample_size, y = p50)) +
    geom_hline(yintercept = POWER_TARGET, linetype = 2) +
    geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.20) +
    geom_line() +
    facet_wrap(~ site, scales = "free_x") +
    labs(
      title = "Tier0 power curves by site",
      x = "Plot sample size (K)",
      y = "Power (Pr[detection])"
    ) +
    theme_minimal(base_size = 11)
  
  gg_save(p_facet, file.path(out_dir, "tier0_power_curves_by_site.png"), w = 14, h = 10)
  
  # topline: power at max K
  pc_topline <- pc_summary %>%
    group_by(site) %>%
    filter(sample_size == max(sample_size, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(p50))
  
  write_csv(pc_topline, file.path(out_dir, "tier0_power_topline_at_maxK.csv"))
}

# ---- n*: smallest K where power >= 0.8 (if n_star table already provides it, plot that) ----
if (nrow(n_star_all) > 0 && "site" %in% names(n_star_all)) {
  
  nstar_col <- c("n_star", "n_star_required", "n_required", "nstar") %>%
    keep(~ .x %in% names(n_star_all)) %>%
    first()
  
  if (is.null(nstar_col)) {
    warning("Couldn't find n* column in n_star_power outputs.")
  } else {
    
    ns_plot <- n_star_all %>%
      mutate(n_star_value = .data[[nstar_col]]) %>%
      ggplot(aes(x = fct_reorder(site, n_star_value, .fun = median, na.rm = TRUE),
                 y = n_star_value)) +
      geom_boxplot(outlier_alpha = 0.3) +
      coord_flip() +
      labs(
        title = "Tier0 n* by site",
        subtitle = glue("n* defined as smallest K where power >= {POWER_TARGET}"),
        x = "Site",
        y = glue("n* ({nstar_col})")
      ) +
      theme_minimal(base_size = 11)
    
    gg_save(ns_plot, file.path(out_dir, "tier0_n_star_by_site_boxplots.png"), w = 12, h = 12)
    
    ns_summary <- n_star_all %>%
      mutate(n_star_value = .data[[nstar_col]]) %>%
      group_by(site) %>%
      summarize(
        nstar_median = median(n_star_value, na.rm = TRUE),
        nstar_p10    = quantile(n_star_value, 0.10, na.rm = TRUE),
        nstar_p90    = quantile(n_star_value, 0.90, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(nstar_median)
    
    write_csv(ns_summary, file.path(out_dir, "tier0_n_star_summary_by_site.csv"))
  }
}

# ---- Optional: year-pair heatmap (if columns exist) ----
if (nrow(power_curve_all) > 0 &&
    all(c("site", "sample_size", "power", "year_baseline", "year_perturbed") %in% names(power_curve_all))) {
  
  # For each site + yearpair, compute n* directly from power_curve (robust)
  nstar_from_pc <- power_curve_all %>%
    mutate(sample_size = as.integer(sample_size)) %>%
    group_by(site, year_baseline, year_perturbed, sample_size) %>%
    summarize(power = median(power, na.rm = TRUE), .groups = "drop") %>%
    group_by(site, year_baseline, year_perturbed) %>%
    summarize(
      n_star = suppressWarnings(min(sample_size[power >= POWER_TARGET], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(n_star = ifelse(is.infinite(n_star), NA, n_star))
  
  write_csv(nstar_from_pc, file.path(out_dir, "tier0_n_star_from_power_curve_yearpairs.csv"))
  
  # Heatmap for one site (pick the first) + a combined faceted heatmap (can be big)
  one_site <- nstar_from_pc %>% filter(site == first(site))
  p_heat1 <- ggplot(one_site, aes(x = year_baseline, y = year_perturbed, fill = n_star)) +
    geom_tile() +
    labs(
      title = glue("Tier0 n* by year-pair (site = {first(one_site$site)})"),
      x = "Baseline year",
      y = "Perturbed year"
    ) +
    theme_minimal(base_size = 12)
  
  gg_save(p_heat1, file.path(out_dir, glue("tier0_yearpair_nstar_heatmap_{first(one_site$site)}.png")), w = 9, h = 6)
}

message("Done. Outputs written to: ", out_dir)

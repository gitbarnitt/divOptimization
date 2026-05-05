set.seed(1)
effect <- 0.2
post <- rnorm(1000, mean=effect, sd=0.1)
hist(post, breaks=30, col='skyblue', main='Posterior Distribution of Effect', xlab='Effect Size')
abline(v=0, col='red', lwd=2, lty=2)
abline(v=effect, col='darkgreen', lwd=2)
legend('topright', legend=c('Null (0)', 'Imposed Effect'), col=c('red','darkgreen'), lty=2:1, lwd=2)

library(arrow)
library(dplyr)
library(ggplot2)

site <- "BART"
pc <- read_parquet(paste0("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0/", site, "/power_curve.parquet"))

ggplot(pc, aes(x=sample_size, y=power, color=interaction(year_baseline, year_perturbed))) +
  geom_line() + geom_point() +
  labs(title=paste("Power Curves for", site),
       x="Sample Size (K)", y="Detection Probability (Power)",
       color="Year Pair") +
  theme_minimal()
ggsave(paste0("outputs/tier0/figuresTables/", site, "_power_curve.png"))




library(dplyr)
pc_summary <- pc %>%
  group_by(sample_size) %>%
  summarize(mean_power = mean(power, na.rm=TRUE))

ggplot(pc, aes(x=sample_size, y=power, group=interaction(year_baseline, year_perturbed), color=interaction(year_baseline, year_perturbed))) +
  geom_line(alpha=0.3) + geom_point(alpha=0.5) +
  geom_line(data=pc_summary, aes(x=sample_size, y=mean_power), color="black", size=1.2, inherit.aes=FALSE) +
  labs(title="Power Curves for BART", x="Sample Size (K)", y="Detection Probability (Power)", color="Year Pair") +
  theme_minimal()



##
library(dplyr)
library(ggplot2)

# Summarize mean and variability across year-pairs for each K
pc_summary <- pc %>%
  group_by(sample_size) %>%
  summarize(
    mean_power = mean(power, na.rm=TRUE),
    sd_power = sd(power, na.rm=TRUE),
    n = n(),
    se_power = sd_power / sqrt(n),
    lower = mean_power - 1.96 * se_power,
    upper = mean_power + 1.96 * se_power
  )

ggplot(pc, aes(x=sample_size, y=power, group=interaction(year_baseline, year_perturbed), color=interaction(year_baseline, year_perturbed))) +
  geom_line(alpha=0.2) + geom_point(alpha=0.4) +
  geom_line(data=pc_summary, aes(x=sample_size, y=mean_power), color="black", size=1.2, inherit.aes=FALSE) +
  geom_ribbon(data=pc_summary, aes(x=sample_size, ymin=lower, ymax=upper), fill="black", alpha=0.2, inherit.aes=FALSE) +
  labs(title="Power Curves for BART", x="Sample Size (K)", y="Detection Probability (Power)", color="Year Pair") +
  theme_minimal()



####

library(arrow)
library(dplyr)
library(ggplot2)

# Set your site name
site <- "ABBY"

# Read the power curve data
pc <- read_parquet(paste0("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0/", site, "/power_curve.parquet"))

# Summarize mean and 95% CI across year-pairs for each sample size
pc_summary <- pc %>%
  group_by(sample_size) %>%
  summarize(
    mean_power = mean(power, na.rm=TRUE),
    n = n(),
    sd_power = sd(power, na.rm=TRUE),
    se_power = sd_power / sqrt(n),
    lower = mean_power - 1.96 * se_power,
    upper = mean_power + 1.96 * se_power
  )

# Plot: mean line with confidence ribbon
ggplot(pc_summary, aes(x = sample_size, y = mean_power)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.2) +
  geom_line(color = "black", size = 1.2) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = sort(unique(pc_summary$sample_size))) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Sample Size (K)",
    y = "Mean Detection Probability (Power)",
    title = paste("Summary Power Curve for", site),
    subtitle = "Mean across year-pairs with 95% CI"
  ) +
  theme_bw()

# Save the figure
ggsave(
  filename = paste0("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0/figuresTables/", site, "_summary_power_curve.png"),
  width = 6, height = 4, dpi = 300
)


##summary table
library(arrow)
library(dplyr)
library(purrr)
library(tidyr)

base_dir <- "C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0"
site_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
site_names <- basename(site_dirs)

summarize_site <- function(site, site_dir) {
  pc_path <- file.path(site_dir, "power_curve.parquet")
  if (!file.exists(pc_path)) return(NULL)
  pc <- read_parquet(pc_path)
  
  summary <- pc %>%
    group_by(sample_size) %>%
    summarize(
      mean_power = mean(power, na.rm=TRUE),
      sd_power = sd(power, na.rm=TRUE),
      n_year_pairs = n(),
      se_power = sd_power / sqrt(n_year_pairs),
      lower_95 = mean_power - 1.96 * se_power,
      upper_95 = mean_power + 1.96 * se_power,
      frac_year_pairs_80 = mean(power >= 0.8, na.rm=TRUE)
    ) %>%
    arrange(sample_size)
  
  n_star_row <- summary %>% filter(mean_power >= 0.8) %>% slice_head(n = 1)
  if (nrow(n_star_row) == 0) {
    n_star <- NA
    mean_power_nstar <- NA
    sd_power_nstar <- NA
    lower_95_nstar <- NA
    upper_95_nstar <- NA
    frac_year_pairs_80_nstar <- NA
    n_year_pairs_nstar <- NA
  } else {
    n_star <- n_star_row$sample_size
    mean_power_nstar <- n_star_row$mean_power
    sd_power_nstar <- n_star_row$sd_power
    lower_95_nstar <- n_star_row$lower_95
    upper_95_nstar <- n_star_row$upper_95
    frac_year_pairs_80_nstar <- n_star_row$frac_year_pairs_80
    n_year_pairs_nstar <- n_star_row$n_year_pairs
  }
  
  tibble(
    site = site,
    n_star = n_star,
    mean_power_nstar = mean_power_nstar,
    sd_power_nstar = sd_power_nstar,
    lower_95_nstar = lower_95_nstar,
    upper_95_nstar = upper_95_nstar,
    frac_year_pairs_80_nstar = frac_year_pairs_80_nstar,
    n_year_pairs_nstar = n_year_pairs_nstar
  )
}

site_summaries <- map2_dfr(site_names, site_dirs, summarize_site)

write.csv(site_summaries, file = file.path(base_dir, "figuresTables", "n_star_summary_detailed.csv"), row.names = FALSE)
print(site_summaries)


######summary table seems a bit strange, STER does not cross threshold, BART only needs 8 plots

site <- "BART"

#distribution of power at site
pc <- read_parquet(file.path(base_dir, site, "power_curve.parquet"))
ggplot(pc, aes(x = sample_size, y = power, color = interaction(year_baseline, year_perturbed))) +
  geom_line() + geom_point() +
  theme_minimal()


##To see the summary for all year-pairs at max K:
pc %>% filter(sample_size == max(sample_size)) %>% arrange(power)

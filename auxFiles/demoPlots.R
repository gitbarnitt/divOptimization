
#fit$parameters$betaBeta <- fit$chains$bgibbs


##########✅ Step 1: Show results from the fitted GJAM model
###R Code: Visualize Posterior Coefficients (betaBeta)

# Assume you have a GJAM fit object: fit

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)  # ✅ this is required for str_extract()

# Extract posterior coefficient matrix
beta_draws <- fit$fit$chains$bgibbs  # adjust if needed

# Convert to long format
beta_long <- as.data.frame(beta_draws) %>%
  pivot_longer(cols = everything(), names_to = "param", values_to = "value") %>%
  mutate(
    species = str_extract(param, "^[^_]+"),
    predictor = str_remove(param, "^[^_]+_")
  ) %>%
  filter(str_detect(predictor, "^year")) %>%
  mutate(predictor = str_remove(predictor, "^year"))  # year2015 → 2015

# Choose a few species to plot
plot_species <- unique(beta_long$species)[1:4]

# Plot
ggplot(filter(beta_long, species %in% plot_species), aes(x = value)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  facet_grid(species ~ predictor, scales = "free_y") +
  labs(
    title = "Posterior distributions of year coefficients",
    x = "Coefficient value",
    y = "Posterior density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text.x = element_text(angle = 0, size = 10),
    strip.text.y = element_text(size = 10)
  )



##########✅ Step 2: Summary Table of Posterior Means and 95% Credible Intervals

beta_summary <- beta_long %>%
  group_by(species, predictor) %>%
  summarize(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    .groups = "drop"
  )

# View a few rows
print(beta_summary %>% filter(species %in% plot_species))


##########Step 3: Conceptual Figures (Mockups for Remaining Pipeline)

###A. Posterior Predictive Distribution Before and After Simulated Change
#✅ Use this to explain what you would do once prediction is working — compare two distributions and assess overlap or detectability.

# Fake data for illustration (replace later with real predictions)
library(ggplot2)

set.seed(42)
baseline <- rnorm(1000, mean = 5, sd = 1.2)
simulated <- rnorm(1000, mean = 6, sd = 1.2)

df <- data.frame(
  value = c(baseline, simulated),
  condition = rep(c("Baseline", "20% Increase"), each = 1000)
)

ggplot(df, aes(x = value, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(title = "Conceptual: Posterior Predictions Before vs After 20% Abundance Increase",
       x = "Predicted Percent Cover", y = "Density") +
  theme_minimal()


###B. Sensitivity to Sample Size: Detection Probability Curve (Placeholder)
sample_sizes <- c(3, 5, 8, 10, 15, 20)
detection_probs <- c(0.35, 0.48, 0.62, 0.75, 0.88, 0.95)

df_sens <- data.frame(sample_size = sample_sizes, detection = detection_probs)

ggplot(df_sens, aes(x = sample_size, y = detection)) +
  geom_line() +
  geom_point() +
  labs(title = "Conceptual: Sensitivity to Sample Size",
       x = "Number of Plots",
       y = "Detection Probability") +
  ylim(0, 1) +
  theme_minimal()


###B. Sensitivity to Sample Size: Detection Probability Curve (Placeholder)
#✅ This is just a placeholder plot, but it shows your audience where the project is headed.

sample_sizes <- c(3, 5, 8, 10, 15, 20)
detection_probs <- c(0.35, 0.48, 0.62, 0.75, 0.88, 0.95)

df_sens <- data.frame(sample_size = sample_sizes, detection = detection_probs)

ggplot(df_sens, aes(x = sample_size, y = detection)) +
  geom_line() +
  geom_point() +
  labs(title = "Conceptual: Sensitivity to Sample Size",
       x = "Number of Plots",
       y = "Detection Probability") +
  ylim(0, 1) +
  theme_minimal()

# 🗂️ Deliverables You Can Bring to the Meeting
# Posterior coefficient density plot — shows you're extracting uncertainty from the fitted GJAM.
# 
# Summary table or CI plot — clear outputs from the model fit.
# 
# Conceptual predictive comparison figure — helps explain the goal of posterior prediction.
# 
# Mockup of detection probability vs. sample size — shows how this connects to design decisions.
# 
# Would you like help saving these as .png files or formatting them for PowerPoint or a one-page handout?
# 
# 
###### try posterior predict
# Take a single row from xdata (representing one plot)
example_plot <- fit$xdata[1, ]
example_plot

# Create two versions: one with original year, one with changed year
xnew_baseline <- example_plot
xnew_baseline$year <- factor("2017", levels = levels(fit$xdata$year))  # baseline

xnew_changed <- example_plot
xnew_changed$year <- factor("2018", levels = levels(fit$xdata$year))  # simulated

# Stack into one xnew data.frame
xnew <- rbind(xnew_baseline, xnew_changed)
rownames(xnew) <- c("baseline", "changed")

# Run posterior prediction
posterior_preds <- manual_posterior_predict(fit, xnew)

# Extract for one species
target_species <- "ACGR2"
pred_df <- data.frame(
  draw = rep(1:dim(posterior_preds)[1], 2),
  value = c(posterior_preds[, 1, target_species], posterior_preds[, 2, target_species]),
  condition = rep(c("Baseline", "Changed"), each = dim(posterior_preds)[1])
)

# Plot
library(ggplot2)
ggplot(pred_df, aes(x = value, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(title = paste("Posterior Predictions for", target_species),
       x = "Predicted Percent Cover", y = "Density") +
  theme_minimal()

#####

# R CODE: Conceptual sensitivity plot for detectability vs sample size

library(ggplot2)
set.seed(42)

# Simulate posterior draws for 3 sample sizes
n_draws <- 1000
sim_data <- data.frame(
  value = c(
    rnorm(n_draws, 5.0, 1.8),
    rnorm(n_draws, 5.0 * 1.2, 1.8),
    rnorm(n_draws, 5.5, 1.2),
    rnorm(n_draws, 5.5 * 1.2, 1.2),
    rnorm(n_draws, 6.0, 0.8),
    rnorm(n_draws, 6.0 * 1.2, 0.8)
  ),
  condition = rep(c("Baseline", "20% Increase"), times = 3, each = n_draws),
  sample_size = rep(c("5 Plots", "10 Plots", "20 Plots"), each = 2 * n_draws)
)

ggplot(sim_data, aes(x = value, fill = condition)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~sample_size, scales = "free", nrow = 1) +
  labs(
    title = "Conceptual: Effect of Sample Size on Detectability of 20% Abundance Change",
    x = "Predicted Abundance (Posterior Draws)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12), legend.position = "top")


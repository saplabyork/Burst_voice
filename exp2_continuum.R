
exp2_data <- read.csv("/Users/chandan/Desktop/GitHub/Burst_voice/Data/data_exp_237477-vall/Exp2_continuum/data-11-14-25.csv", header=TRUE)

number_of_subs <- length(unique(exp2_data$sub))
number_of_subs


library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# 1. SUBSET DATA BY ONS_CON
# ============================================================================

exp2_data_p <- exp2_data %>%
  filter(ons_con == "p")

exp2_data_k <- exp2_data %>%
  filter(ons_con == "k")

# Check sample sizes
cat("Sample sizes:\n")
cat("ons_con = 'p':", nrow(exp2_data_p), "observations\n")
cat("ons_con = 'k':", nrow(exp2_data_k), "observations\n\n")

# Check subjects in each subset
cat("Subjects with 'p' data:", n_distinct(exp2_data_p$sub), "\n")
cat("Subjects with 'k' data:", n_distinct(exp2_data_k$sub), "\n\n")

# ============================================================================
# 2. FIT SEPARATE BAYESIAN MODELS
# ============================================================================

cat("Fitting model for ons_con = 'p'...\n")
exp2_model_p <- brm(
  response_vc ~ stim_num + (1 | sub),
  data = exp2_data_p, 
  family = bernoulli(),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)

cat("\nFitting model for ons_con = 'k'...\n")
exp2_model_k <- brm(
  response_vc ~ stim_num + (1 | sub),
  data = exp2_data_k, 
  family = bernoulli(),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95)
)

# ============================================================================
# 3. CHECK MODEL CONVERGENCE
# ============================================================================

cat("\n=== MODEL SUMMARY: ons_con = 'p' ===\n")
print(summary(exp2_model_p))

cat("\n\n=== MODEL SUMMARY: ons_con = 'k' ===\n")
print(summary(exp2_model_k))

# Check Rhat values
rhat_p <- rhat(exp2_model_p)
rhat_k <- rhat(exp2_model_k)

cat("\n\nConvergence Check:\n")
cat("Model 'p' - Max Rhat:", max(rhat_p, na.rm = TRUE), "\n")
cat("Model 'k' - Max Rhat:", max(rhat_k, na.rm = TRUE), "\n")

if(max(rhat_p, na.rm = TRUE) > 1.01) {
  warning("Model 'p' has convergence issues!")
}
if(max(rhat_k, na.rm = TRUE) > 1.01) {
  warning("Model 'k' has convergence issues!")
}

# ============================================================================
# 4. CREATE PREDICTIONS FOR BOTH MODELS
# ============================================================================

# Prediction grid for 'p' model
pred_grid_p <- data.frame(
  stim_num = seq(1, 6, by = 0.1)
)

# Prediction grid for 'k' model
pred_grid_k <- data.frame(
  stim_num = seq(1, 6, by = 0.1)
)

# Get predictions for 'p'
posterior_draws_p <- posterior_epred(
  exp2_model_p, 
  newdata = pred_grid_p,
  allow_new_levels = TRUE,
  re_formula = NA
)

posterior_summary_p <- apply(posterior_draws_p, 2, function(x) {
  c(estimate = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

plot_data_p <- pred_grid_p %>%
  mutate(
    ons_con = "p",
    estimate = posterior_summary_p[1, ],
    lower = posterior_summary_p[2, ],
    upper = posterior_summary_p[3, ]
  )

# Get predictions for 'k'
posterior_draws_k <- posterior_epred(
  exp2_model_k, 
  newdata = pred_grid_k,
  allow_new_levels = TRUE,
  re_formula = NA
)

posterior_summary_k <- apply(posterior_draws_k, 2, function(x) {
  c(estimate = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

plot_data_k <- pred_grid_k %>%
  mutate(
    ons_con = "k",
    estimate = posterior_summary_k[1, ],
    lower = posterior_summary_k[2, ],
    upper = posterior_summary_k[3, ]
  )

# Combine both for plotting
plot_data_combined <- bind_rows(plot_data_p, plot_data_k)

# ============================================================================
# 5. CREATE COMBINED CURVE PLOT
# ============================================================================

p1 <- ggplot(plot_data_combined, aes(x = stim_num, y = estimate, 
                                     color = ons_con, fill = ons_con)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    title = "Voicing Perception Across Stimulus Continuum",
    subtitle = "Separate Bayesian models by onset consonant",
    x = "Stimulus Number",
    y = "Predicted Probability of Voicing Response",
    color = "Onset Consonant",
    fill = "Onset Consonant"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_line(color = "grey95", size = 0.3)
  ) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = c("p" = "#E41A1C", "k" = "#377EB8")) +
  scale_fill_manual(values = c("p" = "#E41A1C", "k" = "#377EB8"))

print(p1)

# ============================================================================
# 5b. CREATE EXTENDED RANGE PLOT TO SHOW FULL S-CURVE
# ============================================================================

cat("\n\nCreating extended prediction range to show full logistic curve...\n")

# Extended prediction grid for 'p' model
pred_grid_p_extended <- data.frame(
  stim_num = seq(-2, 10, by = 0.1)
)

# Extended prediction grid for 'k' model
pred_grid_k_extended <- data.frame(
  stim_num = seq(-2, 10, by = 0.1)
)

# Get predictions for 'p' (extended)
posterior_draws_p_ext <- posterior_epred(
  exp2_model_p, 
  newdata = pred_grid_p_extended,
  allow_new_levels = TRUE,
  re_formula = NA
)

posterior_summary_p_ext <- apply(posterior_draws_p_ext, 2, function(x) {
  c(estimate = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

plot_data_p_extended <- pred_grid_p_extended %>%
  mutate(
    ons_con = "p",
    estimate = posterior_summary_p_ext[1, ],
    lower = posterior_summary_p_ext[2, ],
    upper = posterior_summary_p_ext[3, ]
  )

# Get predictions for 'k' (extended)
posterior_draws_k_ext <- posterior_epred(
  exp2_model_k, 
  newdata = pred_grid_k_extended,
  allow_new_levels = TRUE,
  re_formula = NA
)

posterior_summary_k_ext <- apply(posterior_draws_k_ext, 2, function(x) {
  c(estimate = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

plot_data_k_extended <- pred_grid_k_extended %>%
  mutate(
    ons_con = "k",
    estimate = posterior_summary_k_ext[1, ],
    lower = posterior_summary_k_ext[2, ],
    upper = posterior_summary_k_ext[3, ]
  )

# Combine both for plotting
plot_data_extended <- bind_rows(plot_data_p_extended, plot_data_k_extended)

# Create extended range plot showing full S-curve
p1_extended <- ggplot(plot_data_extended, aes(x = stim_num, y = estimate, 
                                              color = ons_con, fill = ons_con)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  # Add shaded region for actual data range
  annotate("rect", xmin = 1, xmax = 6, ymin = 0, ymax = 1, 
           alpha = 0.1, fill = "gray") +
  annotate("text", x = 3.5, y = 0.95, label = "Actual data range", 
           size = 3.5, color = "gray30") +
  # Add vertical lines at data boundaries
  geom_vline(xintercept = c(1, 6), linetype = "dashed", color = "gray50", alpha = 0.5) +
  labs(
    title = "Full Logistic Curves (Extended Range)",
    subtitle = "Gray region shows actual stimulus range (1-6)",
    x = "Stimulus Number (Extended Range)",
    y = "Predicted Probability of Voicing Response",
    color = "Onset Consonant",
    fill = "Onset Consonant"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_line(color = "grey95", size = 0.3)
  ) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(-2, 10, 2)) +
  scale_color_manual(values = c("p" = "#E41A1C", "k" = "#377EB8")) +
  scale_fill_manual(values = c("p" = "#E41A1C", "k" = "#377EB8"))

print(p1_extended)


# ============================================================================
# 6. PLOT WITH OBSERVED DATA
# ============================================================================

# Calculate observed proportions
observed_data <- exp2_data %>%
  group_by(stim_num, ons_con) %>%
  summarise(
    observed_prop = mean(response_vc),
    n = n(),
    .groups = "drop"
  )

p2 <- ggplot(plot_data_combined, aes(x = stim_num, y = estimate, 
                                     color = ons_con, fill = ons_con)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_point(data = observed_data, 
             aes(x = stim_num, y = observed_prop, color = ons_con),
             size = 3, alpha = 0.6) +
  labs(
    title = "Model Predictions vs. Observed Data",
    subtitle = "Lines = model predictions, Points = observed proportions",
    x = "Stimulus Number",
    y = "Probability of Voicing Response",
    color = "Onset Consonant",
    fill = "Onset Consonant"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_line(color = "grey95", size = 0.3)
  ) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = c("p" = "#E41A1C", "k" = "#377EB8")) +
  scale_fill_manual(values = c("p" = "#E41A1C", "k" = "#377EB8"))

print(p2)

# ============================================================================
# 7. COMPARE SLOPES BETWEEN MODELS
# ============================================================================

cat("\n\n=== COMPARING STIM_NUM EFFECTS ===\n")

# Extract fixed effects
fixef_p <- fixef(exp2_model_p)
fixef_k <- fixef(exp2_model_k)

cat("\nModel 'p' - stim_num effect:\n")
print(fixef_p["stim_num", ])

cat("\nModel 'k' - stim_num effect:\n")
print(fixef_k["stim_num", ])

# Calculate difference in slopes
slope_diff <- fixef_p["stim_num", "Estimate"] - fixef_k["stim_num", "Estimate"]
cat("\nDifference in slopes (p - k):", round(slope_diff, 3), "\n")

# ============================================================================
# 8. PRINT PREDICTIONS AT INTEGER VALUES
# ============================================================================

cat("\n\n=== PREDICTIONS AT EACH STIMULUS NUMBER ===\n")

predictions_summary <- plot_data_combined %>%
  filter(stim_num %in% 1:6) %>%
  select(ons_con, stim_num, estimate, lower, upper) %>%
  arrange(ons_con, stim_num)

print(predictions_summary)

# ============================================================================
# 9. EXPORT (OPTIONAL)
# ============================================================================

# Uncomment to save:
# write.csv(plot_data_combined, "exp2_predictions_separate_models.csv", row.names = FALSE)
# ggsave("exp2_curves_separate_models.png", p1, width = 8, height = 6, dpi = 300)
# ggsave("exp2_curves_with_data.png", p2, width = 8, height = 6, dpi = 300)
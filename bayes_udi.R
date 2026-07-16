# Just the raw results

library(tidyverse)

# Basic summary
exp1_data %>%
  count(response_vc) %>%
  mutate(proportion = n / sum(n))

# By experimental conditions as a latex
library(xtable)

# Create summary table
summary_table <- exp1_data %>%
  group_by(stim_length, ons_con, vowel) %>%
  summarise(
    prop_voiced = mean(response_vc),
    se = sd(response_vc) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(ons_con, vowel, stim_length)

# Round to 2 decimal places
summary_table <- summary_table %>%
  mutate(
    prop_voiced = round(prop_voiced, 2),
    se = round(se, 2)
  )

# Generate LaTeX table
latex_table <- xtable(summary_table,
                      caption = "Proportion of voiced responses by condition",
                      label = "tab:prop_voiced",
                      digits = 2)

# Print LaTeX code
print(latex_table, 
      include.rownames = FALSE,
      caption.placement = "top")


# Bayesian model


exp1_model_full <- glmer(response_vc ~ stim_length * ons_con * vowel + (1 + stim_length | sub), 
                         data = exp1_data, 
                         family = binomial, control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=2e5)))

summary(exp1_model_full)

# Bayes w Udi

library(brms)
attach(exp1_data)

fit_logistic <- brm(
  formula = response_vc ~ stim_length * ons_con * vowel + (1 + stim_length * ons_con * vowel | sub),
  data = exp1_data,
  family = bernoulli(link = "logit"),
  chains = 4, iter = 2000, warmup = 1000, cores = 4 # Recommended settings
)

# View results
summary(fit_logistic)
plot(fit_logistic)

install.packages("tidybayes")
library(tidybayes)
library(tidyverse)
library(brms)

library(tidyverse)
library(brms)

# Get predicted probabilities from the brms model
pred_grid <- expand_grid(
  stim_length = c("short", "long"),
  ons_con = c("k", "p"),  # Only k and p
  vowel = c("a", "e", "i")
)

# Get predictions (population-level)
predictions <- fitted(fit_logistic, 
                      newdata = pred_grid, 
                      re_formula = NA,  # Population-level predictions
                      summary = TRUE)

# Combine with the prediction grid
summary_df <- pred_grid %>%
  mutate(
    mean_prob = predictions[, "Estimate"],
    lower_ci = predictions[, "Q2.5"],
    upper_ci = predictions[, "Q97.5"],
    se_prob = predictions[, "Est.Error"]
  )

# Ensure stim_length is ordered correctly
summary_df$stim_length <- factor(summary_df$stim_length, levels = c("short", "long"))

# Create the plot in the same style as your lme4 version
perception_plot <- ggplot(summary_df, 
                          aes(x = stim_length, y = mean_prob, 
                              color = vowel, group = vowel)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.15, linewidth = 1) +
  facet_wrap(~ ons_con, labeller = labeller(ons_con = c("k" = "[kʰ-]", "p" = "[pʰ-]"))) +
  scale_color_viridis_d(option = "plasma", end = 0.85,
                        labels = c("a" = "æ", "e" = "ɛ", "i" = "ɪ")) +
  labs(x = "Long-lag aspiration Length",
       y = "Predicted Probability of 'Voiced coda' Response",
       color = "Vowel") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 13, color = "grey20"),
    plot.background = element_rect(fill = "white", color = NA)
  )

perception_plot

# Save
ggsave("brms_perception_plot.png", perception_plot, width = 8, height = 5, dpi = 300)
ggsave("brms_perception_plot.pdf", perception_plot, width = 8, height = 5)


# Get individual subject predictions
pred_grid_subjects <- expand_grid(
  stim_length = c("short", "long"),
  ons_con = c("k", "p"),
  vowel = c("a", "e", "i"),
  sub = unique(exp1_data$sub)
)

predictions_indiv <- fitted(fit_logistic, 
                            newdata = pred_grid_subjects, 
                            re_formula = NULL,  # Include random effects
                            summary = TRUE)

summary_df_indiv <- pred_grid_subjects %>%
  mutate(
    mean_prob = predictions_indiv[, "Estimate"]
  )

summary_df_indiv$stim_length <- factor(summary_df_indiv$stim_length, 
                                       levels = c("short", "long"))

# Plot with individual subjects (faint lines) and population average (bold)
perception_plot_indiv <- ggplot() +
  # Individual subjects
  geom_line(data = summary_df_indiv,
            aes(x = stim_length, y = mean_prob, 
                color = vowel, group = interaction(vowel, sub)),
            alpha = 0.2, linewidth = 0.3) +
  # Population average
  geom_line(data = summary_df,
            aes(x = stim_length, y = mean_prob, 
                color = vowel, group = vowel),
            linewidth = 1.5) +
  geom_point(data = summary_df,
             aes(x = stim_length, y = mean_prob, color = vowel),
             size = 4) +
  geom_errorbar(data = summary_df,
                aes(x = stim_length, y = mean_prob, 
                    ymin = lower_ci, ymax = upper_ci, color = vowel), 
                width = 0.15, linewidth = 1) +
  facet_wrap(~ ons_con, labeller = labeller(ons_con = c("k" = "[kʰ-]", "p" = "[pʰ-]"))) +
  scale_color_viridis_d(option = "plasma", end = 0.85,
                        labels = c("a" = "æ", "e" = "ɛ", "i" = "ɪ")) +
  labs(x = "Long-lag aspiration Length",
       y = "Predicted Probability of 'Voiced coda' Response",
       color = "Vowel",
       title = "Population and Individual-Level Predictions",
       subtitle = "Faint lines = individual subjects; bold lines = population average") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 13, color = "grey20"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold")
  )

perception_plot_indiv

ggsave("brms_perception_plot_with_subjects.png", perception_plot_indiv, 
       width = 8, height = 5, dpi = 300)




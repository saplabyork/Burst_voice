
library(dplyr)

combined_df <- bind_rows(per_data, exp1_data)

library(brms)

combined_bayes_full <- brm(
  formula = response_vc ~ stim_length * ons_con * vowel + (1 + stim_length * ons_con * vowel | sub),
  data = combined_df,
  family = bernoulli(link = "logit"),
  chains = 4, iter = 2000, warmup = 1000, cores = 4 # Recommended settings
)

# View results
summary(fit_logistic)
plot(fit_logistic)

install.packages("tidybayes")
library(tidybayes)
library(tidyverse)

# Get predicted probabilities from the brms model
pred_grid <- expand_grid(
  stim_length = c("short", "long"),
  ons_con = c("k", "p"),  # Only k and p
  vowel = c("a", "e", "i")
)

# Get predictions (population-level)
predictions <- fitted(combined_bayes_full, 
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



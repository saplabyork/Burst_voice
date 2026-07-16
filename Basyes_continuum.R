
exp2_data <- read.csv("/Users/chandan/Desktop/GitHub/Burst_voice/Data/data_exp_237477-vall/Exp2_continuum/data-11-14-25.csv", header=TRUE)

library(brms)
library(tidyverse)

# Create VOT_ms column
exp2_data <- exp2_data %>%
  mutate(VOT_ms = case_when(
    ons_con == "p" & stim_num == 1 ~ 25,
    ons_con == "p" & stim_num == 2 ~ 45,
    ons_con == "p" & stim_num == 3 ~ 65,
    ons_con == "p" & stim_num == 4 ~ 85,
    ons_con == "p" & stim_num == 5 ~ 105,
    ons_con == "p" & stim_num == 6 ~ 125,
    ons_con == "k" & stim_num == 1 ~ 50,
    ons_con == "k" & stim_num == 2 ~ 70,
    ons_con == "k" & stim_num == 3 ~ 90,
    ons_con == "k" & stim_num == 4 ~ 110,
    ons_con == "k" & stim_num == 5 ~ 130,
    ons_con == "k" & stim_num == 6 ~ 150
  ))

# Fit the model
model <- brm(
  response_vc ~ stim_num * ons_con * vowel + (1 | sub),
  data = exp2_data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = Intercept),
    prior(normal(0, 0.05), class = b)  # Regularizing prior for slopes
  ),
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  seed = 123
)

# Check the model
summary(model)

# Visualize the slopes
plot_data <- conditional_effects(model, effects = "VOT_ms:ons_con")

# Nicer custom plot
plot(plot_data, plot = FALSE)[[1]] +
  labs(
    x = "VOT Duration (ms)",
    y = "P(Coda = Voiced)",
    color = "Onset Consonant",
    fill = "Onset Consonant"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("p" = "#E69F00", "k" = "#56B4E9"),
                     labels = c("p" = "/p/", "k" = "/k/")) +
  scale_fill_manual(values = c("p" = "#E69F00", "k" = "#56B4E9"),
                    labels = c("p" = "/p/", "k" = "/k/"))

# Alternative: Plot with actual data points
pred_grid <- expand.grid(
  VOT_ms = seq(25, 150, by = 1),
  ons_con = c("p", "k")
)

preds <- fitted(model, newdata = pred_grid, re_formula = NA) %>%
  as.data.frame() %>%
  bind_cols(pred_grid)

ggplot(preds, aes(x = VOT_ms, y = Estimate, color = ons_con, fill = ons_con)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2, color = NA) +
  geom_point(data = exp2_data %>% 
               group_by(VOT_ms, ons_con) %>% 
               summarise(prop_voiced = mean(response_vc), .groups = "drop"),
             aes(y = prop_voiced), size = 2) +
  labs(
    x = "VOT Duration (ms)",
    y = "P(Coda = Voiced)",
    color = "Onset Consonant",
    fill = "Onset Consonant"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("p" = "#E69F00", "k" = "#56B4E9"),
                     labels = c("p" = "/p/", "k" = "/k/")) +
  scale_fill_manual(values = c("p" = "#E69F00", "k" = "#56B4E9"),
                    labels = c("p" = "/p/", "k" = "/k/"))
```

## Interpreting the VOT effect for each consonant

## Adding sub

model_slopes <- brm(
  response_vc ~ VOT_ms * ons_con + (1 | sub) + (0 + VOT_ms | sub),
  data = exp2_data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 2), class = Intercept),
    prior(normal(0, 0.05), class = b)
  ),
  control = list(adapt_delta = 0.95),
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4
)

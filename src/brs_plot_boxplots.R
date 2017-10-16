library(tidyverse)
library(forcats)
library(stringr)
library(viridis)
library(gridExtra)

options(scipen = 999)

scores_long <- read_csv("data/brs_holdout_scores_long.csv")

scores_long <- 
  scores_long %>%
  filter(
    !(model == "bayesian rule set" & outcome %in% c("gpa", "grit", "material hardship"))
  )

scores_long <- 
  scores_long %>%
  group_by(outcome) %>%
  mutate(rel_mse_scaled = scale(rel_mse)[,1]) %>%
  ungroup()

p1 <- 
  scores_long %>%
  group_by(vars_and_scores) %>%
  mutate(rel_mean = mean(rel_mse_scaled)) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(vars_and_scores, rel_mean, .desc = TRUE), 
             y = rel_mse_scaled)) + 
  scale_x_discrete(labels = function(l) str_pad(l, 29, "left")) +
  # ylim(-2.75, 2) +
  geom_boxplot() +
  geom_point(color = "grey50") +
  coord_flip() +
  labs(subtitle = "Approaches to variable incorporation", y = NULL, x = NULL,
       title = "Comparison of Approaches, Summarized across Outcomes") +
  theme_minimal() + 
  theme(plot.title = element_text(size = rel(1.5)), 
        plot.subtitle = element_text(size = rel(1.2)))

p2 <- 
  scores_long %>%
  mutate(data = fct_relevel(data, "mean imputation", "untyped OLS regression imputation"), 
         data = fct_relabel(data, function(x) str_replace(x, "regression imputation", "reg. imp."))) %>%
  group_by(data) %>%
  mutate(rel_mean = mean(rel_mse_scaled)) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(data, rel_mean, .desc = TRUE), 
             y = rel_mse_scaled)) + 
  scale_x_discrete(labels = function(l) str_pad(l, 25, "left")) +
  # ylim(-2.75, 2) +
  geom_boxplot() +
  geom_point(color = "grey50") +
  coord_flip() +
  labs(subtitle = "Approaches to missingness", 
       x = NULL,
       y = "MSE relative to baseline, standardized by outcome" 
       # title = "Comparison of approaches to missingness"
       ) +
  theme_minimal() + 
  theme(plot.subtitle = element_text(size = rel(1.2)))

boxplot_summaries <- grid.arrange(p1, p2, ncol = 1)

ggsave("output/brs_boxplot_summaries.png", boxplot_summaries, 
       width = 8, height = 5)


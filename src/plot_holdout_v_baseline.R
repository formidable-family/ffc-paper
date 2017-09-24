library(tidyverse)
library(forcats)
library(stringr)
library(viridis)

options(scipen = 999)

# leaderboard scores
scores <- read_csv(file.path("data", "ffc_models_leaderboard_scores.csv"))

# holdout scores
holdout <- read_csv(file.path("data", "FormidableFamily_holdout_results.csv"))
names(holdout) <- c("FF submission no", names(scores)[8:13])

# baseline scores against holdout data DIFFER from baseline against leaderboard
# from http://www.princeton.edu/~akindel/ffc/final_scores/
baseline <- 
  data_frame(
    outcome = names(scores)[8:13],
    baseline = c(
      0.425148881, 
      0.252983596,
      0.024905617,
      0.055457913,
      0.167223718,
      0.185329492
    )
  )

# leaderboard baseline in original data set
zz_baseline <-
  scores %>%
  filter(model == "baseline") %>%
  select(gpa:`job training`) %>%
  gather(outcome, baseline, gpa:`job training`)

zz_scores <- scores

scores <- 
  scores %>%
  filter(model != "baseline") %>%
  # remove leaderboard scores
  select(-c(gpa:`job training`)) %>%
  # replace with holdout scores
  left_join(holdout, by = "FF submission no") %>%
  mutate(data = as_factor(data)) %>%
  select(-`FF submission no`, -`name of submission zip`) %>%
  mutate(
    vars_and_scores = case_when(
      scores == "none" ~ variables,
      scores == "expert" ~ "hp + expert scores",
      scores == "mturk" ~ "hp + mturk scores"
    ), 
    vars_and_scores = as_factor(vars_and_scores)
  ) %>%
  select(name = `model name`, model, scores, variables, vars_and_scores,
         everything()) 
  
scores_long <-
  scores %>%
  gather(outcome, mse, gpa:`job training`) %>%
  left_join(baseline, by = "outcome") %>%
  mutate(outcome = as_factor(outcome), 
         rel_mse = mse - baseline) 

write_csv(scores, "data/holdout_scores_wide.csv")
write_csv(scores_long, "data/holdout_scores_long.csv")

scores_long %>%
  filter(variables == "human priors") %>%
  ggplot(aes(x = fct_rev(outcome), y = rel_mse, fill = fct_rev(data))) + 
  facet_wrap(~ scores, ncol = 1) +
  scale_fill_viridis(discrete = TRUE) +
  geom_col(position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, 
       x = NULL, 
       y = "Out-of-sample MSE, leaderboard data", 
       title = "Comparison of Mean Squared Error by Imputation Strategy", 
       subtitle = "For human-prior variables, varying ranking information")

scores_long %>%
  filter(variables == "human priors") %>%
  ggplot(aes(x = fct_rev(scores), y = rel_mse, fill = fct_rev(data))) + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_x", dir = "v") +
  scale_fill_viridis(discrete = TRUE) +
  geom_col(position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, 
       x = NULL, 
       y = "Out-of-sample MSE, leaderboard data", 
       title = "Comparison of Mean Squared Error by Imputation Strategy", 
       subtitle = "For human-prior variables, varying ranking information")

scores_long %>%
  filter(variables == "human priors") %>%
  ggplot(aes(x = fct_rev(scores), y = rel_mse, color = fct_rev(data))) + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_x", dir = "v") +
  geom_hline(yintercept = 0) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() + 
  coord_flip() + 
  theme_minimal() + 
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = NULL, 
       x = NULL, 
       y = "Out-of-sample MSE, holdout data", 
       title = "Comparison of Mean Squared Error by Imputation Strategy", 
       subtitle = "For human-prior variables, varying ranking information")

scores_long %>%
  filter(scores == "none") %>%
  ggplot(aes(x = fct_rev(variables), y = rel_mse, color = fct_rev(data))) + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_x", dir = "v") +
  geom_hline(yintercept = 0) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() + 
  coord_flip() + 
  theme_minimal() + 
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = NULL, 
       x = NULL, 
       y = "Out-of-sample MSE, leaderboard data", 
       title = "Comparison of Mean Squared Error by Imputation Strategy", 
       subtitle = "For all sets of variables with no ranking information")

scores_long %>%
  filter(scores == "none") %>%
  ggplot(aes(x = fct_rev(variables), y = rel_mse, fill = fct_rev(data))) + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_x", dir = "v") +
  geom_hline(yintercept = 0) +
  scale_fill_viridis(discrete = TRUE) +
  geom_col(position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, 
       x = NULL, 
       y = "Out-of-sample MSE, leaderboard data", 
       title = "Comparison of Mean Squared Error by Imputation Strategy", 
       subtitle = "For all sets of variables with no ranking information")

holdout_v1 <- 
  scores_long %>%
  mutate(data = fct_relevel(data, "mean imputation", "untyped OLS regression imputation"), 
         data = fct_relabel(data, function(x) str_replace(x, "regression imputation", "reg. imp."))) %>%
  ggplot(aes(x = fct_rev(vars_and_scores), y = rel_mse, color = fct_rev(data))) + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_x", dir = "v") +
  geom_hline(yintercept = 0) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  geom_point() + 
  coord_flip() + 
  theme_minimal() + 
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = NULL, 
       x = NULL, 
       y = "Out-of-sample MSE - baseline, from holdout data", 
       title = "Mean Squared Error relative to Baseline") + 
  theme(panel.spacing.x = unit(3, "lines"))

ggsave(filename = file.path("output", "holdout_v1.png"), 
       plot = holdout_v1, 
       height = 5, width = 8)

holdout_v2 <- 
  scores_long %>%
  mutate(data = fct_relevel(data, "mean imputation", "untyped OLS regression imputation"), 
         data = fct_relabel(data, function(x) str_replace(x, "regression imputation", "reg. imp."))) %>%
  ggplot(aes(x = fct_rev(data), y = rel_mse, color = fct_rev(vars_and_scores))) + 
  facet_wrap(~ outcome, ncol = 2, scales = "free_x", dir = "v") +
  geom_hline(yintercept = 0) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  geom_point() + 
  coord_flip() + 
  theme_minimal() + 
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = "Variables and priors", 
       x = NULL, 
       y = "Out-of-sample MSE - baseline, from holdout data", 
       title = "Mean Squared Error relative to Baseline") + 
  theme(panel.spacing.x = unit(3, "lines"))

ggsave(filename = file.path("output", "holdout_v2.png"), 
       plot = holdout_v2, 
       height = 5, width = 8)

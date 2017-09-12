library(tidyverse)
library(forcats)
library(viridis)

scores <- read_csv(file.path("data", "ffc_models_leaderboard_scores.csv"))

scores <- 
  scores %>%
  mutate(data = as_factor(data))

p_imputation_1 <- 
  scores %>%
  filter(scores == "none", variables == "human priors") %>%
  gather(outcome, mse, gpa:`job training`) %>%
  mutate(outcome = as_factor(outcome)) %>%
  ggplot(aes(x = fct_rev(outcome), y = mse, fill = fct_rev(data))) + 
  scale_fill_viridis(discrete = TRUE) +
  geom_col(position = "dodge") + 
  coord_flip() + 
  theme_minimal() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, # "imputation strategy", 
       x = NULL, 
       y = "Out-of-sample MSE, leaderboard data", 
       title = "Comparison of Mean Squared Error by Imputation Strategy", 
       subtitle = "For human-prior variables with no ranking information")
  
ggsave(filename = file.path(output, "p_imputation_1.png"), 
       plot = p_imputation_1, 
       height = 5, width = 8)

p_imputation_2 <-
  scores %>%
  filter(variables == "human priors") %>%
  gather(outcome, mse, gpa:`job training`) %>%
  mutate(outcome = as_factor(outcome)) %>%
  ggplot(aes(x = fct_rev(outcome), y = mse, fill = fct_rev(data))) + 
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

p_imputation_3 <- 
  scores %>%
  filter(scores == "none") %>%
  gather(outcome, mse, gpa:`job training`) %>%
  mutate(outcome = as_factor(outcome)) %>%
  ggplot(aes(x = fct_rev(outcome), y = mse, fill = fct_rev(data))) + 
  facet_wrap(~ variables, ncol = 1) +
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

ggsave(filename = file.path("output", "p_imputation_2.png"), 
       plot = p_imputation_2, 
       height = 8, width = 6)

ggsave(filename = file.path("output", "p_imputation_3.png"), 
       plot = p_imputation_3, 
       height = 8, width = 6)
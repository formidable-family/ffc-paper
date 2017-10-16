library(tidyverse)
library(forcats)
library(stringr)

options(scipen = 999)

scores <- read_csv("data/brs_holdout_scores_wide.csv")

scores_relabeled <- 
  scores %>%
  mutate(data = fct_relevel(data, "mean imputation", "untyped OLS regression imputation"), 
         data = fct_relabel(data, function(x) str_replace(x, " regression imputation", "")), 
         vars_and_scores = str_replace(vars_and_scores, " scores", ""), 
         vars_and_scores = str_replace(vars_and_scores, "Bayesian rule set", "brs"))

scores_relabeled %>%
  select(vars_and_scores, data, gpa:`job training`) %>%
  knitr::kable(format = "latex", digits = 4) %>%
  write_lines("output/appendix_table_base.tex")

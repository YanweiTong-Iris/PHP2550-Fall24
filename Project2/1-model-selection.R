# model selection scratch code
# Load necessary packages
library(tidyverse)
library(kableExtra)
library(knitr)
library(ggplot2)
library(glmnet)
library(pROC)
library(MASS)


# Define data path and import data
data_path = "/Users/yanweitong/Documents/PHP2550-Data/Project2"
data = read.csv(paste0(data_path, "/project2.csv"))

# Data preprocessing
data = data  %>%
  # create race variable
  mutate(race = factor(case_when(
    NHW == 1 ~ "White",
    Black == 1 ~ "Black",
    Hisp == 1 ~ "Hispanic",
    TRUE ~ "Other"  # Handle cases where none of the above conditions are met
  ), levels = c("White", "Black", "Hispanic", "Other"))) %>%
  # create treatment categories
  mutate(treatment_cat = factor(case_when(BA == 1 & Var == 0 ~ "BASC+placebo",
                                          BA == 0 & Var == 0 ~ "ST+placebo",
                                          BA == 1 & Var == 1 ~ "BASC+varenicline",
                                          BA == 0 & Var == 1 ~ "ST+varenicline"))) %>% 
  # factorize categorical/ordinal variables
  mutate(
    abst = factor(abst),
    Var = factor(Var),
    BA = factor(BA),
    sex_ps = factor(sex_ps),
    ftcd.5.mins = factor(ftcd.5.mins),
    otherdiag = factor(otherdiag),
    antidepmed = factor(antidepmed),
    mde_curr = factor(mde_curr),
    Only.Menthol = factor(Only.Menthol),
    edu = factor(edu, levels = c(1, 2, 3, 4, 5)),
    inc = factor(inc, levels = c(1, 2, 3, 4, 5))
  ) %>%
  # make integers numeric 
  mutate(across(
    .cols = where(is.integer) & !all_of("id"),
    .fns = as.numeric 
  ))



# Define the outcome variable and predictors
outcome <- data$abst
predictor_names <- c("Var", "BA", "age_ps", "sex_ps", "inc", "edu", "race",
                     "ftcd_score", "ftcd.5.mins", "bdi_score_w00", "cpd_ps",
                     "crv_total_pq1", "hedonsum_n_pq1", "hedonsum_y_pq1",
                     "shaps_score_pq1", "otherdiag", "antidepmed", "mde_curr",
                     "NMR", "Only.Menthol", "readiness")
predictors <- data[, predictor_names]
# for Lasso (to break down factors with >2 levels)
predictors_dummy <- model.matrix(~ 0 + ., data = predictors)
# remove the extra reference group
predictors_dummy <- predictors_dummy[, -which(colnames(predictors_dummy) =="Var0")]

# Split into train and test
set.seed(2024)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index,]
test_data <- data[-train_index,]
train_outcome <- outcome[train_index]
test_outcome <- outcome[-train_index]
# for best subset
train_predictors <- predictors[train_index, ]
test_predictors <- predictors[-train_index, ]
# for Lasso 
train_predictors_dummy <- predictors_dummy[train_index, ]
test_predictors_dummy <- predictors_dummy[-train_index, ]

train_data_lasso = data.frame(tb = train_outcome, train_predictors_dummy)
test_data_lasso = data.frame(tb = test_outcome, test_predictors_dummy)

train_bestglm <- data.frame(train_predictors, tb = train_outcome)
test_bestglm <- data.frame(test_predictors, tb = test_outcome)

# Lasso


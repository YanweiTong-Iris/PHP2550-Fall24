# model selection scratch code
# Load necessary packages
library(tidyverse)
library(ggplot2)
library(glmnet)
library(bestglm)
library(mice)
library(pROC)
library(MASS)
library(kableExtra)
library(knitr)
library(gtsummary)
library(gt)


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

# Perform MICE imputation
data_mice <- mice(data, m = 5, method = "pmm", 
                  maxit = 50, seed = 2024, printFlag = FALSE)

# Complete the data by extracting one of the imputed datasets
data_imp <- complete(data_mice, action = 1)


# Define the outcome variable and predictors
outcome <- data_imp$abst
predictor_names <- c("Var", "BA", "age_ps", "sex_ps", "inc", "edu", "race",
                     "ftcd_score", "ftcd.5.mins", "bdi_score_w00", "cpd_ps",
                     "crv_total_pq1", "hedonsum_n_pq1", "hedonsum_y_pq1",
                     "shaps_score_pq1", "otherdiag", "antidepmed", "mde_curr",
                     "NMR", "Only.Menthol", "readiness")
predictors <- data_imp[, predictor_names]
# for Lasso and Ridge (to break down factors with >2 levels)
predictors_dummy <- model.matrix(~ 0 + ., data = predictors)
# remove the extra reference group
predictors_dummy <- predictors_dummy[, -which(colnames(predictors_dummy) =="Var0")]

# Split into train and test
set.seed(2024)
train_index <- sample(1:nrow(data_imp), 0.7 * nrow(data_imp))
train_data <- data_imp[train_index,]
test_data <- data_imp[-train_index,]
train_outcome <- outcome[train_index]
test_outcome <- outcome[-train_index]
# for best subset
train_predictors <- predictors[train_index, ]
test_predictors <- predictors[-train_index, ]
# for Lasso and Ridge
train_predictors_dummy <- predictors_dummy[train_index, ]
test_predictors_dummy <- predictors_dummy[-train_index, ]

train_data_glmnet = data.frame(abst = train_outcome, train_predictors_dummy)
test_data_glmnet = data.frame(abst = test_outcome, test_predictors_dummy)

train_bestglm <- data.frame(train_predictors, abst = train_outcome)
test_bestglm <- data.frame(test_predictors, abst = test_outcome)

# Enforce Var and BA as 0 penalty
# Lasso
# ^2 generates all pairwise interactions
train_predictors_dummy_df <- as.data.frame(train_predictors_dummy)
train_predictors_dummy_interactions <- model.matrix(~ .^2, data = train_predictors_dummy_df)

# Step 2: Set penalty factors
# Initialize penalty factors to 1 for all variables
penalty_factors <- rep(1, ncol(train_predictors_dummy_interactions))

# Identify columns corresponding exactly to "Var1" and "BA1" (not their interactions)
var1_col <- grep("^Var1$", colnames(train_predictors_dummy_interactions))
ba1_col <- grep("^BA1$", colnames(train_predictors_dummy_interactions))

penalty_factors[c(var1_col, ba1_col)] <- 0
names(penalty_factors) <- colnames(train_predictors_dummy_interactions)


# Step 3: Fit the LASSO model with penalty factors
lasso_model <- cv.glmnet(as.matrix(train_predictors_dummy_interactions), train_outcome,
                         penalty.factor = penalty_factors,
                         alpha = 1, family = "binomial")

# Visualize the cross-validation results
plot(lasso_model,
     main = "") 

mtext("Deviance vs. log(lambda)", side = 3, line = 2, cex = 1.2, font = 2)

# Extract coefficients at the optimal lambda (best_lambda)
best_lambda_lasso <- lasso_model$lambda.min
#remove intercept
optimal_coefs_lasso <- as.numeric(coef(lasso_model, s = best_lambda_lasso)[-1])
coef_names_lasso <- rownames(coef(lasso_model, s = best_lambda_lasso))[-1]  

result_table_lasso <- data.frame(
  Predictor = coef_names_lasso,
  Coefficient = optimal_coefs_lasso
) %>%
  filter(Coefficient != 0) 

kable(result_table_lasso, 
      caption = "Lasso Model Coefficientsc at Optimal Lambda")


# Ridge
ridge_model <- cv.glmnet(as.matrix(train_predictors_dummy_interactions), train_outcome,
                         penalty.factor = penalty_factors,
                         alpha = 0, family = "binomial")

# Visualize the cross-validation results
plot(ridge_model,
     main = "") 

mtext("Deviance vs. log(lambda)", side = 3, line = 2, cex = 1.2, font = 2)

# Extract coefficients at the optimal lambda (best_lambda)
best_lambda_ridge <- ridge_model$lambda.min
#remove intercept
optimal_coefs_ridge <- as.numeric(coef(ridge_model, s = best_lambda_ridge)[-1])
coef_names_ridge <- rownames(coef(ridge_model, s = best_lambda_ridge))[-1]  

result_table_ridge <- data.frame(
  Predictor = coef_names_ridge,
  Coefficient = optimal_coefs_ridge
)

kable(result_table_ridge, 
      caption = "Ridge Model Coefficients at Optimal Lambda")


# Best subset GLM
enforced_variables <- c("Var", "BA")  # Variables you want to enforce

# Create x.fixed matrix for enforced variables
x.fixed <-train_bestglm[,enforced_variables, drop = FALSE]

bestglm_model <- bestglm(
  Xy = train_bestglm,
  family = binomial,
  IC = "BIC",               
  nvmax = NULL,              
  x.fixed = x.fixed          # Enforces Var1 and BA1 to be included in all models
)

# View the best model
print(bestglm_model$BestModel)

best_model <- glmulti(
  y = abst ~ Var + BA + .,  # Enforce both Var and BA in the formula
  data = train_bestglm,
  #xr = c("Var", "BA"),
  intercept = F,
  method = "h",           # Use exhaustive search
  crit = "bic",
  confsetsize = 3,        # Number of best models to keep in the confidence set
  level = 2,
  minsize = 5,
  maxsize = 20,
  family = binomial,
  plotty = F,
  report = T
  #exclude = c("age_ps:sex_ps", "age_ps:inc", "age_ps:edu_merged")
)
summary(best_model)


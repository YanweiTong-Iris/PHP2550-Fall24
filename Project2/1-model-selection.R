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


# Logistic regression
# Define the outcome and variables in the model
outcome <- data_imp$abst
variable_names <- c("Var", "BA", "age_ps", "sex_ps", "inc", "edu_merged", "race",
                    "ftcd_score", "ftcd.5.mins", "bdi_score_w00", "cpd_ps",
                    "crv_total_pq1", "hedonsum_n_pq1", "hedonsum_y_pq1",
                    "shaps_score_pq1", "otherdiag", "antidepmed", "mde_curr",
                    "NMR", "Only.Menthol", "readiness")
variables <- data_imp[, variable_names]
# for Lasso (to break down factors with >2 levels)
variables_dummy <- model.matrix(~ 0 + ., data = variables)
# remove the extra reference group
variables_dummy <- variables_dummy[, -which(colnames(variables_dummy) =="Var0")]

# Split into train and test
set.seed(1)
train_index <- sample(1:nrow(data_imp), 0.7 * nrow(data_imp))
train_data <- data_imp[train_index,]
test_data <- data_imp[-train_index,]
train_outcome <- outcome[train_index]
test_outcome <- outcome[-train_index]
# for best subset
train_variables <- variables[train_index, ]
test_variables <- variables[-train_index, ]


# Main effects for `Var` and `BA` (to be controlled in all models)
main_effects <- paste(c("Var", "BA", "age_ps", "sex_ps", "inc", "edu_merged", "race",
                        "ftcd_score", "ftcd.5.mins", "bdi_score_w00", "cpd_ps",
                        "crv_total_pq1", "hedonsum_n_pq1", "hedonsum_y_pq1",
                        "shaps_score_pq1", "otherdiag", "antidepmed", "mde_curr",
                        "NMR", "Only.Menthol", "readiness"), collapse = " + ")

# Define the interaction terms we want to consider
interaction_terms <- paste(
  "BA:mde_curr + BA:age_ps + BA:sex_ps + BA:race + BA:ftcd_score + BA:cpd_ps",
  "Var:age_ps + Var:sex_ps + Var:race + Var:ftcd_score + Var:cpd_ps",
  "inc:edu_merged + antidepmed:readiness + Only.Menthol:readiness + mde_curr:readiness",
  "ftcd.5.mins:readiness + bdi_score_w00:readiness + Var:shaps_score_pq1 + BA:shaps_score_pq1",
  "shaps_score_pq1:mde_curr + sex_ps:ftcd_score + race:ftcd_score + age_ps:ftcd_score",
  "sex_ps:Only.Menthol + race:Only.Menthol + inc:Only.Menthol + edu_merged:Only.Menthol",
  "sex_ps:NMR + age_ps:NMR + cpd_ps:NMR + NMR:readiness + ftcd_score:NMR",
  sep = " + "
)

# Full formula for main effects and interactions
full_formula <- as.formula(paste("abst ~", main_effects, "+", interaction_terms))

# Define scope with `Var` and `BA` as forced terms in the main model
scope_list <- list(
  lower = as.formula("abst ~ Var + BA"),  # Minimal model with controlled terms
  upper = full_formula                    # Full model with all main and interaction terms
)

# Fit logistic regression model with stepwise selection
set.seed(2024)
logistic_model <- step(
  glm(formula = abst ~ Var + BA, data = train_data, family = "binomial"),
  scope = scope_list,
  direction = "both",
  trace = 0
)

# Display the summary of the final model
summary(logistic_model)



# for L0 + L1
train_variables_dummy <- variables_dummy[train_index, ]
test_variables_dummy <- variables_dummy[-train_index, ]

train_data_glmnet = data.frame(abst = train_outcome, train_variables_dummy)
test_data_glmnet = data.frame(abst = test_outcome, test_variables_dummy)

train_bestglm <- data.frame(train_variables, abst = train_outcome)
test_bestglm <- data.frame(test_variables, abst = test_outcome)

# Enforce Var and BA as 0 penalty
# Elastic Net
# ^2 generates all pairwise interactions
train_variables_dummy_df <- as.data.frame(train_variables_dummy)
train_variables_dummy_full_interactions <- model.matrix(~ .^2, 
                                                        data = train_variables_dummy_df)

test_variables_dummy_df <- as.data.frame(test_variables_dummy)
test_variables_dummy_full_interactions <- model.matrix(~ .^2, 
                                                       data = test_variables_dummy_df)

# To identify the potential interaction terms for moderator effects
train_variables_dummy_include_names <- c(
  "Var1", "BA1", "age_ps", "sex_ps2", "inc2", "inc3", 
  "inc4", "inc5", "edu_merged2", "edu_merged3",
  "raceBlack", "raceHispanic", "raceOther", 
  "ftcd_score", "ftcd.5.mins1", "bdi_score_w00", "cpd_ps",
  "crv_total_pq1", "hedonsum_n_pq1", "hedonsum_y_pq1",              
  "shaps_score_pq1", "otherdiag1", "antidepmed1",                  
  "mde_curr1", "NMR", "Only.Menthol1",                 
  "readiness", "Var1:BA1", 
  #Behavioral treatment
  "BA1:mde_curr1", 
  "BA1:age_ps", "BA1:sex_ps2", 
  "BA1:raceBlack", "BA1:raceHispanic",
  "BA1:raceOther", "BA1:ftcd_score", 
  # Psychotherapy
  "Var1:mde_curr1",
  "Var1:age_ps", "Var1:sex_ps2",
  "Var1:raceBlack", "Var1:raceHispanic",
  "Var1:raceOther", "Var1:ftcd_score", 
  # Income*Edu
  "inc2:edu_merged2", "inc2:edu_merged3", 
  "inc3:edu_merged2", "inc3:edu_merged3", 
  "inc4:edu_merged2", "inc4:edu_merged3", 
  "inc5:edu_merged2", "inc5:edu_merged3", 
  # Readiness to quit
  "antidepmed1:readiness", "Only.Menthol1:readiness", 
  "mde_curr1:readiness", "ftcd.5.mins1:readiness", 
  "bdi_score_w00:readiness",  
  # Anhedonia
  "BA1:shaps_score_pq1",
  "shaps_score_pq1:mde_curr1", 
  "shaps_score_pq1:otherdiag1",
  "shaps_score_pq1:antidepmed1",  
  "Var1:shaps_score_pq1",
  # FTCD Score 
  "sex_ps2:ftcd_score", "raceBlack:ftcd_score", 
  "raceHispanic:ftcd_score", "raceOther:ftcd_score", 
  "age_ps:ftcd_score", 
  # Menthol exclusive
  "sex_ps2:Only.Menthol1", "raceBlack:Only.Menthol1", 
  "raceHispanic:Only.Menthol1", "raceOther:Only.Menthol1", 
  "inc2:Only.Menthol1", "inc3:Only.Menthol1", 
  "inc4:Only.Menthol1", "inc5:Only.Menthol1", 
  "edu_merged2:Only.Menthol1", "edu_merged3:Only.Menthol1", 
  # NMR
  "sex_ps2:NMR", "age_ps:NMR", "cpd_ps:NMR", 
  "NMR:readiness", "ftcd_score:NMR"
)

train_variables_dummy_include = train_variables_dummy_full_interactions[,train_variables_dummy_include_names]
test_variables_dummy_include = test_variables_dummy_full_interactions[,train_variables_dummy_include_names]


# Set penalty factors to enforce keeping Var and BA
# Initialize penalty factors to 1 for all variables
penalty_factors <- rep(1, ncol(train_variables_dummy_include))

# Identify columns corresponding exactly to "Var1" and "BA1" (not their interactions)
var1_col <- grep("^Var1$", colnames(train_variables_dummy_include))
ba1_col <- grep("^BA1$", colnames(train_variables_dummy_include))

penalty_factors[c(var1_col, ba1_col)] <- 0
names(penalty_factors) <- colnames(train_variables_dummy_include)

# Fit lasso
set.seed(2024)
lasso_model <- cv.glmnet(as.matrix(train_variables_dummy_include), train_outcome,
                         penalty.factor = penalty_factors,
                         alpha = 1, family = "binomial")

# Extract coefficients at the optimal lambda (best_lambda)
best_lambda_lasso <- lasso_model$lambda.min
#remove intercept
optimal_coefs_lasso <- as.numeric(coef(lasso_model, s = best_lambda_lasso)[-1])
coef_names_lasso <- rownames(coef(lasso_model, s = best_lambda_lasso))[-1]  

result_table_lasso <- data.frame(
  variable = coef_names_lasso,
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


# Best subset 

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


# train_bestglm = data.frame(train_variables_dummy_include, abst = train_outcome)
# enforced_variables <- c("Var1", "BA1")  # Variables you want to enforce
# 
# # Create x.fixed matrix for enforced variables
# x.fixed <-train_bestglm[,enforced_variables, drop = FALSE]
# 
# bestglm_model <- bestglm(
#   Xy = train_bestglm,
#   family = binomial,
#   IC = "BIC",               
#   nvmax = NULL              
#   #x.fixed = x.fixed          # Enforces Var1 and BA1 to be included in all models
# )
# 
# # View the best model
# print(bestglm_model$BestModel)

# PHP2550 Project 2: A regression data analysis

## Smoking cessation in adults with major depressive disorder (MDD)

## Objectives

Building on a previous randomized, placebo-controlled study exploring factors influencing smoking cessation among adults with major depressive disorder (MDD), this project reexamined data from the same trial to accomplish the following two main objectives: 

1) to identify baseline variables that may moderate the impact of behavioral treatments on end-of-treatment (EOT) smoking abstinence,  

2) to evaluate baseline variables as predictors of abstinence outcomes, accounting for the effects of both behavioral treatments and pharmacotherapy. 

## Methods

We applied logistic regression to model the binary smoking abstinence outcome and implemented three selection methods to identify moderating and predictive variables: bidirectional stepwise selection using AIC, elastic net regression with cross-validation, and best subset selection with Mallow’s Cp criterion. Interaction terms were carefully incorporated, especially for factors hypothesized to moderate behavioral activation effects on abstinence. *Objective 1* included a comprehensive range of baseline variables and interaction terms with BA, while *Objective 2* focused solely on baseline predictors without interactions to assess their independent predictive effects. Calibration and discrimination metrics were used to evaluate model performance.           

## Results and conclusion

Our analysis identified several key baseline characteristics as potential moderators and predictors for smoking cessation among individuals with MDD. Interactions between BA and FTCD score, Nicotine Metabolism Ratio (NMR), readiness to quit, MDD history, and anhedonia suggested nuanced influences on cessation, potentially acting as moderators of treatment effects. For predictor effects, variables such as education level, race, FTCD score, and NMR had certain impacts on odds of abstinence. In terms of model performance, Elastic Net achieved the best calibration metrics, with the lowest Brier score and calibration error, whereas bidirectional stepwise and best subset exhibited slightly higher AUC values, reflecting marginally better discrimination.

## Directory Structure

**`0-american-statistical-association.csl`**: ASA citation style language file for citation formatting in the report file.

**`0-references.bib`**: the article bibliography for the main report.

**`1-model-selection.R**: contains preliminary and supplementary model selection testing that were not necessarily included in the main report.

**`2-PHP2550-Project2Main.Rmd`**: the main report Rmarkdown.

**`2-PHP2550-Project2Main.pdf`**: the knitted report as .pdf.

## Dependencies

The following packages were used in this analysis:

1)  Data extraction, manipulation, and imputation: `tidyverse`, `MASS`, `mice`

2)  Table formatting: `gt`, `gtsummary`, `knitr`, `kableExtra`

3)  Data visualization: `ggplot2`, `patchwork`, `grid`, `gridExtra`, `patchwork`,`naniar`, `pROC`, `RColorBrewer`, `cowplot`

4)  Statistical analyses: `glmnet`, `bestglm`, `leaps`, `glmulti`

5)  Citations: `knitcitations`

**Contributor**: Yanwei (Iris) Tong

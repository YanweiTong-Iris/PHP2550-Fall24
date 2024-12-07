# PHP2550 Project 3: A simulation study

## Objectives
Our study aims to determine the optimal number of clusters ($G$) and observations per cluster ($R$) under a fixed budget ($B$) to maximize the efficiency of treatment effect estimates in a cluster randomized trial. The budget constraint includes a higher cost for the initial observation in each cluster ($c_1$) compared to subsequent observations ($c_2$, where $c_2 < c_1$), simulating a realistic cost structure in study designs. We conduct simulations to explore different parameter settings, including cost ratios ($c_1/c_2$), within-cluster and between-cluster variances, and outcome distributions (Normal vs. Poisson). The goal is to find the configuration that yields the most precise estimate of the treatment effect while maintaining an efficient use of resources.
  
## Methods
In this study, we conducted a series of simulation experiments to investigate the impact of different design choices on the efficiency of treatment effect estimates under a fixed budget constraint ($B$). The number of clusters ($G$), was systematically iterated/varied to explore their effect on the variance of the estimated treatment effect ($\hat{\beta}$). For each value of $G$, the maximum feasible value of observations per cluster ($R$) was determined to remain within budget. We then simulated data 100 times for each combination of $G$ and $R$ and fitted hierarchical models to obtain estimates of $\hat{\beta}$. Additionally, we examined the influence of other parameters, such as within-cluster variance ($\sigma^2$) for Y~Normal, between-cluster variance ($\gamma^2$) for both Y~Normal and Poisson, and cost ratio structures $c_1/c_2$ to investigate their impacts on finding the optimal design. Key performance measures included variance, empirical bias, MSE, coverage probability, and power of the estimated treatment effects. This allowed us to derive insights into the trade-offs between cluster size and cluster composition for efficient resource allocation.    

## Results and conclusion

The simulation results showed that the optimal $G$ and $R$ are influenced by the relative costs ($c_1/c_2$) and the variance components ($\gamma^2$, $\sigma^2$). As $c_1$ decreased or $c_2$ increased, the optimal design favored more clusters with fewer observations per cluster. Reducing between-cluster variability ($\gamma^2$) led to a preference for fewer clusters, while increasing within-cluster variability ($\sigma^2$) required more clusters to minimize variance. The consistent finding across different scenarios was that minimizing the variance of the treatment effect estimate led to consistently high coverage probability and power, highlighting the robustness of the chosen optimal designs.

## Directory Structure

**`0-american-statistical-association.csl`**: ASA citation style language file for citation formatting in the report file.

**`0-references.bib`**: the article bibliography for the main report.

**`PHP2550-Project3Main.Rmd`**: the main report Rmarkdown.

**`PHP2550-Project3Main.pdf`**: the knitted report as .pdf.

**`simulated_data`**: the sub-folder where the simulated data was stored.

**`results`**: the sub-folder where the results were stored.

## Dependencies

The following packages were used in this analysis:

1)  Data extraction, manipulation, and imputation: `tidyverse`

2)  Table formatting: `gt`, `gtsummary`, `knitr`, `kableExtra`

3)  Data visualization: `ggplot2`, `patchwork`, `grid`, `gridExtra`, `egg`, `RColorBrewer`, `cowplot`

4)  Statistical analyses: `lme4`

5)  Citations: `knitcitations`

**Contributor**: Yanwei (Iris) Tong

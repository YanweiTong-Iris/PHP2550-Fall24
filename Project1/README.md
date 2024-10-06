# PHP2550 Project 1: An exploratory data analysis

## IMPACT OF WEATHER ON MARATHON PERFORMANCE ACROSS AGE AND GENDER

## Overview

This exploratory data analysis focused on the following 3 aims:

1.  Examine effects of increasing age on marathon performance in men and women;

2.  Explore the impact of environmental conditions on marathon performance, and whether the impact differs across age and gender;

3.  Identify the environmental parameters (WBGT, Flag conditions, temperature, air pollution etc) that have the largest impact on marathon performance.

## Abstract

## Directory Structure

**`american-statistical-association.csl`**: ASA citation style language file for citation formatting in the report file.

**`0-references.bib`**: the article bibliography for the main report.

**`1-aqi.R`**: to extract ambient air quality data from the US EPA Air Quality System Data Mart API for all relevant marathon dates and locations.

**`2-EDA.R`**: contains preliminary and supplementary exploratory data analyses that were not necessarily included in the main report.

**`3-PHP2550-Project1Main.Rmd`**: the main EDA report Rmarkdown.

**`3-PHP2550-Project1Main.pdf`**: the knitted report as .pdf.

## Dependencies

The following packages were used in this analysis:

1)  Data extraction and manipulation: `tidyverse`, `RAQSAPI`

2)  Table formatting: `gt`, `gtsummary`, `knitr`, `kableExtra`

3)  Data visualization: `ggplot2`, `patchwork`, `naniar`

4)  Statistical analyses: `npreg`, `mosaic`, `mgcv`

5)  Citations: `knitcitations`

**Contributor**: Yanwei (Iris) Tong

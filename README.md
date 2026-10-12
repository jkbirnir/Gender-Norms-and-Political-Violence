# Gender Norms and Attitudes about Political Violence

Replication materials for the analysis of gender norms and support for
political violence using a 2022 survey of 1,289 Indonesian adults.

## Requirements

R (>= 4.1). Install the required packages:

```r
install.packages(c("here", "tidyverse", "caret", "ranger", "MASS",
                   "survey", "psych", "sandwich", "lmtest",
                   "stargazer", "knitr", "kableExtra"))
```

## Reproduce

From the repository root:

```r
source("run_all.R")
```

This fits the models and writes every table to `tables/` as `.tex`.

## Layout

```
run_all.R                 Runs the analysis and builds the tables
R/01_prepare_data.R       How the analysis data was built (raw data not shared)
R/02_analysis.R           Random forest, OLS models, robustness checks
R/03_tables.R             All tables
data/analysis_data.csv    De-identified survey responses + religion indicators
data/religion_population_2024.csv   Population by religion, used for weighting
tables/                   Output
```

The raw individual survey includes identifiers (panel ID, device, location,
timestamps) and is not shared. `R/01_prepare_data.R` documents how the
de-identified analysis file was produced from it.
# 00_master.R
# Run this file to reproduce all results presented in Birnir and Overos 2026.
# Each script depends on objects produced by the previous one.

rm(list = ls())
install.packages("here")

library(here)

source(here("analysis", "01_load.R"))       # → jakpatStacked, parallel cluster
source(here("analysis", "02_rf_prep.R"))    # → rf_dat, train, test, fitCtrl, rfGrid
source(here("analysis", "03_rf_model.R"))   # → rf.regQ51, varimpQ51, vimp_df, top_vars
source(here("analysis", "04_ols_models.R")) # → all lm objects
source(here("analysis", "05_output.R"))     # → all tables and figures


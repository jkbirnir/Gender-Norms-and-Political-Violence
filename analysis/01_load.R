# 01_load.R
# Requires: jakpatStacked.RData in the expected directory
# Produces: jakpatStacked, parallel cluster registered

pacman::p_load(mlbench, parallel, doParallel, foreach,
               haven, MASS, caret, ranger, pROC, party,
               tidyverse, ggraph, igraph, rpart.plot,
               here, rio, stargazer, gmodels, RCPA3,
               vip, lmtest, sandwich, knitr, kableExtra)

options(tibble.print_max = Inf)
options(tibble.width = Inf)

rf_dat <- import(here("data", "rf_dat.RData"))

cl <- makeCluster(detectCores() - 1, setup_timeout = 0.5)
registerDoParallel(cl)

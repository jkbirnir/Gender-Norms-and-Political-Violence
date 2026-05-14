# 03_rf_prep.R
# Produces: train, test, fitCtrl, rfGrid
set.seed(1985)

trainIndex <- createDataPartition(rf_dat$political_violence_flip,
                                  p = 0.7, list = FALSE, times = 1)
train <- rf_dat[trainIndex, ]
test  <- rf_dat[-trainIndex, ]

fitCtrl <- trainControl(method          = "repeatedcv",
                        number          = 5,
                        repeats         = 2,
                        summaryFunction = defaultSummary,
                        search          = "random",
                        allowParallel   = TRUE)

rfGrid <- expand.grid(mtry           = c(2:5),
                      min.node.size  = c(1, 5),
                      splitrule      = c("gini", "extratrees"))
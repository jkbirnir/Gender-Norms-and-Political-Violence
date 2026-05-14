# 04_rf_model.R
# Produces: rf.regQ51, varimpQ51, vimp_df, top_vars

rf.regQ51 <- train(political_violence_flip ~ .,
                   data       = train,
                   method     = "ranger",
                   trControl  = fitCtrl,
                   tuneGrid   = rfGrid,
                   importance = "impurity",
                   num.trees  = 500,
                   metric     = "RMSE",
                   verbose    = FALSE,
                   na.action  = na.omit)

rf.regQ51

plot(rf.regQ51)

varimpQ51 <- varImp(rf.regQ51)
varimpQ51
plot(varimpQ51)
vip(rf.regQ51$finalModel, num_features = 20)

vimp_df <- varimpQ51$importance %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(Overall))

print(vimp_df)

mean_imp <- mean(vimp_df$Overall)
top_vars  <- vimp_df %>% filter(Overall >= mean_imp) %>% pull(variable)

cat("Variables above mean importance:\n")
print(top_vars)

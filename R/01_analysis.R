library(here)
library(tidyverse)
library(caret)
library(ranger)
library(MASS)
library(survey)
library(psych)
library(sandwich)
library(lmtest)

# HC1 robust standard errors
robust_se <- function(m) sqrt(diag(vcovHC(m, type = "HC1")))

norm_vars <- c("male_breadwinner", "dual_income",
               "women_political_participation", "men_suppress_emotions",
               "male_assault_justified", "women_defer_vote_to_man",
               "women_endure_violence_family")

# Data
dat <- read.csv(here("data", "analysis_data.csv"))

# Random forest
rf_input <- dat %>%
  dplyr::select(political_violence_flip,
                gender, age, education, monthly_spending, ses,
                all_of(norm_vars),
                econ_nat, econ_household, pol_nat, pol_province,
                voted_presidential_2019, voted_legislative_2019,
                ever_ran_for_office, ever_campaigned_for_other,
                political_interest, demonstrations_area,
                ever_womens_rights_org, ever_peace_movement,
                ever_violent_demo_bin, ever_strike_boycott_bin,
                muslim, christian)

set.seed(1985)
idx   <- createDataPartition(rf_input$political_violence_flip, p = 0.7, list = FALSE)
train <- rf_input[idx, ]
test  <- rf_input[-idx, ]

fitCtrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
rfGrid  <- expand.grid(mtry = 2:5, min.node.size = c(1, 5),
                       splitrule = c("variance", "extratrees"))

rf_impurity <- train(political_violence_flip ~ ., data = train, method = "ranger",
                     trControl = fitCtrl, tuneGrid = rfGrid,
                     importance = "impurity", num.trees = 500,
                     na.action = na.omit)
set.seed(1985)
rf_perm <- train(political_violence_flip ~ ., data = train, method = "ranger",
                 trControl = fitCtrl, tuneGrid = rfGrid,
                 importance = "permutation", num.trees = 500,
                 na.action = na.omit)

# Variable importance, scaled to 0-100
scale100 <- function(v) 100 * v / max(v)
vimp_df <- varImp(rf_impurity)$importance %>%
  rownames_to_column("variable") %>% arrange(desc(Overall)) %>%
  mutate(Overall = scale100(Overall))
vimp_perm_df <- varImp(rf_perm)$importance %>%
  rownames_to_column("variable") %>% arrange(desc(Overall)) %>%
  transmute(variable, perm_importance = scale100(Overall))
mean_imp <- mean(vimp_df$Overall)

# Predictive performance
rf_cv_perf <- rf_impurity$results %>%
  semi_join(rf_impurity$bestTune, by = c("mtry", "splitrule", "min.node.size")) %>%
  dplyr::select(RMSE, Rsquared, MAE)
test_cc <- test %>% tidyr::drop_na()
rf_test_perf <- postResample(predict(rf_impurity, newdata = test_cc),
                             test_cc$political_violence_flip)

# Derived variables ---------------------------------------------------
# Ordered outcome for the ordered logit; religion weights for weighting.
dat$political_violence_ord <- factor(dat$political_violence_flip, 1:4,
                                     c("Never", "Rarely", "Sometimes", "Always"),
                                     ordered = TRUE)

pop <- read.csv(here("data", "religion_population_2024.csv"))
pop_shares <- setNames(pop$population / sum(pop$population), pop$religion_group)
dat <- dat %>%
  mutate(religion_group = case_when(muslim == 1 ~ "muslim",
                                    christian == 1 ~ "christian",
                                    TRUE ~ "other"))
weight_lookup <- dat %>% count(religion_group) %>%
  mutate(sample_share = n / sum(n),
         pop_share = pop_shares[religion_group],
         ps_weight = pop_share / sample_share)
dat <- dat %>%
  left_join(dplyr::select(weight_lookup, religion_group, ps_weight),
            by = "religion_group")

# OLS models ----------------------------------------------------------
# Bivariate
m_bv1 <- lm(political_violence_flip ~ male_breadwinner,              data = dat)
m_bv2 <- lm(political_violence_flip ~ dual_income,                   data = dat)
m_bv3 <- lm(political_violence_flip ~ women_political_participation, data = dat)
m_bv4 <- lm(political_violence_flip ~ men_suppress_emotions,         data = dat)
m_bv5 <- lm(political_violence_flip ~ male_assault_justified,        data = dat)
m_bv6 <- lm(political_violence_flip ~ women_defer_vote_to_man,       data = dat)
m_bv7 <- lm(political_violence_flip ~ women_endure_violence_family,  data = dat)

# Norms only and full model
m_gender <- lm(political_violence_flip ~ male_breadwinner + dual_income +
                 women_political_participation + men_suppress_emotions +
                 male_assault_justified + women_defer_vote_to_man +
                 women_endure_violence_family, data = dat)

m_full <- lm(political_violence_flip ~ male_breadwinner + dual_income +
               women_political_participation + men_suppress_emotions +
               male_assault_justified + women_defer_vote_to_man +
               women_endure_violence_family +
               gender + age + education + ses +
               ever_violent_demo_bin + ever_strike_boycott_bin +
               political_interest + pol_nat + econ_nat, data = dat)

# Gender interactions (H3 hostile, H4 benevolent)
h3a_no_controls <- lm(political_violence_flip ~ women_endure_violence_family * gender +
                        women_defer_vote_to_man * gender, data = dat)
h3a_controls    <- lm(political_violence_flip ~ women_endure_violence_family * gender +
                        women_defer_vote_to_man * gender +
                        age + education + ses + political_interest + pol_nat + econ_nat,
                      data = dat)
h3b_no_controls <- lm(political_violence_flip ~ male_assault_justified * gender, data = dat)
h3b_controls    <- lm(political_violence_flip ~ male_assault_justified * gender +
                        age + education + ses + political_interest + pol_nat + econ_nat,
                      data = dat)
h4a_no_controls <- lm(political_violence_flip ~ male_breadwinner * gender, data = dat)
h4a_controls    <- lm(political_violence_flip ~ male_breadwinner * gender +
                        age + education + ses + political_interest + pol_nat + econ_nat,
                      data = dat)
h4b_no_controls <- lm(political_violence_flip ~ men_suppress_emotions * gender, data = dat)
h4b_controls    <- lm(political_violence_flip ~ men_suppress_emotions * gender +
                        age + education + ses + political_interest + pol_nat + econ_nat,
                      data = dat)

# Gender predicting each norm
s1 <- lm(male_breadwinner ~ gender, data = dat)
s2 <- lm(dual_income ~ gender, data = dat)
s3 <- lm(women_political_participation ~ gender, data = dat)
s4 <- lm(men_suppress_emotions ~ gender, data = dat)
s5 <- lm(male_assault_justified ~ gender, data = dat)
s6 <- lm(women_defer_vote_to_man ~ gender, data = dat)
s7 <- lm(women_endure_violence_family ~ gender, data = dat)

# Robustness ----------------------------------------------------------
# Ordered logit
m_ord_gender <- polr(political_violence_ord ~ male_breadwinner + dual_income +
                       women_political_participation + men_suppress_emotions +
                       male_assault_justified + women_defer_vote_to_man +
                       women_endure_violence_family, data = dat, Hess = TRUE)
m_ord_full   <- polr(political_violence_ord ~ male_breadwinner + dual_income +
                       women_political_participation + men_suppress_emotions +
                       male_assault_justified + women_defer_vote_to_man +
                       women_endure_violence_family +
                       gender + age + education + ses +
                       ever_violent_demo_bin + ever_strike_boycott_bin +
                       political_interest + pol_nat + econ_nat, data = dat, Hess = TRUE)

# Religion controls
m_full_religion <- lm(political_violence_flip ~ male_breadwinner + dual_income +
                        women_political_participation + men_suppress_emotions +
                        male_assault_justified + women_defer_vote_to_man +
                        women_endure_violence_family +
                        gender + age + education + ses +
                        ever_violent_demo_bin + ever_strike_boycott_bin +
                        political_interest + pol_nat + econ_nat +
                        muslim + christian, data = dat)

# Post-stratification weighting on religion
svy_dat     <- svydesign(ids = ~1, weights = ~ps_weight, data = dat)
m_wt_gender <- svyglm(political_violence_flip ~ male_breadwinner + dual_income +
                        women_political_participation + men_suppress_emotions +
                        male_assault_justified + women_defer_vote_to_man +
                        women_endure_violence_family, design = svy_dat)
m_wt_full   <- svyglm(political_violence_flip ~ male_breadwinner + dual_income +
                        women_political_participation + men_suppress_emotions +
                        male_assault_justified + women_defer_vote_to_man +
                        women_endure_violence_family +
                        gender + age + education + ses +
                        ever_violent_demo_bin + ever_strike_boycott_bin +
                        political_interest + pol_nat + econ_nat, design = svy_dat)

# Factor structure of the norm items
norm_items <- dat %>% dplyr::select(all_of(norm_vars)) %>%
  mutate(across(everything(), as.numeric))
poly_cor <- polychoric(norm_items)$rho
fa_norms <- fa(norm_items, nfactors = 2, rotate = "oblimin", fm = "ml", cor = "poly")

# Benjamini-Hochberg FDR
norm_pvals <- function(m, name) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
  tibble(model = name, variable = rownames(ct),
         estimate = ct[, "Estimate"], p_value = ct[, "Pr(>|t|)"]) %>%
    filter(variable %in% norm_vars)
}
fdr_df <- bind_rows(
  bind_rows(lapply(list(m_bv1, m_bv2, m_bv3, m_bv4, m_bv5, m_bv6, m_bv7),
                   norm_pvals, name = "Bivariate")),
  norm_pvals(m_gender, "Gender Norms Only"),
  norm_pvals(m_full,   "Full OLS Model")) %>%
  mutate(p_bh = p.adjust(p_value, "BH"))

# Age eligibility: respondents aged 17 and older
m_full_age17 <- lm(political_violence_flip ~ male_breadwinner + dual_income +
                     women_political_participation + men_suppress_emotions +
                     male_assault_justified + women_defer_vote_to_man +
                     women_endure_violence_family +
                     gender + age + education + ses +
                     ever_violent_demo_bin + ever_strike_boycott_bin +
                     political_interest + pol_nat + econ_nat,
                   data = filter(dat, age >= 17))

# Full model plus the additional controls
m_full_rf <- lm(political_violence_flip ~ male_breadwinner + dual_income +
                  women_political_participation + men_suppress_emotions +
                  male_assault_justified + women_defer_vote_to_man +
                  women_endure_violence_family +
                  gender + age + education + ses +
                  ever_violent_demo_bin + ever_strike_boycott_bin +
                  political_interest + pol_nat + econ_nat +
                  ever_campaigned_for_other + ever_ran_for_office +
                  ever_womens_rights_org + demonstrations_area +
                  voted_presidential_2019 + voted_legislative_2019 +
                  ever_peace_movement + econ_household, data = dat)

# Distribution of the outcome
dv_dist <- dat %>% count(political_violence_flip) %>% mutate(percent = 100 * n / sum(n))

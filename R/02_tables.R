# Tables
# Writes every table in the paper and appendix to tables/ as .tex.
# Run after 02_analysis.R.

library(here)
library(stargazer)
library(knitr)
library(kableExtra)
library(dplyr)
library(tibble)

dir.create(here("tables"), showWarnings = FALSE)

# Readable labels for variable-name columns
labels <- c(
  male_breadwinner = "Male Breadwinner", dual_income = "Dual Income",
  women_political_participation = "Women in Politics",
  men_suppress_emotions = "Men Suppress Emotions",
  male_assault_justified = "Male Assault Justified",
  women_defer_vote_to_man = "Women Defer Vote to Man",
  women_endure_violence_family = "Women Endure Violence",
  genderMale = "Gender (Male)", gender = "Gender (Male)", age = "Age",
  education = "Education", ses = "Socio-Economic Status",
  monthly_spending = "Monthly Spending", political_interest = "Political Interest",
  pol_nat = "National Political Condition", pol_province = "Provincial Political Condition",
  econ_nat = "National Economic Condition", econ_household = "Household Economic Condition",
  demonstrations_area = "Demonstrations in Area",
  ever_campaigned_for_other = "Ever Campaigned for Other",
  ever_ran_for_office = "Ever Ran for Office",
  ever_womens_rights_org = "Ever Joined Women's Rights Org",
  ever_peace_movement = "Ever Joined Peace Movement",
  ever_violent_demo_bin = "Ever Joined Violent Demo",
  ever_strike_boycott_bin = "Ever Joined Strike/Boycott",
  voted_presidential_2019 = "Voted Presidential 2019",
  voted_legislative_2019 = "Voted Legislative 2019",
  muslim = "Muslim", christian = "Christian")
relabel <- function(v) ifelse(is.na(labels[v]), v, labels[v])

norm_labels <- c("Male Breadwinner", "Dual Income", "Women in Politics",
                 "Men Suppress Emotions", "Male Assault Justified",
                 "Women Defer Vote to Man", "Women Endure Violence")
ctrl_labels <- c("Gender", "Age", "Education", "SES", "Ever Joined Violent Demo",
                 "Ever Joined Strike/Boycott", "Political Interest",
                 "National Political Condition", "National Economic Condition")

# Bivariate models
stargazer(m_bv1, m_bv2, m_bv3, m_bv4, m_bv5, m_bv6, m_bv7,
  type = "latex", out = here("tables", "bivariate.tex"),
  se = lapply(list(m_bv1, m_bv2, m_bv3, m_bv4, m_bv5, m_bv6, m_bv7), robust_se),
  covariate.labels = paste0(norm_labels, " Norm"),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "HC1 robust SEs.", notes.append = TRUE)

# Norms only and full model
stargazer(m_gender, m_full,
  type = "latex", out = here("tables", "ols_full.tex"),
  se = lapply(list(m_gender, m_full), robust_se),
  column.labels = c("Gender Norms Only", "Full Model"),
  covariate.labels = c(paste0(norm_labels, " Norm"), ctrl_labels),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "HC1 robust SEs.", notes.append = TRUE)

# Gender predicting each norm
stargazer(s1, s2, s3, s4, s5, s6, s7,
  type = "latex", out = here("tables", "gender_on_norms.tex"),
  se = lapply(list(s1, s2, s3, s4, s5, s6, s7), robust_se),
  covariate.labels = "Male", dep.var.labels = norm_labels,
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "HC1 robust SEs.", notes.append = TRUE)

# Gender interaction models (coefficient names shown as-is to keep the
# interaction rows unambiguous)
int_table <- function(m_nc, m_c, out, title) {
  stargazer(m_nc, m_c, type = "latex", out = here("tables", out), title = title,
            se = lapply(list(m_nc, m_c), robust_se),
            column.labels = c("No Controls", "Controls"),
            dep.var.labels = "Tolerance for Political Violence",
            star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
            notes = "HC1 robust SEs.", notes.append = TRUE)
}
int_table(h3a_no_controls, h3a_controls, "h3a.tex", "H3a: Hostile Patriarchy by Gender")
int_table(h3b_no_controls, h3b_controls, "h3b.tex", "H3b: Violent Hypermasculinity by Gender")
int_table(h4a_no_controls, h4a_controls, "h4a.tex", "H4a: Benevolent Patriarchy by Gender")
int_table(h4b_no_controls, h4b_controls, "h4b.tex", "H4b: Benevolent Hypermasculinity by Gender")

# Ordered logit
stargazer(m_ord_gender, m_ord_full,
  type = "latex", out = here("tables", "ordered_logit.tex"),
  column.labels = c("Gender Norms Only", "Full Model"),
  covariate.labels = c(paste0(norm_labels, " Norm"), ctrl_labels),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Ordered logit coefficients; SEs in parentheses.", notes.append = TRUE)

# Religion controls
stargazer(m_full, m_full_religion,
  type = "latex", out = here("tables", "religion.tex"),
  se = lapply(list(m_full, m_full_religion), robust_se),
  column.labels = c("Full Model", "With Religion"),
  covariate.labels = c(paste0(norm_labels, " Norm"), ctrl_labels, "Muslim", "Christian"),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "HC1 robust SEs. Reference: other affiliations.", notes.append = TRUE)

# Post-stratification weights and weighted models
weight_lookup %>%
  dplyr::select(religion_group, n, sample_share, pop_share, ps_weight) %>%
  kable(format = "latex", booktabs = TRUE, digits = 3, linesep = "",
        caption = "Post-Stratification Weights by Religious Affiliation",
        col.names = c("Religion", "N", "Sample share", "Population share", "Weight")) %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  save_kable(here("tables", "weights.tex"))

stargazer(m_wt_gender, m_wt_full,
  type = "latex", out = here("tables", "weighted_models.tex"),
  column.labels = c("Gender Norms Only", "Full Model"),
  covariate.labels = c(paste0(norm_labels, " Norm"), ctrl_labels),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "Post-stratification weighted on religion.", notes.append = TRUE)

# Full model plus random-forest-identified controls
stargazer(m_full, m_full_rf,
  type = "latex", out = here("tables", "rf_controls.tex"),
  se = lapply(list(m_full, m_full_rf), robust_se),
  column.labels = c("Full Model", "+ RF Controls"),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "HC1 robust SEs.", notes.append = TRUE)

# Age eligibility
stargazer(m_full, m_full_age17,
  type = "latex", out = here("tables", "age_17plus.tex"),
  se = lapply(list(m_full, m_full_age17), robust_se),
  column.labels = c("All Respondents", "Age 17+"),
  covariate.labels = c(paste0(norm_labels, " Norm"), ctrl_labels),
  dep.var.labels = "Tolerance for Political Violence",
  star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("f", "ser"),
  notes = "HC1 robust SEs.", notes.append = TRUE)

# Outcome distribution
dv_dist %>%
  mutate(political_violence_flip = c("Never acceptable", "Rarely acceptable",
                                     "Sometimes acceptable", "Always acceptable")) %>%
  kable(format = "latex", booktabs = TRUE, digits = 1,
        caption = "Distribution of the Dependent Variable",
        col.names = c("Violence in politics is...", "N", "Percent")) %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  save_kable(here("tables", "dv_distribution.tex"))

# Random forest importance: impurity vs permutation
vimp_df %>%
  rename(impurity = Overall) %>%
  full_join(vimp_perm_df, by = "variable") %>%
  arrange(desc(impurity)) %>%
  filter(impurity >= mean_imp) %>%
  transmute(Variable = relabel(variable),
            Impurity = round(impurity, 2),
            Permutation = round(perm_importance, 2)) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "",
        caption = "Variable Importance: Impurity (Gini) vs. Permutation") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  save_kable(here("tables", "rf_importance.tex"))

# Random forest performance
tibble(Statistic = c("Cross-validated RMSE", "Cross-validated R2", "Cross-validated MAE",
                     "Test-set RMSE", "Test-set R2", "Test-set MAE"),
       Value = c(rf_cv_perf$RMSE, rf_cv_perf$Rsquared, rf_cv_perf$MAE,
                 rf_test_perf["RMSE"], rf_test_perf["Rsquared"], rf_test_perf["MAE"])) %>%
  kable(format = "latex", booktabs = TRUE, digits = 3,
        caption = "Random Forest Predictive Performance") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  save_kable(here("tables", "rf_performance.tex"))

# Factor loadings
fa_norms$loadings %>% unclass() %>% as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(variable = relabel(variable), across(where(is.numeric), ~ round(.x, 2))) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "",
        caption = "Exploratory Factor Analysis of Gender-Norm Items") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  save_kable(here("tables", "efa_loadings.tex"))

# Polychoric correlations
poly_cor %>% round(2) %>% as.data.frame() %>%
  setNames(relabel(colnames(poly_cor))) %>%
  mutate(Item = relabel(rownames(poly_cor)), .before = 1) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "",
        caption = "Polychoric Correlations Among Gender-Norm Items") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable(here("tables", "polychoric.tex"))

# Benjamini-Hochberg adjusted p-values
fdr_df %>%
  mutate(variable = relabel(variable)) %>%
  transmute(Model = model, Variable = variable,
            Estimate = round(estimate, 3),
            `Unadjusted p` = round(p_value, 3),
            `BH-adjusted p` = round(p_bh, 3)) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "",
        caption = "Norm Coefficients Under Benjamini-Hochberg Adjustment") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  save_kable(here("tables", "fdr.tex"))

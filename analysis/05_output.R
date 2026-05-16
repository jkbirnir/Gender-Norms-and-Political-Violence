# 06_output.R
# Produces: all stargazer and kable tables

dir.create(here("tables"), showWarnings = FALSE)

# Bivariate table
stargazer(m_bv1, m_bv2, m_bv3, m_bv4, m_bv5, m_bv6, m_bv7,
     type = "latex",
     out = here("tables", "bivariate_models.tex"),
     title = "Stage 1: Bivariate Models — Gender Role Variables",
     covariate.labels = c("Male breadwinner norm", "Dual income norm",
                "Women in politics", "Men suppress emotions",
                "Male assault justified", "Women defer vote to man",
                "Women endure violence "),
     dep.var.labels = "View: Political Violence Acceptable",
     star.cutoffs = c(0.05, 0.01, 0.001),
     omit.stat = c("f", "ser"))

# Gender norms + full model table
stargazer(m_gender, m_full,
     type  = "latex",
     out = here("tables", "ols_full_model.tex"),
     header = FALSE,
     title  = "OLS Regression: Gender Role Attitudes and Tolerance for Political Violence",
     label  = "tab:ols",
     dep.var.labels = "Tolerance for Political Violence",
     column.labels  = c("Gender Norms Only", "Full Model"),
     covariate.labels = c("Male Breadwinner Norm", "Dual Income Norm",
                "Women in Politics", "Men Suppress Emotions",
                "Male Assault Justified", "Women Defer Vote to Man",
                "Women Endure Violence ",
                "Gender", "Age", "Education", "SES",
                "Ever Joined Violent Demo", "Ever Joined Strike/Boycott",
                "Political Interest", "National Political Condition",
                "National Economic Condition"),
     star.cutoffs = c(0.05, 0.01, 0.001),
     omit.stat   = c("f", "ser"),
     notes     = "Robust standard errors in parentheses.",
     notes.append = FALSE)

# Gender interaction tables (H3/H4)
# H3a: Hostile Patriarchy x Gender
stargazer(h3a_no_controls, h3a_controls,
     type = "latex",
     out = here("tables", "h3a_hostile_patriarchy.tex"),
     header = FALSE,
     title = "H3a: Hostile Patriarchy and Tolerance for Political Violence by Gender",
     label = "tab:h3a",
     column.labels = c("No Controls", "Controls"),
     dep.var.labels = "Tolerance for Political Violence",
     order = c("women_endure_violence_family$",
               "genderMale",
               "women_endure_violence_family:genderMale",
               "age", "education", "ses",
               "political_interest", "pol_nat", "econ_household"),
     covariate.labels = c("Women Endure Violence",
                          "Male",
                          "Women Endure Violence × Male",
                          "Age", "Education", "SES",
                          "Political Interest",
                          "National Political Condition",
                          "National Economic Condition"),
     star.cutoffs = c(0.05, 0.01, 0.001),
     omit.stat = c("f", "ser"),
     notes = "Robust standard errors in parentheses.",
     notes.append = FALSE)

# H3b: Violent Hypermasculinity x Gender
stargazer(h3b_no_controls, h3b_controls,
     type = "latex",
     out = here("tables", "h3b_violent_hypermasculinity.tex"),
     header = FALSE,
     title = "H3b: Violent Hypermasculinity and Tolerance for Political Violence by Gender",
     label = "tab:h3b",
     column.labels = c("No Controls", "Controls"),
     dep.var.labels = "Tolerance for Political Violence",
     order = c("male_assault_justified$",
               "genderMale",
               "male_assault_justified:genderMale",
               "age", "education", "ses",
               "political_interest", "pol_nat", "econ_household"),
     covariate.labels = c("Male Assault Justified",
                          "Male",
                          "Male Assault Justified × Male",
                          "Age", "Education", "SES",
                          "Political Interest",
                          "National Political Condition",
                          "National Economic Condition"),
     star.cutoffs = c(0.05, 0.01, 0.001),
     omit.stat = c("f", "ser"),
     notes = "Robust standard errors in parentheses.",
     notes.append = FALSE)

# H4a: Benevolent Patriarchy x Gender
stargazer(h4a_no_controls, h4a_controls,
     type = "latex",
     out = here("tables", "h4a_benevolent_patriarchy.tex"),
     header = FALSE,
     title = "H4a: Benevolent Patriarchy and Tolerance for Political Violence by Gender",
     label = "tab:h4a",
     column.labels = c("No Controls", "Controls"),
     dep.var.labels = "Tolerance for Political Violence",
     order = c("male_breadwinner$",
               "women_defer_vote_to_man$",
               "genderMale",
               "male_breadwinner:genderMale",
               "women_defer_vote_to_man:genderMale",
               "age", "education", "ses",
               "political_interest", "pol_nat", "econ_household"),
     covariate.labels = c("Male Breadwinner Norm",
                          "Women Defer Vote to Man",
                          "Male",
                          "Male Breadwinner × Male",
                          "Women Defer Vote × Male",
                          "Age", "Education", "SES",
                          "Political Interest",
                          "National Political Condition",
                          "National Economic Condition"),
     star.cutoffs = c(0.05, 0.01, 0.001),
     omit.stat = c("f", "ser"),
     notes = "Robust standard errors in parentheses.",
     notes.append = FALSE)

# H4b: Benevolent Hypermasculinity x Gender
stargazer(h4b_no_controls, h4b_controls,
     type = "latex",
     out = here("tables", "h4b_benevolent_hypermasculinity.tex"),
     header = FALSE,
     title = "H4b: Benevolent Hypermasculinity and Tolerance for Political Violence by Gender",
     label = "tab:h4b",
     column.labels = c("No Controls", "Controls"),
     dep.var.labels = "Tolerance for Political Violence",
     order = c("men_suppress_emotions$",
               "genderMale",
               "men_suppress_emotions:genderMale",
               "age", "education", "ses",
               "political_interest", "pol_nat", "econ_household"),
     covariate.labels = c("Men Suppress Emotions",
                          "Male",
                          "Men Suppress Emotions × Male",
                          "Age", "Education", "SES",
                          "Political Interest",
                          "National Political Condition",
                          "National Economic Condition"),     
     star.cutoffs = c(0.05, 0.01, 0.001),
     omit.stat = c("f", "ser"),
     notes = "Robust standard errors in parentheses.",
     notes.append = FALSE)

# Variable importance table
vimp_df %>%
 kable(format  = "latex",
    booktabs = TRUE,
    digits  = 2,
    caption  = "Top 20 Predictors of Attitudes Toward Political Violence (Random Forest)",
    label   = "tab:vip",
    col.names = c("Variable", "Importance Score")) %>%
 kable_styling(latex_options = c("hold_position"), full_width = FALSE) %>%
 column_spec(1, width = "1cm") %>%
 column_spec(2, width = "7cm") %>%
 column_spec(3, width = "3cm") %>%
 footnote(general = "Variable importance scores are based on impurity reduction across 500 trees using the ranger algorithm. Scores are normalized to a 0--100 scale relative to the highest-ranked predictor. All variables shown scored above the mean importance threshold.",
      general_title   = "Note:",
      footnote_as_chunk = TRUE,
      threeparttable  = TRUE) %>% 
  save_kable(here("tables", "vip_table.tex"))

# Supplementary gender models (s1-7):

stargazer(s1,s2,s3,s4,s5,s6,s7,
          type = "latex",
          out = here("tables", "supplementary_tables.tex"),
          title = "Impact of gender on agreement with Patriarchal Values",
          dep.var.labels = c("Male breadwinner", "Dual income",
                             "Women in politics", "Men suppress emotions",
                             "Assault justified", "Women defer vote",
                             "Endure violence "),
          covariate.labels = "Male",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"))


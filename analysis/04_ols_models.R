# 05_ols_models.R
# Produces: m_bv1–m_bv7, m_gender, m_full, h3a/h3b/h4a/h4b models

# Bivariate models
m_bv1 <- lm(political_violence_flip ~ male_breadwinner,             data = rf_dat)
m_bv2 <- lm(political_violence_flip ~ dual_income,                  data = rf_dat)
m_bv3 <- lm(political_violence_flip ~ women_political_participation, data = rf_dat)
m_bv4 <- lm(political_violence_flip ~ men_suppress_emotions,        data = rf_dat)
m_bv5 <- lm(political_violence_flip ~ male_assault_justified,       data = rf_dat)
m_bv6 <- lm(political_violence_flip ~ women_defer_vote_to_man,      data = rf_dat)
m_bv7 <- lm(political_violence_flip ~ women_endure_violence_family, data = rf_dat)

# Gender norms only
m_gender <- lm(political_violence_flip ~ male_breadwinner +
                 dual_income +
                 women_political_participation +
                 men_suppress_emotions +
                 male_assault_justified +
                 women_defer_vote_to_man +
                 women_endure_violence_family,
               data = rf_dat)

# Full model with controls
m_full <- lm(political_violence_flip ~ male_breadwinner +
               dual_income +
               women_political_participation +
               men_suppress_emotions +
               male_assault_justified +
               women_defer_vote_to_man +
               women_endure_violence_family +
               gender + age + education + ses +
               ever_violent_demo_bin + ever_strike_boycott_bin +
               political_interest + pol_nat + econ_nat,
             data = rf_dat)

# H3a: Men ~ Hostile Patriarchy x Gender
h3a_no_controls <- lm(political_violence_flip ~ women_endure_violence_family * gender,
                      data = rf_dat)

h3a_controls    <- lm(political_violence_flip ~ women_endure_violence_family * gender +
                        age + education + ses + political_interest + pol_nat + econ_nat,
                      data = rf_dat)

# H3b: Men ~ Violent Hypermasculinity x Gender
h3b_no_controls <- lm(political_violence_flip ~ male_assault_justified * gender,
                      data = rf_dat)
h3b_controls    <- lm(political_violence_flip ~ male_assault_justified * gender +
                        age + education + ses + political_interest +
                        pol_nat + econ_nat,
                      data = rf_dat)

# H4a: Women ~ Benevolent Patriarchy x Gender
h4a_no_controls <- lm(political_violence_flip ~ male_breadwinner * gender +
                        women_defer_vote_to_man * gender,
                      data = rf_dat)

h4a_controls    <- lm(political_violence_flip ~ male_breadwinner * gender +
                        women_defer_vote_to_man * gender +
                        age + education + ses + political_interest +
                        pol_nat + econ_nat,
                      data = rf_dat)

# H4b: Women ~ Benevolent Hypermasculinity x Gender
h4b_no_controls <- lm(political_violence_flip ~ men_suppress_emotions * gender,
                      data = rf_dat)

h4b_controls    <- lm(political_violence_flip ~ men_suppress_emotions * gender +
                        age + education + ses + political_interest +
                        pol_nat + econ_nat,
                      data = rf_dat)

# Supplementary models showing gender predicting the patriarchal values
s1 <- lm(male_breadwinner ~ gender, data = rf_dat)

s2 <- lm(dual_income ~ gender, data = rf_dat)

s3 <- lm(women_political_participation ~ gender, data = rf_dat)

s4 <- lm(men_suppress_emotions ~ gender, data = rf_dat)

s5 <- lm(male_assault_justified ~ gender, data = rf_dat)

s6 <- lm(women_defer_vote_to_man ~ gender, data = rf_dat)

s7 <- lm(women_endure_violence_family ~ gender, data = rf_dat)

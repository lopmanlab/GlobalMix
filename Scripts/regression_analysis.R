rm(list=ls())

country = "Guatemala" # change to India, Mozambique, Pakistan
cty = "gt" # change to ind, moz, pak
hhmbr = "Non-member"

participants <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_participant_data_aim1.RDS")) 
if(hhmbr == "Non-member"){
  contacts <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_contact_data_aim1.RDS")) %>%
    filter(hh_membership == hhmbr)
}else{
  contacts <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_contact_data_aim1.RDS"))
}

## Average Daily Contacts Histograms ---------------------------------------

daily <- contacts %>%
  ungroup() %>%
  dplyr::group_by(rec_id, study_day) %>%
  summarise(daily_num_contacts = n()) %>%
  ungroup() %>%
  dplyr::group_by(rec_id) %>%
  summarise(avg_daily_contacts = mean(daily_num_contacts, na.rm = T))

contacts_daily = left_join(participants, daily, by = c("rec_id" = "rec_id")) %>%
  filter(!is.na(avg_daily_contacts))

if(hhmbr == ""){
  
}else{
  hhmbr = paste0("_", hhmbr)
}

r_lm_model <- lm(data = contacts_daily %>% filter(study_site == "Rural"), avg_daily_contacts ~ 1) # Linear model (mean only)
u_lm_model <- lm(data = contacts_daily %>% filter(study_site != "Rural"), avg_daily_contacts ~ 1) # Linear model (mean only)
r_pois_model <- glm(data = contacts_daily %>% filter(study_site == "Rural"), avg_daily_contacts ~ 1, family = poisson())  # Poisson model
u_pois_model <- glm(data = contacts_daily %>% filter(study_site != "Rural"), avg_daily_contacts ~ 1, family = poisson())  # Poisson model
r_nb_model <- glm.nb(data = contacts_daily %>% filter(study_site == "Rural"), avg_daily_contacts ~ 1) # Negative binomial model
u_nb_model <- glm.nb(data = contacts_daily %>% filter(study_site != "Rural"), avg_daily_contacts ~ 1) # Negative binomial model
r_mu_lm <- coef(r_lm_model)[1]
u_mu_lm <- coef(u_lm_model)[1]
r_mu_pois <- exp(coef(r_pois_model)[1])
u_mu_pois <- exp(coef(u_pois_model)[1])
r_mu_nb <- exp(coef(r_nb_model)[1])
u_mu_nb <- exp(coef(u_nb_model)[1])
r_theta_nb <- r_nb_model$theta
u_theta_nb <- u_nb_model$theta

y_vals <- 0:max(contacts_daily$avg_daily_contacts)  # range of y values to plot
df <- data.frame(
  y = y_vals,
  Rural_Normal = dnorm(y_vals, mean = r_mu_lm, sd = sd(residuals(r_lm_model))),
  Urban_Normal = dnorm(y_vals, mean = u_mu_lm, sd = sd(residuals(u_lm_model))),
  Rural_Poisson = dpois(y_vals, lambda = r_mu_pois),
  Urban_Poisson = dpois(y_vals, lambda = u_mu_pois),
  Rural_NegBinom = dnbinom(y_vals, size = r_theta_nb, mu = r_mu_nb),
  Urban_NegBinom = dnbinom(y_vals, size = u_theta_nb, mu = u_mu_nb)
)
df_long <- pivot_longer(df, cols = -y, names_to = "Model", values_to = "Density") %>%
  separate(Model, into = c("study_site", "Dist"), sep = "_")

#png(filename = paste0(here(), "/figs/", cty, hhmbr, "_hist.png" ), height = 800, width = 1200)
ggplot() +
  geom_histogram(data = contacts_daily, aes(x = avg_daily_contacts, y = ..density..), bins = length(unique((contacts_daily$avg_daily_contacts))), fill = "gray80", color = "black") +
  geom_line(data = df_long, aes(x = y, y = Density, color = Dist, lty=Dist), size = 1.3) +
  theme_bw() +
  facet_wrap(~study_site)+
  ggtitle("")+
  xlab("")+
  theme(text = element_text(size = 20))
#dev.off() 


# Specify comparison groups --------------------------------------------------

contacts_daily <- contacts_daily %>%
  mutate(age = factor(participant_age, levels = c("30-39y",
                                                  "<6mo",
                                                  "6-11mo",
                                                  "1-4y",
                                                  "5-9y", 
                                                  "10-19y",
                                                  "20-29y",
                                                  "40-59y",
                                                  "60+y"))) %>%
  mutate(occupation = ifelse(age %in% c("<6mo", "6-11mo", "1-4y", "5-9y"), "Child", as.character(occupation))) %>%
  mutate(occupation = factor(occupation, levels = c("Unemployed outside home", 
                                                    "Child", 
                                                    "Student",
                                                    "Semiskilled / skilled labor",
                                                    "Semiprofessional / professional", 
                                                    "Other"))) %>%
  mutate(high_educ = factor(high_educ, levels = c("Primary school",
                                                  "Currently enrolled in school",
                                                  "Secondary school",
                                                  "Some college or higher",
                                                  "None")))
  
# Average Daily Contacts Models ----------------------------------------------
contacts_daily$sex <- contacts_daily$participant_sex

table(contacts_daily$occupation)

urban_negbin_model <- glm.nb(avg_daily_contacts ~ age + sex + hh_size_cat + occupation + high_educ,
                       data = contacts_daily %>% filter(study_site == "Urban"))
coef_exp <- coef(urban_negbin_model)[!is.na(coef(urban_negbin_model))]
urban_daily_negbin <- data.frame(
  Term = names(coef_exp),
  RR =  exp(coef_exp),
  Lower_95_CI = exp(coef_exp - 1.96 * sqrt(diag(vcov(urban_negbin_model)))),
  Upper_95_CI = exp(coef_exp + 1.96 * sqrt(diag(vcov(urban_negbin_model))))
)
  #as.data.frame(summary(urban_negbin_model)$coefficients) %>%
  #mutate(Predictor = ifelse(`Pr(>|z|)` < 0.05, "*", "")) %>%
  

rural_negbin_model <- glm.nb(avg_daily_contacts ~ age + sex + hh_size_cat + occupation + high_educ,
                             data = contacts_daily %>% filter(study_site == "Rural"))
coef_exp <- coef(rural_negbin_model)[!is.na(coef(rural_negbin_model))]
rural_daily_negbin <- data.frame(
  Term = names(coef_exp),
  RR =  exp(coef_exp),
  Lower_95_CI = exp(coef_exp - 1.96 * sqrt(diag(vcov(rural_negbin_model)))),
  Upper_95_CI = exp(coef_exp + 1.96 * sqrt(diag(vcov(rural_negbin_model))))
)

urban_poisson_model <- glm(avg_daily_contacts ~ age + sex + hh_size_cat + occupation + high_educ,
                    data = contacts_daily  %>% filter(study_site == "Urban"),
                    family = poisson(link = "log"))
coef_exp <- coef(urban_poisson_model)[!is.na(coef(urban_poisson_model))]
se_est <- diag(vcov(urban_poisson_model))[!is.na(diag(vcov(urban_poisson_model)))]
urban_daily_poisson <- data.frame(
  Term = names(coef_exp),
  RR =  exp(coef_exp),
  Lower_95_CI = exp(coef_exp - 1.96 * sqrt(se_est)),
  Upper_95_CI = exp(coef_exp + 1.96 * sqrt(se_est))
)

rural_poisson_model <- glm(avg_daily_contacts ~ age + sex + hh_size_cat + occupation + high_educ,
                           data = contacts_daily  %>% filter(study_site == "Rural"),
                           family = poisson(link = "log"))
coef_exp <- coef(rural_poisson_model)[!is.na(coef(rural_poisson_model))]
se_est <- diag(vcov(rural_poisson_model))[!is.na(diag(vcov(rural_poisson_model)))]
rural_daily_poisson <- data.frame(
  Term = names(coef_exp),
  RR =  exp(coef_exp),
  Lower_95_CI = exp(coef_exp - 1.96 * sqrt(se_est)),
  Upper_95_CI = exp(coef_exp + 1.96 * sqrt(se_est))
)

urban_linear_model <- glm(avg_daily_contacts ~ age + sex + hh_size_cat + occupation + high_educ,
                     data = contacts_daily  %>% filter(study_site == "Urban"),
                     family = gaussian(link = "identity"))
coef_exp <- coef(urban_linear_model)[!is.na(coef(urban_linear_model))]
se_est <- diag(vcov(urban_linear_model))[!is.na(diag(vcov(urban_linear_model)))]
urban_daily_linear <- data.frame(
  Term = names(coef_exp),
  RR =  exp(coef_exp),
  Lower_95_CI = exp(coef_exp - 1.96 * sqrt(se_est)),
  Upper_95_CI = exp(coef_exp + 1.96 * sqrt(se_est))
)

rural_linear_model <- glm(avg_daily_contacts ~ age + sex + hh_size_cat + occupation + high_educ,
                          data = contacts_daily  %>% filter(study_site == "Rural"),
                          family = gaussian(link = "identity"))
coef_exp <- coef(rural_linear_model)[!is.na(coef(rural_linear_model))]
se_est <- diag(vcov(rural_linear_model))[!is.na(diag(vcov(rural_linear_model)))]
rural_daily_linear <- data.frame(
  Term = names(coef_exp),
  RR =  exp(coef_exp),
  Lower_95_CI = exp(coef_exp - 1.96 * sqrt(se_est)),
  Upper_95_CI = exp(coef_exp + 1.96 * sqrt(se_est))
)

# Plot fitted

# Save Data --------------------------------------------------------------------

# saveRDS(contacts_daily, paste0(here(), "/data/","/", cty, hhmbr, "_contacts_daily.RDS"))
# 
# write.csv(urban_daily_linear, paste0(here(),"/data/","/", cty, hhmbr, "_urban_daily_linear.csv"))
# write.csv(rural_daily_linear, paste0(here(),"/data/","/", cty, hhmbr, "_rural_daily_linear.csv"))
# write.csv(urban_daily_poisson, paste0(here(),"/data/","/", cty, hhmbr, "_urban_daily_poisson.csv"))
# write.csv(rural_daily_poisson, paste0(here(),"/data/","/", cty, hhmbr, "_rural_daily_poisson.csv"))
# write.csv(urban_daily_negbin, paste0(here(),"/data/","/", cty, hhmbr, "_urban_daily_negbin.csv"))
# write.csv(rural_daily_negbin, paste0(here(),"/data/","/", cty, hhmbr, "_rural_daily_negbin.csv"))
 

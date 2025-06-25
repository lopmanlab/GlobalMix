#India
in.pa <- readRDS("./India/ind_participant_data_aim1.RDS")
in.co <- readRDS("./India/ind_contact_data_aim1.RDS")
in.we <- read.csv("./Other/ind_pop.csv")

# Read Prem data
## Contact data
p_contact <- read.csv("./Other/synthetic_contacts_2021.csv", header = T)
## Population data
pop <- read.csv("./Other/popage_total2020.csv")%>%
  rename(country = Region..subregion..country.or.area..,
         year = Reference.date..as.of.1.July.)%>%
  filter(country == "Mozambique"|country == "India"| country == "Guatemala"|country == "Pakistan")

#######################
# Calculation of weights
#######################

in.pa.we <- in.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<1y",
                                     participant_age == "6-11mo" ~ "<1y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

in.we <- in.we%>%
  left_join(in.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)

#######################
# TABLE 1 INPUTS
#######################

# Participant data characteristics
# Total number of participants
nrow(in.pa) #1244 participants

# By site
table(in.pa$study_site)
# By age and site
table(in.pa$participant_age, in.pa$study_site, useNA = 'always')
# By age
table(in.pa$participant_age, useNA = "always")
# By sex and site
table(in.pa$participant_sex, in.pa$study_site, useNA = 'always')
# By sex
table(in.pa$participant_sex, useNA = "always")

#Education
#restrict to non/kids only
in.pa %>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y")) -> in.pa.notkids
nrow(in.pa.notkids) #896
table(in.pa.notkids$high_educ, in.pa.notkids$study_site, useNA = 'always')
table(in.pa.notkids$high_educ, useNA = 'always')

#Occupation
#restrict to adults only
in.pa %>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y", "5-9y")) -> in.pa.adults
nrow(in.pa.adults) #770
table(in.pa.adults$occupation, in.pa.adults$study_site, useNA = 'always')
table(in.pa.adults$occupation, useNA = 'always')

#HH size by site
table(in.pa$hh_size_cat, in.pa$study_site, useNA = 'always')
table(in.pa$hh_size_cat, useNA = "always")

in.pa %>%
  group_by(study_site) %>% 
  summarise(mean = mean(hh_size, na.rm = T), 
            median = median(hh_size, na.rm = T), 
            q = list(quantile(hh_size, na.rm = T))) %>%
  unnest_wider(q)

median(in.pa$hh_size, na.rm = T)
quantile(in.pa$hh_size, na.rm = T)

#Number of generations and families in households
in.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(study_site, hh.gens) %>%
  summarize(freq = n())

in.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(hh.gens) %>%
  summarize(freq = n())

in.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(study_site, hh.multfams) %>%
  summarize(freq = n()) 

in.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(hh.multfams) %>%
  summarize(freq = n()) 

#####################
# Join contact with participant data to assess contact patterns by participant characteristics
#####################

# add participant data to contact data
in.co.pa.full <-  full_join(in.co, in.pa, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(contact, name = "num_contacts")

# Merge contact counts with participant data
in.co.pa <- left_join(in.pa, in.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))

in.co.pa.age <- in.co.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<1y",
                                     participant_age == "6-11mo" ~ "<1y",
                                     TRUE ~ participant_age))%>%
  left_join(in.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))

# For weekday/weekend count
in.co.ed <- in.co%>%
  mutate(dayofweek = ifelse(weekdays(survey_date) %in% c('Saturday', 'Sunday'), 
                            'Weekend', 'Weekday'))
in.co.pa.wd <- full_join(in.pa, in.co.ed, by = c("rec_id", "study_site"))%>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>%
  group_by(rec_id, study_day, dayofweek)%>%
  summarise(num_contacts = sum(contact))%>%
  full_join(in.pa, by = "rec_id")%>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<1y",
                                     participant_age == "6-11mo" ~ "<1y",
                                     TRUE ~ participant_age))%>%
  left_join(in.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))


#######################
# TABLE 2 INPUTS
#######################

# Contact overall
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and age (No weighting)
in.co.pa %>%
  group_by(study_site, participant_age) %>%
  summarise(mean = round(mean(num_contacts/2, na.rm = T), 1), 
            sd = sd(num_contacts/2, na.rm = T),
            median = median(num_contacts/2, na.rm = T), 
            q = list(quantile(num_contacts/2, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

# Contact by age
in.co.pa %>%
  group_by(participant_age) %>%
  summarise(mean = round(mean(num_contacts/2, na.rm = T), 1), 
            sd = sd(num_contacts/2, na.rm = T),
            median = median(num_contacts/2, na.rm = T), 
            q = list(quantile(num_contacts/2, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

# Contact by site and sex
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, participant_sex)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by sex
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(participant_sex)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and hh size
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, hh_size_cat)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by hh size
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(hh_size_cat)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and education
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, high_educ)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by education
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(high_educ)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and occupation
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, occupation)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by occupation
in.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(occupation)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by weekday/weekend
in.co.pa.wd%>%
  as_survey(weights = psweight)%>%
  group_by(study_site, dayofweek)%>%
  summarise(mean = survey_mean(num_contacts, na.rm = T),
            sd = survey_sd(num_contacts, na.rm = T),
            median = survey_median(num_contacts, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

#######################
# SUPPTABLE 3 INPUTS
#######################

#count total contacts
nrow(in.co) #20270

#Number of contacts by site
in.co%>%
  group_by(study_site)%>%
  summarise(n = n())

#Number of contacts by study day
in.co%>%
  group_by(study_site, study_day) %>%
  summarise(n = n()) 

in.co%>%
  group_by(study_day) %>%
  summarise(n = n()) 

#Repeat contacts 
in.co%>%
  group_by(study_site, fromdayone) %>%
  summarise(n = n())

in.co%>%
  group_by(fromdayone) %>%
  summarise(n = n())

#Contacts with household members
in.co%>%
  group_by(study_site, hh_membership) %>%
  summarise(n = n())

in.co%>%
  group_by(hh_membership) %>%
  summarise(n = n())

#Physical contacts
in.co%>%
  group_by(study_site, touch_contact) %>%
  summarise(n = n()) 

in.co%>%
  group_by(touch_contact) %>%
  summarise(n = n()) 

#Familiarity with contacts
in.co%>%
  group_by(study_site, never_before) %>%
  summarise(n = n()) 

in.co%>%
  group_by(never_before) %>%
  summarise(n = n()) 

#Indoor/outdoor contacts
in.co%>%
  group_by(study_site, where_contact) %>%
  summarise(n = n()) 

in.co%>%
  group_by(where_contact) %>%
  summarise(n = n())

#Duration of contacts
in.co%>%
  group_by(study_site, duration_contact) %>%
  summarise(n = n())

in.co%>%
  group_by(duration_contact) %>%
  summarise(n = n())

#Location of contact
in.co%>%
  group_by(study_site, location) %>%
  summarise(n = n()) 

in.co%>%
  group_by(location) %>%
  summarise(n = n()) 




######################
# FIGURE 1 
######################

# Figure 1A
#create plot of contact by age
in.co.pa %>%
  ggplot(aes(x = participant_age, y = num_contacts/2, fill=study_site)) + 
  geom_boxplot(show.legend = FALSE) +
  ylim(0, 45) +
  xlab("") +
  ylab("") +
  ggtitle("India") +
  scale_fill_manual(values=c("aquamarine4", "steelblue3")) +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7)) -> in.co.pa.byage.plot


# Figure 1B
# CREATE CONTACT MATRICES
##### Step 1. Create denominator datasets to calculate contact rates
##### Step 2. Symmetrize matrices
##### Step 3. Plot
in.co.pa.counts <-  full_join(in.co, in.pa, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

# Step 1. Create denominator datasets (rural, urban overall)
in.pa%>%
  count(participant_age) %>%
  filter(!is.na(participant_age)) -> o.denoms.byage.in

# Create all combinations of age-age categories
o.denoms.byage.in %>%
  tidyr::expand(participant_age, participant_age) %>%
  setNames(c("participant_age", "contact_age")) -> allagecombs

#create dfs for contact rate calculation, which include denominators for each age group (rural, urban, overall)
in.co.pa.counts  %>%
  group_by(participant_age, contact_age) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs, by=c("participant_age", "contact_age"))  %>%
  left_join(o.denoms.byage.in, by="participant_age") %>% #added .in
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = num_contacts / n) -> in.co.pa.counts.formatrix.o

# Step 2. Adjust for reciprocity (need to load function, 'adjust_for_reciprocity' from Summary figures and functions.R first)
adjust_for_reciprocity(in.co.pa.counts.formatrix.o, o.denoms.byage.in, 9) -> in.co.pa.counts.formatrix.o.sym

# Step 3. Plot matrices
in.co.pa.counts.formatrix.o.sym  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(c.rate.sym, 4))) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(c.rate.sym, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.15) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7)) -> mat.in.o.sym


# Figure 1C
# Location of contact by age
in.co.pa.counts  %>%  
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, fill = location)) +
  geom_bar(position = "fill", color = "black", show.legend = FALSE) +
  xlab("Participant Age") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7)) -> loc.in

######################
# FIGURE 2
######################
id_loc_frame <- expand_grid(rec_id = unique(in.pa$rec_id), location = unique(in.co$location))%>%
  left_join(in.pa%>%select(rec_id, participant_age), by = "rec_id")
age_levels2 <- c("<6mo", "6-11mo", "1-4y")

in.co.pa.counts %>%
  filter(hh_membership == "Non-member") %>%
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  mutate(cont_time = cont_time/(60*2))%>%
  group_by(rec_id, location)%>%
  summarise(cont_time = sum(cont_time))%>%
  full_join(id_loc_frame, by = c("rec_id", "location"))%>%
  mutate(cont_time = replace_na(cont_time, 0))%>%
  group_by(location, participant_age)%>%
  summarise(mean_conthours = mean(cont_time),
            sd = sd(cont_time),
            n = n())%>%
  mutate(lci = mean_conthours - 1.96 * (sd / sqrt(n)),
         uci = mean_conthours + 1.96 * (sd / sqrt(n)))%>%
  mutate(participant_age = factor(participant_age, levels = c("<6mo", "6-11mo", "1-4y")))%>%
  filter(!is.na(participant_age))-> cont_time_byageloc.u5

cont_time_byageloc.u5 %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity", color = "black") +
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(width = 0.9),
    width = 0.4
  ) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0, 10) +
  ggtitle("India") +
  theme_bw()+
  theme(legend.position = c(0.85, 0.72),  # Adjust the position as needed
        legend.background = element_rect(fill = alpha("white", 1)), color = "black",
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(5, 5, 5, 5),  # Ensure space around the legend box
        legend.margin = margin(5, 5, 5, 5))-> conthours.loc.in.u5

# See modeling file for Figure 3 and modeling outputs

###################
# Figure 4 - update the figure numbering
###################
# Panel A
# Modify Prem et al. data
p_ind_mod <- p_contact%>%
  filter(iso3c == "IND" & setting == "overall" & location_contact == "all")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")))

p_ind_table <- p_ind_mod%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "India")


# Modify GlobalMix data
gm_ind_table_prep <- in.co%>%
  left_join(in.pa, by = "rec_id")%>%
  filter(participant_age == "<6mo"|participant_age == "6-11mo"|participant_age == "1-4y")%>%
  mutate(participant_age = "0-4y")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())

gm_ind_table <- in.co%>%
  left_join(in.pa, by = "rec_id")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  filter(participant_age %in% c("5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())%>%
  bind_rows(gm_ind_table_prep)%>%
  mutate(lower_ci = contact_rate - 1.96 * sd/sqrt(n), # Lower bound of 95% CI
         upper_ci = contact_rate + 1.96 * sd/sqrt(n), # Upper bound of 95% CI
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  arrange(participant_age)%>%
  mutate(country = "India")

# Get midpoint of age group
p_ind_table$age_midpoint <- sapply(p_ind_table$participant_age, get_midpoint)
gm_ind_table$age_midpoint <- sapply(gm_ind_table$participant_age, get_midpoint)

# Add a data source column
p_ind_table <- p_ind_table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)

gm_ind_table <- gm_ind_table%>%
  mutate(dataset = "Current study")%>%
  select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)

# Combine the two data
ind_age_table <- rbind(p_ind_table, gm_ind_table)

# Plot
ind_age_plot <- ggplot(ind_age_table, aes(x = age_midpoint, y = contact_rate, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 0.7) +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  scale_y_continuous("Contact Rate") +
  ylim(0, 25)+
  labs(title = "India",
       color = "Dataset") +
  ylab("Contact Rate")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))

# Panel B
# Prepare Prem data
p_ind_school <- p_contact%>%
  filter(iso3c == "IND" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_ind_work <- p_contact%>%
  filter(iso3c == "IND" & setting == "overall" & location_contact == "work")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_ind_home <- p_contact%>%
  filter(iso3c == "IND" & setting == "overall" & location_contact == "home")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_ind_other <- p_contact%>%
  filter(iso3c == "IND" & setting == "overall" & location_contact == "others")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

# Combine the location
p_ind_home_lab <- p_ind_home%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Home")
p_ind_school_lab <- p_ind_school%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "School")
p_ind_work_lab <- p_ind_work%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Work")
p_ind_other_lab <- p_ind_other%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Other")

p_ind_loc <- rbind(p_ind_home_lab, p_ind_school_lab, p_ind_work_lab, p_ind_other_lab)%>%
  group_by(location)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(contact_rate_tot = sum(contact_rate),
         percentage = contact_rate/contact_rate_tot,
         country = "India")

# Prepare GlobalMix data
ind_location <- in.co%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(in.pa, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<1y",
                                     participant_age == "6-11mo" ~ "<1y",
                                     TRUE ~ participant_age))%>%
  left_join(in.we, by = c("participant_age", "study_site"))%>%
  mutate(country = "India")


######################
# Supplemental figures
######################

# Supplemental figure 1
# Plot of contact duration  by location and age
in.co.we <- in.co%>%
  left_join(in.pa%>%select(rec_id, participant_age), by = "rec_id")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(in.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  filter(!is.na(psweight))

in.co.we  %>%  
  filter(!duration_contact == "Unknown") %>%
  as_survey(weights = c(psweight))%>%
  group_by(location, duration_contact)%>%
  summarise(prop = survey_prop())%>%
  ggplot(aes(x = location, fill = duration_contact)) +
  geom_bar(aes(y = prop), position = "fill", stat = "identity") +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab("") +
  ylab("Proportion of contacts") +
  theme(plot.background = element_rect(fill = "white", color = NA))+
  ggtitle("India") +
  scale_fill_viridis(option = "G", discrete = TRUE, direction = -1, alpha = 0.9, begin = 0.3, end = 0.9) -> dur.loc.in


# Supplemental figure 2
# Create exposure-hours variable and then plot by location / age
location_levels <- unique(in.co$location)
age_levels <- unique(in.pa$participant_age)

in.co%>%
  mutate(cont_time = cont_time/(60*2))%>%
  group_by(rec_id, location)%>%
  summarise(cont_time = sum(cont_time))%>%
  full_join(id_loc_frame, by = c("rec_id", "location"))%>%
  mutate(cont_time = replace_na(cont_time, 0))%>%
  group_by(location, participant_age)%>%
  summarise(mean_conthours = mean(cont_time),
            sd = sd(cont_time),
            n = n())%>%
  mutate(lci = mean_conthours - 1.96 * (sd / sqrt(n)),
         uci = mean_conthours + 1.96 * (sd / sqrt(n)))-> cont_time_byageloc_all.in

cont_time_byageloc_all.in %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity", color = "black") +
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(width = 0.9),
    width = 0.6
  ) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0, 19) +
  theme_bw() +
  ggtitle("India") +
  theme(legend.position = c(0.87, 0.74),  # Adjust the position as needed
        legend.background = element_rect(fill = alpha("white", 1)), color = "black",
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(2, 2, 2, 2),  # Ensure space around the legend box
        legend.margin = margin(5, 5, 5, 5))  -> conthours.loc.all.in

# Supplemental figure 3
#Proportion of high-risk contacts
in.hr.co <- in.co.pa.counts%>%
  filter(duration_contact == "1-4 hrs"| duration_contact == ">4 hrs")%>%
  filter(touch_contact == "Yes")

in.hr.co %>%
  filter(!location == "Unreported")%>%
  ggplot(aes(x = participant_age, fill = location)) +
  geom_bar(position = "fill", color = "black") +
  xlab("Participant Age") +
  ylab("Prop contacts") +
  labs(title = "India")+
  scale_x_discrete(labels = label_wrap(10))-> hr.loc.in

in.hr.co%>%
  filter(location != "Home")%>%
  group_by(location)%>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))


# See supp4-8 file for supplemental figures 4-8.

## Supp fig contact matrix
# Step 1. Create denominator datasets (Rural)
in.pa %>%
  filter(study_site == "Rural")%>%
  count(participant_age) %>%
  filter(!is.na(participant_age))-> r.denoms.byage.in

#create dataframes for contact rate calculation, which include denominators for each age group
in.co.pa.counts  %>%
  filter(study_site == "Rural")%>%
  group_by(participant_age, contact_age) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs, by=c("participant_age", "contact_age"))  %>%
  left_join(r.denoms.byage.in, by="participant_age") %>% 
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = num_contacts / n)-> in.co.pa.counts.formatrix.r

# Step 2. Adjust for reciprocity (need to load function, 'adjust_for_reciprocity' from Summary figures and functions.R first)
adjust_for_reciprocity(in.co.pa.counts.formatrix.r, r.denoms.byage.in, 9) -> in.co.pa.counts.formatrix.r.sym

# Step 3. Plot matrices
in.co.pa.counts.formatrix.r.sym  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(c.rate.sym, 4))) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  scale_fill_distiller(palette = "YlGnBu",  direction = 1,
                       name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(c.rate.sym, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.15) +
  labs(x = "", y = "", title = "India")+
  scale_x_discrete(labels = label_wrap(10)) -> mat.in.r.sym


# Step 1. Create denominator datasets (Urban)
in.pa %>%
  filter(study_site == "Urban")%>%
  count(participant_age) %>%
  filter(!is.na(participant_age))-> u.denoms.byage.in

#create dataframes for contact rate calculation, which include denominators for each age group
in.co.pa.counts  %>%
  filter(study_site == "Urban")%>%
  group_by(participant_age, contact_age) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs, by=c("participant_age", "contact_age"))  %>%
  left_join(u.denoms.byage.in, by="participant_age") %>% 
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = num_contacts / n)-> in.co.pa.counts.formatrix.u

# Step 2. Adjust for reciprocity (need to load function, 'adjust_for_reciprocity' from Summary figures and functions.R first)
adjust_for_reciprocity(in.co.pa.counts.formatrix.u, u.denoms.byage.in, 9) -> in.co.pa.counts.formatrix.u.sym

# Step 3. Plot matrices
in.co.pa.counts.formatrix.u.sym  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(c.rate.sym, 4))) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(c.rate.sym, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.15) +
  labs(x = "Participant age", y = "")+
  scale_x_discrete(labels = label_wrap(10)) -> mat.in.u.sym


# Location of contact by age
in.co.pa.counts  %>% 
  filter(study_site == "Rural")%>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, fill = location)) +
  geom_bar(position = "fill", color = "black", show.legend = FALSE) +
  labs(x = "", y = "", title = "India")+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) -> loc.in.r

in.co.pa.counts  %>% 
  filter(study_site == "Urban")%>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, fill = location)) +
  geom_bar(position = "fill", color = "black", show.legend = FALSE) +
  labs(x = "Participant Age", y = "")+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) -> loc.in.u


##################################
## Supp figure for comparing GlobalMix and Prem contact matrix
##################################

# Modify the Prem data
# Take weighted mean for aggregating age groups
in_pop <- pop%>%
  filter(country == "India")%>%
  t()%>%
  as.data.frame()%>%
  slice(-1, -2) %>%
  setNames("population")%>%
  tibble::rownames_to_column("age")%>%
  mutate(age_group = case_when(age == "age0" ~ "0-4y",
                               age == "age5" ~ "5-9y",
                               age == "age10" ~ "10-14y",
                               age == "age15" ~ "15-19y",
                               age == "age20" ~ "20-24y",
                               age == "age25" ~ "25-29y",
                               age == "age30" ~ "30-34y",
                               age == "age35" ~ "35-39y",
                               age == "age40" ~ "40-44y",
                               age == "age45" ~ "45-49y",
                               age == "age50" ~ "50-54y",
                               age == "age55" ~ "55-59y",
                               age == "age60" ~ "60-64y",
                               age == "age65" ~ "65-69y",
                               age == "age70" ~ "70-74y",
                               TRUE ~ "75+y"),
         population = as.numeric(population))%>%
  group_by(age_group)%>%
  summarise(subtotal = sum(population))


p_in_o <- p_ind_mod%>%
  mutate(contact_age_mod = case_when(contact_age == "10-14y" ~ "10-19y",
                                     contact_age == "15-19y" ~ "10-19y",
                                     contact_age == "20-24y" ~ "20-29y",
                                     contact_age == "25-29y" ~ "20-29y",
                                     contact_age == "30-34y" ~ "30-39y",
                                     contact_age == "35-39y" ~ "30-39y",
                                     contact_age == "40-44y" ~ "40-59y",
                                     contact_age == "45-49y" ~ "40-59y",
                                     contact_age == "50-54y" ~ "40-59y",
                                     contact_age == "55-59y" ~ "40-59y",
                                     contact_age == "60-64y" ~ "60+y",
                                     contact_age == "65-69y" ~ "60+y",
                                     contact_age == "70-74y" ~ "60+y",
                                     contact_age == "75+y" ~ "60+y",
                                     TRUE ~ contact_age))%>%
  group_by(participant_age, contact_age_mod)%>%
  summarise(contact_rate_we = sum(contact_rate))%>%
  left_join(in_pop, by = c("participant_age" = "age_group"))%>%
  rename(par_pop = subtotal)%>%
  mutate(participant_age_mod = case_when(participant_age == "10-14y" ~ "10-19y",
                                         participant_age == "15-19y" ~ "10-19y",
                                         participant_age == "20-24y" ~ "20-29y",
                                         participant_age == "25-29y" ~ "20-29y",
                                         participant_age == "30-34y" ~ "30-39y",
                                         participant_age == "35-39y" ~ "30-39y",
                                         participant_age == "40-44y" ~ "40-59y",
                                         participant_age == "45-49y" ~ "40-59y",
                                         participant_age == "50-54y" ~ "40-59y",
                                         participant_age == "55-59y" ~ "40-59y",
                                         participant_age == "60-64y" ~ "60+y",
                                         participant_age == "65-69y" ~ "60+y",
                                         participant_age == "70-74y" ~ "60+y",
                                         participant_age == "75+y" ~ "60+y",
                                         TRUE ~ participant_age))%>%
  group_by(participant_age_mod, contact_age_mod)%>%
  summarise(contact_rate_we = sum(contact_rate_we*par_pop)/sum(par_pop))%>%
  mutate(participant_age_mod = factor(participant_age_mod, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age_mod = factor(contact_age_mod, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  rename(participant_age = participant_age_mod,
         contact_age = contact_age_mod)

# Plot matrix
p_in_o  %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(contact_rate_we, 7))) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 7),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(contact_rate_we, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.1) +
  xlab("Participant age") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) -> p.mat.in


# GlobalMix data
in.co.pa.counts.7 <-  full_join(in.co, in.pa, 
                                by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "0-4y",
                                     participant_age == "6-11mo" ~ "0-4y",
                                     participant_age == "1-4y" ~ "0-4y",
                                     TRUE ~ participant_age),
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = case_when(contact_age == "<6mo" ~ "0-4y",
                                 contact_age == "6-11mo" ~ "0-4y",
                                 contact_age == "1-4y" ~ "0-4y",
                                 TRUE ~ contact_age),
         contact_age = factor(contact_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

# Step 1. Create denominator datasets (rural, urban overall)
in.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "0-4y",
                                     participant_age == "6-11mo" ~ "0-4y",
                                     participant_age == "1-4y" ~ "0-4y",
                                     TRUE ~ participant_age),
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  count(participant_age) %>%
  filter(!is.na(participant_age)) -> o.denoms.byage.in.7

# Create all combinations of age-age categories
o.denoms.byage.in.7 %>% 
  tidyr::expand(participant_age, participant_age) %>%
  setNames(c("participant_age", "contact_age")) -> allagecombs.7

#create dfs for contact rate calculation, which include denominators for each age group
in.co.pa.counts.7  %>%
  group_by(participant_age, contact_age) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs.7, by=c("participant_age", "contact_age"))  %>%
  left_join(o.denoms.byage.in.7, by="participant_age") %>%
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = (num_contacts) / n) -> in.co.pa.counts.formatrix.o.7

# Step 2. Adjust for reciprocity
adjust_for_reciprocity(in.co.pa.counts.formatrix.o.7, o.denoms.byage.in.7, 7) -> in.co.pa.counts.formatrix.o.sym.7

# Step 3. Plot matrices
in.co.pa.counts.formatrix.o.sym.7  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(c.rate.sym, 4))) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 7),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(c.rate.sym, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.1) +
  labs(x = "", y = "", title = "India")+
  scale_x_discrete(labels = label_wrap(10)) -> mat.in.o.sym.7

#####################
# RESULTS TEXT INPUTS FOR MANUSCRIPT
#####################
# Contact by age - DAY 1 ONLY
in.co.pa %>%
  group_by(participant_age) %>%
  summarise(mean = round(mean(num_contacts, na.rm = T), 1), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

# Proportion of the location of contact
in.co.pa.counts %>%
  group_by(location) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

in.co.pa.counts %>%
  group_by(participant_age, location) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  print(n = 100)


# Proportion of contacts reported at home by age
in.co.pa.counts %>%
  filter(location == "Home") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at school by age
in.co.pa.counts %>%
  filter(location == "School") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at work by age
in.co.pa.counts %>%
  filter(location == "Work") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

# Proportion of contacts reported at transit by age
in.co.pa.counts %>%
  filter(location == "Transit") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

# Calculate exposure-hours to contacts to determine mean
in.co.pa.counts %>%
  group_by(rec_id, study_site) %>%
  summarize(cont_time = sum(cont_time)/60) -> cont_time_byp
cont_time_byp %>%
  group_by(study_site)%>%
  summarise(mean = mean(cont_time, na.rm = T),
            min = min(cont_time, na.rm = T),
            max = max(cont_time, na.rm = T)) 

#Familiarity with contacts *by location*
in.co%>%
  group_by(location, never_before) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

#Physicality of contacts *by location*
in.co%>%
  group_by(location, touch_contact) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

#Indoors/outdoors of contacts *by location*
in.co%>%
  group_by(location, where_contact) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  print(n = 30)

# Proportion of contacts that are with household members for u5s
in.co.pa.counts  %>%  
  filter(participant_age == "<6mo") %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
in.co.pa.counts  %>%  
  filter(participant_age == "6-11mo") %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
in.co.pa.counts  %>%  
  filter(participant_age == "1-4y") %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
in.co.pa.counts  %>%  
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(hh_membership) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

# Proportion of contacts by location for u5s
in.co.pa.counts  %>%  
  filter(participant_age == "<6mo") %>%
  group_by(location) %>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n)) 
in.co.pa.counts  %>%  
  filter(participant_age == "6-11mo") %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
in.co.pa.counts  %>%  
  filter(participant_age == "1-4y") %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
in.co.pa.counts  %>%  
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
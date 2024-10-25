#Mozambique
mo.pa <- readRDS("./Mozambique/moz_participant_data_aim1.RDS")
mo.co <- readRDS("./Mozambique/moz_contact_data_aim1.RDS")
mo.we <- read.csv("./Other/moz_pop.csv")

######################
# Calculation of weight
######################

mo.pa.we <- mo.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

mo.we <- mo.we%>%
  left_join(mo.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)

#######################
# TABLE 1 INPUTS
#######################

# Participant data characteristics
# Total number of participants
nrow(mo.pa) #1363 participants

## By site
table(mo.pa$study_site)
## By age and site
table(mo.pa$participant_age, mo.pa$study_site, useNA = 'always')
# By age
table(mo.pa$participant_age, useNA = "always")
## By sex and site
table(mo.pa$participant_sex, mo.pa$study_site, useNA = 'always')
# By sex
table(mo.pa$participant_sex, useNA = "always")

# Education
# restrict to non/kids only
mo.pa%>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y")) -> mo.pa.notkids
nrow(mo.pa.notkids) #954
table(mo.pa.notkids$high_educ, mo.pa.notkids$study_site, useNA = 'always')
table(mo.pa.notkids$high_educ, useNA = 'always')

#Occupation
# restrict to adults only
mo.pa%>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y", "5-9y")) -> mo.pa.adults
nrow(mo.pa.adults) #832
table(mo.pa.adults$occupation, mo.pa.adults$study_site, useNA = 'always')
table(mo.pa.adults$occupation, useNA = 'always')

#HH size by site
table(mo.pa$hh_size_cat, mo.pa$study_site, useNA = 'always')

#hh size
table(mo.pa$hh_size_cat, useNA = 'always')

mo.pa%>%
  group_by(study_site) %>% 
  summarise(mean = mean(hh_size, na.rm = T), 
            median = median(hh_size, na.rm = T), 
            q = list(quantile(hh_size, na.rm = T))) %>%
  unnest_wider(q)

median(mo.pa$hh_size, na.rm = T)
quantile(mo.pa$hh_size, na.rm = T)

#Number of generations and families in households
mo.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(study_site, hh.gens) %>%
  summarize(freq = n())

mo.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(hh.gens) %>%
  summarize(freq = n())

mo.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(study_site, hh.multfams) %>%
  summarize(freq = n()) 

mo.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(hh.multfams) %>%
  summarize(freq = n()) 


#####################
# Join contact with participant data to assess contact patterns by participant characteristics
#####################

# add participant data to contact data
mo.co.pa.full <-  full_join(mo.co, mo.pa, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(contact, name = "num_contacts")

# Merge contact counts with participant data
mo.co.pa <- left_join(mo.pa, mo.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))

# Adjust the age group
mo.co.pa.age <- mo.co.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))


#######################
# TABLE 2 INPUTS
#######################

# Contact overall
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and age
mo.co.pa %>%
  group_by(study_site, participant_age) %>%
  summarise(mean = round(mean(num_contacts/2, na.rm = T), 1), 
            sd = sd(num_contacts/2, na.rm = T),
            median = median(num_contacts/2, na.rm = T), 
            q = list(quantile(num_contacts/2, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

# Contact by age
mo.co.pa %>%
  group_by(participant_age) %>%
  summarise(mean = round(mean(num_contacts/2, na.rm = T), 1), 
            sd = sd(num_contacts/2, na.rm = T),
            median = median(num_contacts/2, na.rm = T), 
            q = list(quantile(num_contacts/2, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

# Contact by site and sex
mo.co.pa.age %>%
  filter(!is.na(participant_sex))%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, participant_sex)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by sex
mo.co.pa.age %>%
  filter(!is.na(participant_sex))%>%
  as_survey(weights = c(psweight))%>%
  group_by(participant_sex)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and hh size
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, hh_size_cat)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by hh size
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(hh_size_cat)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and education 
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, high_educ)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by education
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(high_educ)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and occupation
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, occupation)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by occupation
mo.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(occupation)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))


#######################
# SUPP TABLE 3 INPUTS
#######################

#join contact data with weight
mo.co.we <- mo.co%>%
  left_join(mo.pa%>%select(rec_id, participant_age), by = "rec_id")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))

#count total contacts
nrow(mo.co.we) #17674

# Number of contacts by site
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site)%>%
  summarise(n = survey_total())

#Number of contacts by site and study day
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, study_day) %>%
  summarise(n = survey_total())

# By study day
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_day) %>%
  summarise(n = survey_total())

#Repeat contacts 
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, fromdayone) %>%
  summarise(n = survey_total())

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(fromdayone) %>%
  summarise(n = survey_total())

#Contacts with household members
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, hh_membership) %>%
  summarise(n = survey_total()) 

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(hh_membership) %>%
  summarise(n = survey_total()) 

#Physical contacts
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, touch_contact) %>%
  summarise(n = survey_total())

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(touch_contact) %>%
  summarise(n = survey_total())

#Familiarity with contacts
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, never_before) %>%
  summarise(n = survey_total())

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(never_before) %>%
  summarise(n = survey_total())

#Indoor/outdoor contacts
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, where_contact) %>%
  summarise(n = survey_total())

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(where_contact) %>%
  summarise(n = survey_total())

#Duration of contacts
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, duration_contact) %>%
  summarise(n = survey_total())

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(duration_contact) %>%
  summarise(n = survey_total())

#Location of contact
mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, location) %>%
  summarise(n = survey_total())

mo.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(location) %>%
  summarise(n = survey_total())


######################
# FIGURE 1 
######################

# Figure 1A
#create plot of contact by age
mo.co.pa %>%
  ggplot(aes(x = participant_age, y = num_contacts/2, fill=study_site)) + 
  geom_boxplot(show.legend = FALSE) +
  ylim(0, 45) +
  xlab("") +
  ylab("") +
  ggtitle("Mozambique") +
  scale_fill_manual(values=c("aquamarine4", "steelblue3")) +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7)) -> mo.co.pa.byage.plot


# Figure 1B
# CREATE CONTACT MATRICES
##### Step 1. Create denominator datasets to calculate contact rates
##### Step 2. Symmetrize matrices
##### Step 3. Plot

mo.co.pa.counts <-  full_join(mo.co, mo.pa, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

# Step 1. Create denominator datasets (rural, urban overall)
mo.pa%>%
  count(participant_age) %>%
  filter(!is.na(participant_age)) -> o.denoms.byage.mo

# Create all combinations of age-age categories
o.denoms.byage.mo %>% # changed
  tidyr::expand(participant_age, participant_age) %>%
  setNames(c("participant_age", "contact_age")) -> allagecombs

#create dfs for contact rate calculation, which include denominators for each age group (rural, urban, overall)
mo.co.pa.counts  %>%
  group_by(participant_age, contact_age) %>%
  #filter(study_day == 1) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs, by=c("participant_age", "contact_age"))  %>%
  left_join(o.denoms.byage.mo, by="participant_age") %>% #changed
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = (num_contacts) / n) -> mo.co.pa.counts.formatrix.o

# Step 2. Adjust for reciprocity (need to load function, 'adjust_for_reciprocity' from Summary figures and functions.R first)
adjust_for_reciprocity(mo.co.pa.counts.formatrix.o, o.denoms.byage.mo) -> mo.co.pa.counts.formatrix.o.sym 

# Step 3. Plot matrices
mo.co.pa.counts.formatrix.o.sym  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = c.rate.sym)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7)) +
  scale_fill_distiller(palette = "Spectral", limits=c(0, 10), name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = round(c.rate.sym, digits = 1)), color = "white", size = 3) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) -> mat.mo.o.sym


# Figure 1C
# Location of contact by age
mo.co.pa.counts  %>%  
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, fill = location)) +
  geom_bar(position = "fill", color = "black") +
  xlab("Participant Age") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7)) -> loc.mo

######################
# FIGURE 2
######################

# Create exposure-hours variable and then plot by location / age
mo.co.pa.counts %>%
  #filter(study_day==1) %>%
  filter(hh_membership == "Non-member")%>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/(60*2)) %>%
  left_join(o.denoms.byage.mo, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n) -> cont_time_byageloc_all

cont_time_byageloc_all %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0,10) +
  ggtitle("Mozambique") +
  theme_bw() -> conthours.loc.mo


# See modeling file for Figure 3 and modeling outputs

######################
# SUPPLEMENTAL FIGURES
######################

# Supplemental figure 1
# Plot of contact duration  by location and age
mo.co.we  %>%  
  filter(!duration_contact == "Unknown") %>%
  as_survey(weights = c(psweight))%>%
  group_by(location, duration_contact)%>%
  summarise(prop = survey_prop())%>%
  ggplot(aes(x = location, fill = duration_contact)) +
  geom_bar(aes(y = prop), position = "fill", stat = "identity",show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab("") +
  ylab("Proportion of contacts") +
  theme(plot.background = element_rect(fill = "white", color = NA))+
  ggtitle("Mozambique") +
  scale_fill_viridis(option = "G", discrete = TRUE, direction = -1, alpha = 0.9, begin = 0.3, end = 0.9) -> dur.loc.mo


# Supplemental figure 2
# Nature and locations of contact
## Physicality
mo.co.we  %>%  
  subset(!is.na(contact_age)) %>%
  filter(!is.na(touch_contact))%>%
  as_survey(weights = c(psweight))%>%
  group_by(location, touch_contact)%>%
  summarise(prop = survey_prop())%>%
  ggplot(aes(x = location, fill = touch_contact)) +
  geom_bar(aes(y = prop), position="fill", stat = "identity", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  ggtitle("Mozambique") +
  guides(fill=guide_legend(title="Physical contact")) -> phys.loc.mo

## Familiarity
mo.co.we  %>%  
  subset(!is.na(contact_age)) %>%
  filter(!is.na(known_contact)) %>%
  as_survey(weights = c(psweight))%>%
  group_by(location, known_contact)%>%
  summarise(prop = survey_prop())%>%
  ggplot(aes(x = location, fill = known_contact)) +
  geom_bar(aes(y = prop), position="fill", stat = "identity", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  guides(fill=guide_legend(title="Familiarity")) -> known.loc.mo

## Indoors/Outdoors
mo.co.we  %>%  
  subset(!is.na(contact_age)) %>%
  subset(!is.na(where_contact)) %>%
  as_survey(weights = c(psweight))%>%
  group_by(location, where_contact)%>%
  summarise(prop = survey_prop())%>%
  #filter(!location == "Unreported") %>%
  ggplot(aes(x = location, fill = where_contact)) +
  geom_bar(aes(y = prop), position="fill", stat = "identity") +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  guides(fill=guide_legend(title="Setting"))-> indoor.loc.mo

# Supplemental figure 3
# Create exposure-hours variable and then plot by location / age FOR UNDER 5s
mo.co.pa.counts %>%
  filter(hh_membership == "Non-member") %>%
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/(60*2)) %>%
  left_join(o.denoms.byage.mo, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n)  -> cont_time_byageloc.u5

cont_time_byageloc.u5 %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0, 10) +
  ggtitle("Mozambique") +
  theme_bw() -> conthours.loc.mo.u5


# See supp4-8 file for supplemental figures 4-8.

# Exposure hours (all contacts)
mo.co.pa.counts %>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/(60*2)) %>%
  left_join(o.denoms.byage.mo, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n) -> cont_time_byageloc_all

cont_time_byageloc_all %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0,18) +
  ggtitle("Mozambique") +
  theme_bw() -> conthours.loc.all.mo

#####################
# RESULTS TEXT INPUTS FOR MANUSCRIPT
#####################
# Contact by age
mo.co.pa %>%
  group_by(participant_age) %>%
  summarise(mean = round(mean(num_contacts, na.rm = T), 1), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

#Location of contact
mo.co.pa.counts %>%
  group_by(location) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at home by age
mo.co.pa.counts %>%
  filter(location == "Home") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at school by age
mo.co.pa.counts %>%
  filter(location == "School") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at work by age
mo.co.pa.counts %>%
  filter(location == "Work") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

# Proportion of contacts reported at transit by age
mo.co.pa.counts %>%
  filter(location == "Transit") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

# Calculate exposure-hours to contacts to determine mean
mo.co.pa.counts %>%
  group_by(rec_id, study_site) %>%
  summarize(cont_time = sum(cont_time)/60) -> cont_time_byp
cont_time_byp %>%
  group_by(study_site) %>%
  summarise(mean = mean(cont_time, na.rm = T),
            min = min(cont_time, na.rm = T),
            max = max(cont_time, na.rm = T)) 

#Familiarity with contacts *by location*
mo.co%>%
  group_by(location, never_before) %>% #added location
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

#Physicality of contacts *by location*
mo.co%>%
  group_by(location, touch_contact) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

#Indoor/outdoor location of contacts *by location*
mo.co%>%
  group_by(location, where_contact) %>%
  filter(hh_membership == "Non-member") %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  print(n = 30)

# Proportion of contacts that are with household members for u5s
mo.co.pa.counts  %>%  
  filter(participant_age == "<6mo") %>%
  group_by(hh_membership) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))  
mo.co.pa.counts  %>%  
  filter(participant_age == "6-11mo") %>%
  group_by(hh_membership) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))  
mo.co.pa.counts  %>%  
  filter(participant_age == "1-4y") %>%
  group_by(hh_membership) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))  
mo.co.pa.counts  %>%  
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(hh_membership) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts by location for u5s
mo.co.pa.counts  %>%  
  filter(participant_age == "<6mo") %>%
  group_by(location) %>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n)) 
mo.co.pa.counts  %>%  
  filter(participant_age == "6-11mo") %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
mo.co.pa.counts  %>%  
  filter(participant_age == "1-4y") %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
mo.co.pa.counts  %>%  
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
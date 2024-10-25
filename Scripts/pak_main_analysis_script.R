#Pakistan
pa.pa <- readRDS("./Pakistan/pak_participant_data_aim1.RDS")
pa.co <- readRDS("./Pakistan/pak_contact_data_aim1.RDS")
pa.we <- read.csv("./Other/pak_pop.csv")

######################
# Calculation of weight
######################

pa.pa.we <- pa.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

pa.we <- pa.we%>%
  left_join(pa.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)

#######################
# TABLE 1 INPUTS
#######################

# Participant data characteristics
# Total number of participants
nrow(pa.pa) #1337 participants

# By site
table(pa.pa$study_site)
# By age and site
table(pa.pa$participant_age, pa.pa$study_site, useNA = 'always')
# By age
table(pa.pa$participant_age, useNA = "always")
# By sex and site
table(pa.pa$participant_sex, pa.pa$study_site, useNA = 'always')
# By sex
table(pa.pa$participant_sex, useNA = "always")

#Education
#restrict to non/kids only
pa.pa%>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y")) -> pa.pa.notkids
nrow(pa.pa.notkids) #946
table(pa.pa.notkids$high_educ, pa.pa.notkids$study_site, useNA = 'always')
table(pa.pa.notkids$high_educ, useNA = 'always')

#Occupation
# restrict to adults only
pa.pa%>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y", "5-9y")) -> pa.pa.adults
nrow(pa.pa.adults) #817
table(pa.pa.adults$occupation, pa.pa.adults$study_site, useNA = 'always')
table(pa.pa.adults$occupation, useNA = 'always')

# HH size by site
table(pa.pa$hh_size_cat, pa.pa$study_site, useNA = 'always')
table(pa.pa$hh_size_cat, useNA = "always")

pa.pa%>%
  group_by(study_site) %>%
  summarise(mean = mean(hh_size, na.rm = T), 
            median = median(hh_size, na.rm = T), 
            q = list(quantile(hh_size, na.rm = T))) %>%
  unnest_wider(q)

median(pa.pa$hh_size, na.rm = T)
quantile(pa.pa$hh_size, na.rm = T)

#Number of generations and families in households - generate participant-level statistics to put into TABLE 1
pa.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(study_site, hh.gens) %>%
  summarize(freq = n())

pa.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(hh.gens) %>%
  summarize(freq = n())

pa.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(study_site, hh.multfams) %>%
  summarize(freq = n()) 

pa.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(hh.multfams) %>%
  summarize(freq = n()) 

#####################
# Join contact with participant data to assess contact patterns by participant characteristics
#####################

# add participant data to contact data
pa.co.pa.full <-  full_join(pa.co, pa.pa, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(contact, name = "num_contacts")

# Merge contact counts with participant data
pa.co.pa <- left_join(pa.pa, pa.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))

# Adjust the age group
pa.co.pa.age <- pa.co.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pa.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  filter(!is.na(participant_age)) # remove the missing participant age observations as this will introduce error when weighting for age


#######################
# TABLE 2 INPUTS
#######################

# Contact overall
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# contact by site
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and age
pa.co.pa %>%
  group_by(study_site, participant_age) %>%
  summarise(mean = round(mean(num_contacts/2, na.rm = T), 1), 
            sd = sd(num_contacts/2, na.rm = T),
            median = median(num_contacts/2, na.rm = T), 
            q = list(quantile(num_contacts/2, na.rm = T)),
            n = n()) %>%
  unnest_wider(q)

# Contact by age
pa.co.pa %>%
  group_by(participant_age) %>%
  summarise(mean = round(mean(num_contacts/2, na.rm = T), 1), 
            sd = sd(num_contacts/2, na.rm = T),
            median = median(num_contacts/2, na.rm = T), 
            q = list(quantile(num_contacts/2, na.rm = T)),
            n = n()) %>%
  unnest_wider(q)

# Contact by site and sex
pa.co.pa.age %>%
  filter(!is.na(participant_sex))%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, participant_sex)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by sex
pa.co.pa.age %>%
  filter(!is.na(participant_sex))%>%
  as_survey(weights = c(psweight))%>%
  group_by(participant_sex)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and hh size
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, hh_size_cat)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by hh size
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(hh_size_cat)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and education
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, high_educ)%>%
  filter(n() >= 2) %>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contacct by education
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(high_educ)%>%
  filter(n() >= 2) %>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by site and occupation
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, occupation)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

# Contact by occupation
pa.co.pa.age %>%
  as_survey(weights = c(psweight))%>%
  group_by(occupation)%>%
  summarise(mean = survey_mean(num_contacts/2, na.rm = T),
            sd = survey_sd(num_contacts/2, na.rm = T),
            median = survey_median(num_contacts/2, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts/2, c(0.25, 0.75), na.rm = T))

#######################
# SUPPTABLE 3 INPUTS
#######################

pa.co.we <- pa.co%>%
  left_join(pa.pa%>%select(rec_id, participant_age), by = "rec_id")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pa.we%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  filter(!is.na(psweight))

#count total contacts
nrow(pa.co.we) #32178

# Number of contacts by site
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site)%>%
  summarise(n = survey_total())

#Number of contacts by site and study day
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, study_day) %>%
  summarise(n = survey_total())

# Number of contacts by study day
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_day) %>%
  summarise(n = survey_total())

#Repeat contacts 
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, fromdayone) %>%
  summarise(n = survey_total())

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(fromdayone) %>%
  summarise(n = survey_total())

#Contacts with household members
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, hh_membership) %>%
  summarise(n = survey_total()) 

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(hh_membership) %>%
  summarise(n = survey_total()) 

#Physical contacts
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, touch_contact) %>%
  summarise(n = survey_total())

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(touch_contact) %>%
  summarise(n = survey_total())

#Familiarity with contacts
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, never_before) %>%
  summarise(n = survey_total())

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(never_before) %>%
  summarise(n = survey_total())

#Indoor/outdoor contacts
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, where_contact) %>%
  summarise(n = survey_total())

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(where_contact) %>%
  summarise(n = survey_total())

#Duration of contacts
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, duration_contact) %>%
  summarise(n = survey_total())

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(duration_contact) %>%
  summarise(n = survey_total())

#Location of contact
pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(study_site, location) %>%
  summarise(n = survey_total())

pa.co.we%>%
  as_survey(weights = c(psweight))%>%
  group_by(location) %>%
  summarise(n = survey_total())


######################
# FIGURE 1 
######################

# Figure 1A
#create plot of contact by age - DAY 1 ONLY
pa.co.pa %>%
  filter(!is.na(participant_age)) %>%
  ggplot(aes(x = participant_age, y = num_contacts/2, fill=study_site)) + 
  geom_boxplot(show.legend = FALSE) +
  ylim(0, 45) +
  xlab("") +
  ylab("") +
  ggtitle("Pakistan") +
  scale_fill_manual(values=c("aquamarine4", "steelblue3")) +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7))-> pa.co.pa.byage.plot


# Figure 1B
# CREATE CONTACT MATRICES
##### Step 1. Create denominator datasets to calculate contact rates
##### Step 2. Symmetrize matrices
##### Step 3. Plot

pa.co.pa.counts <-  full_join(pa.co, pa.pa, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

# Step 1. Create denominator datasets (rural, urban overall)
pa.pa%>%
  count(participant_age) %>%
  filter(!is.na(participant_age)) -> o.denoms.byage.pa

# Create all combinations of age-age categories
o.denoms.byage.pa %>%
  expand(participant_age, participant_age) %>%
  setNames(c("participant_age", "contact_age")) -> allagecombs

#create dfs for contact rate calculation, which include denominators for each age group (rural, urban, overall)
pa.co.pa.counts  %>%
  group_by(participant_age, contact_age) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs, by=c("participant_age", "contact_age"))  %>%
  left_join(o.denoms.byage.pa, by="participant_age") %>% 
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = num_contacts / n) -> pa.co.pa.counts.formatrix.o

# Step 2. Adjust for reciprocity (need to load function, 'adjust_for_reciprocity' from Summary figures and functions.R first)
adjust_for_reciprocity(pa.co.pa.counts.formatrix.o, o.denoms.byage.pa) -> pa.co.pa.counts.formatrix.o.sym

# Step 3. Plot matrices
pa.co.pa.counts.formatrix.o.sym  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = c.rate.sym)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7)) +
  scale_fill_distiller(palette = "Spectral", limits=c(0, 9), name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = round(c.rate.sym, digits = 1)), color = "white", size = 3) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) -> mat.pa.o.sym


# Figure 1C
# Location of contact by age
pa.co.pa.counts  %>%  
  filter(!location == "Unreported") %>%
  filter(!is.na(participant_age)) %>%
  ggplot(aes(x = participant_age, fill = location)) +
  geom_bar(position = "fill", color = "black", show.legend = FALSE) +
  xlab("Participant Age") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(axis.text.x = element_text(size = 7)) -> loc.pa


######################
# FIGURE 2
######################

# Create exposure-hours variable and then plot by location / age
pa.co.pa.counts %>%
  filter(hh_membership == "Non-member")%>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/(60*2)) %>%
  left_join(o.denoms.byage.pa, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n) -> cont_time_byageloc_all

cont_time_byageloc_all %>%
  filter(!location == "Unreported") %>%
  filter(!is.na(participant_age)) %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0, 10) +
  theme_bw() +
  ggtitle("Pakistan") -> conthours.loc.pa


# See modeling file for Figure 3 and modeling outputs.

######################
# SUPPLEMENTAL FIGURES
######################

# Supplemental figure 1
# Plot of contact duration  by location and age
pa.co.we  %>%  
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
  ggtitle("Pakistan") +
  scale_fill_viridis(option = "G", discrete = TRUE, direction = -1, alpha = 0.9, begin = 0.3, end = 0.9) -> dur.loc.pa


# Supplemental figure 2
# Nature and locations of contacts
## Physicality
pa.co.we  %>%  
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
  ggtitle("Pakistan") +
  guides(fill=guide_legend(title="Physical contact")) -> phys.loc.pa

## Familiarity
pa.co.we  %>%  
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
  guides(fill=guide_legend(title="Familiarity")) -> known.loc.pa

## Indoors/Outdoors
pa.co.we  %>%  
  subset(!is.na(contact_age)) %>%
  subset(!is.na(where_contact)) %>%
  as_survey(weights = c(psweight))%>%
  group_by(location, where_contact)%>%
  summarise(prop = survey_prop())%>%
  #filter(!location == "Unreported") %>%
  ggplot(aes(x = location, fill = where_contact)) +
  geom_bar(aes(y = prop), position="fill", stat = "identity", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  guides(fill=guide_legend(title="Setting"))-> indoor.loc.pa


# Supplemental figure 3
# Create exposure-hours variable and then plot by location / age for u5s
pa.co.pa.counts %>%
  filter(hh_membership == "Non-member") %>%
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/(60*2)) %>%
  left_join(o.denoms.byage.pa, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n)  -> cont_time_byageloc.u5

cont_time_byageloc.u5 %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0, 16) +
  ggtitle("Pakistan") +
  theme_bw() -> conthours.loc.pa.u5


# See supp4-8 file for supplemental figures 4-8.

#####################
# RESULTS TEXT INPUTS FOR MANUSCRIPT
#####################

# Contact by age - DAY 1 ONLY
pa.co.pa %>%
  group_by(participant_age) %>%
  summarise(mean = round(mean(num_contacts, na.rm = T), 1), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

#Location of contact
pa.co.pa.counts %>%
  group_by(location) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at home by age
pa.co.pa.counts %>%
  filter(location == "Home") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at school by age
pa.co.pa.counts %>%
  filter(location == "School") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Proportion of contacts reported at work by age
pa.co.pa.counts %>%
  filter(location == "Work") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

# Proportion of contacts reported at transit by age
pa.co.pa.counts %>%
  filter(location == "Transit") %>%
  group_by(participant_age) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

# Calculate exposure-hours to contacts to determine mean
pa.co.pa.counts %>%
  group_by(rec_id, study_site) %>%
  summarize(cont_time = sum(cont_time)/60) -> cont_time_byp
cont_time_byp %>%
  group_by(study_site)%>%
  summarise(mean = mean(cont_time, na.rm = T),
            min = min(cont_time, na.rm = T),
            max = max(cont_time, na.rm = T)) 


#Familiarity with contacts *by location*
pa.co%>%
  group_by(location, never_before) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

#Physicality of contacts *by location*
pa.co%>%
  group_by(location, touch_contact) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

#Indoors/outdoors of contacts *by location*
pa.co%>%
  group_by(location, where_contact) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  print(n = 30)


#Calculate proportion of contacts that are with household members for u5s
pa.co.pa.counts  %>%  
  filter(participant_age == "<6mo") %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
pa.co.pa.counts  %>%  
  filter(participant_age == "6-11mo") %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
pa.co.pa.counts  %>%  
  filter(participant_age == "1-4y") %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
pa.co.pa.counts  %>%  
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(hh_membership) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))

#Calculate proportion of contacts by location for u5s
pa.co.pa.counts  %>%  
  filter(participant_age == "<6mo") %>%
  group_by(location) %>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n)) 
pa.co.pa.counts  %>%  
  filter(participant_age == "6-11mo") %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
pa.co.pa.counts  %>%  
  filter(participant_age == "1-4y") %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n)) 
pa.co.pa.counts  %>%  
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(location) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))
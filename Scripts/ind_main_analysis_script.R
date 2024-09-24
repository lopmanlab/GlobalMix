#India
in.pa <- readRDS("./India/ind_participant_data_aim1.RDS")
in.co <- readRDS("./India/ind_contact_data_aim1.RDS")


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
# By sex and site
table(in.pa$participant_sex, in.pa$study_site, useNA = 'always')

#Education
#restrict to non/kids only
in.pa %>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y")) -> in.pa.notkids
nrow(in.pa.notkids) #896
table(in.pa.notkids$high_educ, in.pa.notkids$study_site, useNA = 'always')

#Occupation
#restrict to adults only
in.pa %>%
  subset(!participant_age %in% c("<6mo", "6-11mo", "1-4y", "5-9y")) -> in.pa.adults
nrow(in.pa.adults) #770
table(in.pa.adults$occupation, in.pa.adults$study_site, useNA = 'always')

#HH size by site
table(in.pa$hh_size_cat, in.pa$study_site, useNA = 'always')

in.pa %>%
  group_by(study_site) %>% 
  summarise(mean = mean(hh_size, na.rm = T), 
            median = median(hh_size, na.rm = T), 
            q = list(quantile(hh_size, na.rm = T))) %>%
  unnest_wider(q)

#Number of generations and families in households
in.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id  (one per participant)
  group_by(study_site, hh.gens) %>%
  summarize(freq = n())

in.co%>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>% # select first contact for each rec_id
  group_by(study_site, hh.multfams) %>%
  summarize(freq = n()) 

#######################
# TABLE 2 INPUTS
#######################

#count total contacts
nrow(in.co) #20270

#Number of contacts by site
in.co%>%
  group_by(study_site) %>%
  summarize(freq = n()) 

#Number of contacts by site (DAY 1)
in.co%>%
  group_by(study_site) %>%
  filter(study_day == 1) %>%
  summarize(freq = n()) 

#Number of contacts by site (DAY 2)
in.co%>%
  group_by(study_site) %>%
  filter(study_day == 2) %>%
  summarize(freq = n()) 

#Repeat contacts 
in.co%>%
  group_by(study_site, fromdayone) %>%
  summarize(freq = n()) 

#Contacts with household members
in.co%>%
  group_by(study_site, hh_membership) %>%
  summarize(freq = n()) 

#Physical contacts
in.co%>%
  group_by(study_site, touch_contact) %>%
  summarize(freq = n()) 

#Familiarity with contacts *by study site*
in.co%>%
  group_by(study_site, never_before) %>%
  summarize(freq = n()) 

#Indoor/outdoor contacts
in.co%>%
  group_by(study_site, where_contact) %>%
  summarize(freq = n()) 

#Duration of contacts
in.co%>%
  group_by(study_site, duration_contact) %>%
  summarize(freq = n()) 

#Location of contact
in.co%>%
  group_by(study_site, location) %>%
  summarize(freq = n()) 


#####################
# Join contact with participant data to assess contact patterns by participant characteristics
#####################

# add participant data to contact data
in.co.pa.counts <-  full_join(in.co, in.pa, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1)) #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
# not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.

# count contacts on DAY 1
in.co.pa.counts.d1.only <- in.co.pa.counts %>%
  filter(study_day == 1) %>%
  count(contact, name = "num_contacts")

# Merge contact counts with participant data - DAY 1 ONLY
in.co.pa <- left_join(in.pa, in.co.pa.counts.d1.only, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))


#######################
# SUPPTABLE 2 INPUTS - DAY 1 ONLY
#######################

# Contact overall - DAY 1 ONLY
in.co.pa %>%
  summarise(mean = mean(num_contacts, na.rm = T), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            n = n(), 
            q = list(quantile(num_contacts, na.rm = T))) %>%
  unnest_wider(q)

# Contact by site - DAY 1 ONLY
in.co.pa %>%
  group_by(study_site) %>%
  summarise(mean = mean(num_contacts, na.rm = T),
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            n = n(), 
            q = list(quantile(num_contacts, na.rm = T))) %>%
  unnest_wider(q)

# Contact by site and age - DAY 1 ONLY
in.co.pa %>%
  group_by(study_site, participant_age) %>%
  summarise(mean = round(mean(num_contacts, na.rm = T), 1), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q) 

# Contact by site and sex - DAY 1 ONLY
in.co.pa %>%
  group_by(study_site, participant_sex) %>%
  summarise(mean = round(mean(num_contacts, na.rm = T), 1), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q)

# Contact by site and hh size - DAY 1 ONLY
in.co.pa %>%
  group_by(study_site, hh_size_cat) %>%
  summarise(mean = round(mean(num_contacts, na.rm = T), 1), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q)

# Contact by site and education - DAY 1 ONLY
in.co.pa %>%
  group_by(study_site, high_educ) %>%
  summarise(mean = mean(num_contacts, na.rm = T), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q)

# Contact by site and occupation - DAY 1 ONLY
in.co.pa %>%
  group_by(study_site, occupation) %>%
  summarise(mean = mean(num_contacts, na.rm = T), 
            sd = sd(num_contacts, na.rm = T),
            median = median(num_contacts, na.rm = T), 
            q = list(quantile(num_contacts, na.rm = T)),
            n = n()) %>%
  unnest_wider(q)


######################
# FIGURE 1 
######################

# Figure 1A
#create plot of contact by age - DAY 1 ONLY
in.co.pa %>%
  ggplot(aes(x = participant_age, y = num_contacts, fill=study_site)) + 
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

# Step 1. Create denominator datasets (rural, urban overall)
in.pa%>%
  count(participant_age) %>%
  filter(!is.na(participant_age)) -> o.denoms.byage.in

# Create all combinations of age-age categories
o.denoms.byage.in %>%
  expand(participant_age, participant_age) %>%
  setNames(c("participant_age", "contact_age")) -> allagecombs

#create dfs for contact rate calculation, which include denominators for each age group (rural, urban, overall)
in.co.pa.counts  %>%
  group_by(participant_age, contact_age) %>%
  filter(study_day == 1) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs, by=c("participant_age", "contact_age"))  %>%
  left_join(o.denoms.byage.in, by="participant_age") %>% #added .in
  mutate(num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = num_contacts / n) -> in.co.pa.counts.formatrix.o

# Step 2. Adjust for reciprocity (need to load function, 'adjust_for_reciprocity' from Summary figures and functions.R first)
adjust_for_reciprocity(in.co.pa.counts.formatrix.o, o.denoms.byage.in) -> in.co.pa.counts.formatrix.o.sym

# Step 3. Plot matrices
in.co.pa.counts.formatrix.o.sym  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = c.rate.sym)) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral", limits=c(0, 12), name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = round(c.rate.sym, digits = 1)), color = "white", size = 3) +
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

# Create exposure-hours variable and then plot by location / age
in.co.pa.counts %>%
  filter(study_day==1) %>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/60) %>%
  left_join(o.denoms.byage.in, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n) -> cont_time_byageloc_all

cont_time_byageloc_all %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ggtitle("India") +
  ylim(0, 25) +
  theme_bw() -> conthours.loc.in


# See modeling file for Figure 3 and modeling outputs

######################
# Supplemental figures
######################

# Supplemental figure 1
# Plot of contact duration  by location and age
in.co.pa.counts  %>%  
  filter(!duration_contact == "Unknown") %>%
  ggplot(aes(x = location, fill = duration_contact)) +
  geom_bar(position = "fill", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab("") +
  ylab("Proportion of contacts") +
  ggtitle("India") +
  scale_fill_viridis(option = "G", discrete = TRUE, direction = -1, alpha = 0.9, begin = 0.3, end = 0.9) -> dur.loc.in


# Supplemental figure 2
# Nature and locations of contacts
## Physicality
in.co.pa.counts  %>%  
  subset(!is.na(contact_age)) %>%
  #filter(!location == "Unreported") %>%
  ggplot(aes(x = location, fill = touch_contact)) +
  geom_bar(position="fill", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  ggtitle("India") +
  guides(fill=guide_legend(title="Physical contact")) -> phys.loc.in

## Familiarity
in.co.pa.counts  %>%  
  subset(!is.na(contact_age)) %>%
  filter(!is.na(known_contact)) %>%
  #filter(!location == "Unreported") %>%
  filter(hh_membership == "Non-member") %>%
  ggplot(aes(x = location, fill = known_contact)) +
  geom_bar(position="fill", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  guides(fill=guide_legend(title="Familiarity")) -> known.loc.in

## Indoors/Outdoors
in.co.pa.counts  %>%  
  subset(!is.na(contact_age)) %>%
  #filter(!location == "Unreported") %>%
  ggplot(aes(x = location, fill = where_contact)) +
  geom_bar(position="fill", show.legend = FALSE) +
  scale_x_discrete(limits = c("Home","School","Work", 'Market / essential', "Worship", "Transit", "Other social / leisure"), labels = label_wrap(10)) +
  xlab ("") +
  ylab("") +
  guides(fill=guide_legend(title="Setting"))-> indoor.loc.in


# Supplemental figure 3
# Create exposure-hours with non-hh member variable and then plot by location / age FOR UNDER 5s
in.co.pa.counts %>%
  filter(hh_membership == "Non-member") %>%
  filter(participant_age %in% c("<6mo", "6-11mo", "1-4y")) %>%
  group_by(location, participant_age) %>%
  summarize(cont_time = sum(cont_time)/60) %>%
  left_join(o.denoms.byage.in, by = "participant_age") %>%
  mutate(mean_conthours = cont_time / n)  -> cont_time_byageloc.u5

cont_time_byageloc.u5 %>%
  filter(!location == "Unreported") %>%
  ggplot(aes(x = participant_age, y = mean_conthours, fill = location)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Participant age") +
  ylab("Daily exposure-hours") +
  ylim(0, 16) +
  ggtitle("India") +
  theme_bw() -> conthours.loc.in.u5


# See supp4-8 file for supplemental figures 4-8.

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

# Porportion of the location of contact
in.co.pa.counts %>%
  group_by(location) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) 

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
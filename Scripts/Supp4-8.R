# Supplemental figures 4-8

# Load package
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)
pacman::p_load(dplyr, lubridate, ggplot2, gridExtra, grid, tidyr)

# Read the GM data
moz_participant <- readRDS("./Mozambique/moz_participant_data_aim1.RDS")
moz_contact <- readRDS("./Mozambique/moz_contact_data_aim1.RDS")
ind_participant <- readRDS("./India/ind_participant_data_aim1.RDS")
ind_contact <- readRDS("./India/ind_contact_data_aim1.RDS")
gt_participant <- readRDS("./Guatemala/gt_participant_data_aim1.RDS")
gt_contact <- readRDS("./Guatemala/gt_contact_data_aim1.RDS")
pak_participant <- readRDS("./Pakistan/pak_participant_data_aim1.RDS")
pak_contact <- readRDS("./Pakistan/pak_contact_data_aim1.RDS")

# Read population age structure
mo.we <- read.csv("./Other/moz_pop.csv")
in.we <- read.csv("./Other/ind_pop.csv")
gt.we <- read.csv("./Other/gt_pop.csv")
pa.we <- read.csv("./Other/pak_pop.csv")

# Calculate age weight
mo.pa.we <- moz_participant%>%
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

in.pa.we <- ind_participant%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<1y",
                                     participant_age == "6-11mo" ~ "<1y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

in.we <- in.we%>%
  left_join(in.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)

gt.pa.we <- gt_participant%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

gt.we <- gt.we%>%
  left_join(gt.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)

pa.pa.we <- pak_participant%>%
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

# Read stringency data
stringency <- read.csv("./Other/OxCGRT_compact_national_v1.csv", header = T)
stringency <- stringency%>%
  mutate(Date = ymd(Date))

# Read Prem data
p_contact <- read.csv("./Other/synthetic_contacts_2021.csv", header = T)

###########################
## Supplemental Figure 4 ##
###########################

## Edit the stringency index data
moz_stringency <- stringency%>%
  filter(CountryName == "Mozambique")%>%
  mutate(date_participant_enrolled = ymd(Date))
ind_stringency <- stringency%>%
  filter(CountryName == "India")%>%
  mutate(date_participant_enrolled = ymd(Date))
gt_stringency <- stringency%>%
  filter(CountryName == "Guatemala")%>%
  mutate(date_participant_enrolled = ymd(Date))
pak_stringency <- stringency%>%
  filter(CountryName == "Pakistan")%>%
  mutate(date_participant_enrolled = ymd(Date))


## Mozambique
moz_part_count <- moz_participant%>%
  group_by(date_participant_enrolled)%>%
  summarise(num_part = n())

moz_str_part_plot <- ggplot(moz_part_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = num_part), stat = "identity")+
  geom_line(data = moz_stringency, aes(y = StringencyIndex_Average * (max(moz_part_count$num_part) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(moz_part_count$num_part)))) +
  labs(title = "Mozambique") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.background = element_rect(color = "white"))


## India
ind_part_count <- ind_participant%>%
  group_by(date_participant_enrolled)%>%
  summarise(num_part = n())

ind_str_part_plot <- ggplot(ind_part_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = num_part), stat = "identity")+
  geom_line(data = ind_stringency, aes(y = StringencyIndex_Average * (max(ind_part_count$num_part) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(ind_part_count$num_part)))) +
  labs(title = "India") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.background = element_rect(color = "white"))

## Guatemala
gt_part_count <- gt_participant%>%
  group_by(date_particpant_enrolled)%>%
  summarise(num_part = n())%>%
  rename(date_participant_enrolled = date_particpant_enrolled)

gt_str_part_plot <- ggplot(gt_part_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = num_part), stat = "identity")+
  geom_line(data = gt_stringency, aes(y = StringencyIndex_Average * (max(gt_part_count$num_part) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(gt_part_count$num_part)))) +
  labs(title = "Guatemala") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.background = element_rect(color = "white"))

## Pakistan
pak_part_count <- pak_participant%>%
  group_by(date_participant_enrolled)%>%
  summarise(num_part = n())

pak_str_part_plot <- ggplot(pak_part_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = num_part), stat = "identity")+
  geom_line(data = pak_stringency, aes(y = StringencyIndex_Average * (max(pak_part_count$num_part) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(pak_part_count$num_part)))) +
  labs(title = "Pakistan") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.background = element_rect(color = "white"))


str_plot <- grid.arrange(moz_str_part_plot, gt_str_part_plot, ind_str_part_plot, pak_str_part_plot, ncol = 1, 
                         #top = textGrob("A", x = 0, just = "left", gp = gpar(fontsize = 30)),
                         left = textGrob("Number of participants", rot = 90, just = "centre", gp = gpar(fontsize = 20)),
                         right = textGrob("Stringency Index (Average)", rot = 270, just = "centre", gp = gpar(fontsize = 20)),
                         bottom = textGrob("Participant enrollment date", just = "centre", gp = gpar(fontsize = 20)))

#############################
## Supplemental Figure 5-7 ##
#############################

# School closure
gt_school <- gt_stringency%>%
  mutate(C1M_School.closing = ifelse(C1M_School.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C1M_School.closing != lag(C1M_School.closing, default = first(C1M_School.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C1M_School.closing)

ind_school <- ind_stringency%>%
  mutate(C1M_School.closing = ifelse(C1M_School.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C1M_School.closing != lag(C1M_School.closing, default = first(C1M_School.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C1M_School.closing)

moz_school <- moz_stringency%>%
  mutate(C1M_School.closing = ifelse(C1M_School.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C1M_School.closing != lag(C1M_School.closing, default = first(C1M_School.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C1M_School.closing)

# Workplace closure
gt_work <- gt_stringency%>%
  mutate(C2M_Workplace.closing = ifelse(C2M_Workplace.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C2M_Workplace.closing != lag(C2M_Workplace.closing, default = first(C2M_Workplace.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C2M_Workplace.closing)

ind_work <- ind_stringency%>%
  mutate(C2M_Workplace.closing = ifelse(C2M_Workplace.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C2M_Workplace.closing != lag(C2M_Workplace.closing, default = first(C2M_Workplace.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C2M_Workplace.closing)

moz_work <- moz_stringency%>%
  mutate(C2M_Workplace.closing = ifelse(C2M_Workplace.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C2M_Workplace.closing != lag(C2M_Workplace.closing, default = first(C2M_Workplace.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C2M_Workplace.closing)


# Public events restriction
gt_event <- gt_stringency%>%
  mutate(C3M_Cancel.public.events = ifelse(C3M_Cancel.public.events == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C3M_Cancel.public.events != lag(C3M_Cancel.public.events, default = first(C3M_Cancel.public.events))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C3M_Cancel.public.events)

ind_event <- ind_stringency%>%
  mutate(C3M_Cancel.public.events = ifelse(C3M_Cancel.public.events == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C3M_Cancel.public.events != lag(C3M_Cancel.public.events, default = first(C3M_Cancel.public.events))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C3M_Cancel.public.events)

moz_event <- moz_stringency%>%
  mutate(C3M_Cancel.public.events = ifelse(C3M_Cancel.public.events == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C3M_Cancel.public.events != lag(C3M_Cancel.public.events, default = first(C3M_Cancel.public.events))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C3M_Cancel.public.events)

# Gathering restriction
gt_gather <- gt_stringency%>%
  mutate(C4M_Restrictions.on.gatherings = ifelse(C4M_Restrictions.on.gatherings == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C4M_Restrictions.on.gatherings != lag(C4M_Restrictions.on.gatherings, default = first(C4M_Restrictions.on.gatherings))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C4M_Restrictions.on.gatherings)

ind_gather <- ind_stringency%>%
  mutate(C4M_Restrictions.on.gatherings = ifelse(C4M_Restrictions.on.gatherings == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C4M_Restrictions.on.gatherings != lag(C4M_Restrictions.on.gatherings, default = first(C4M_Restrictions.on.gatherings))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C4M_Restrictions.on.gatherings)

moz_gather <- moz_stringency%>%
  mutate(C4M_Restrictions.on.gatherings = ifelse(C4M_Restrictions.on.gatherings == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C4M_Restrictions.on.gatherings != lag(C4M_Restrictions.on.gatherings, default = first(C4M_Restrictions.on.gatherings))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C4M_Restrictions.on.gatherings)

# Public transportation closure
gt_transport <- gt_stringency%>%
  mutate(C5M_Close.public.transport = ifelse(C5M_Close.public.transport == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C5M_Close.public.transport != lag(C5M_Close.public.transport, default = first(C5M_Close.public.transport))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C5M_Close.public.transport)

ind_transport <- ind_stringency%>%
  mutate(C5M_Close.public.transport = ifelse(C5M_Close.public.transport == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C5M_Close.public.transport != lag(C5M_Close.public.transport, default = first(C5M_Close.public.transport))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C5M_Close.public.transport)

moz_transport <- moz_stringency%>%
  mutate(C5M_Close.public.transport = ifelse(C5M_Close.public.transport == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C5M_Close.public.transport != lag(C5M_Close.public.transport, default = first(C5M_Close.public.transport))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C5M_Close.public.transport)

# Stay-at-home requirement
gt_stayhome <- gt_stringency%>%
  mutate(C6M_Stay.at.home.requirements = ifelse(C6M_Stay.at.home.requirements == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C6M_Stay.at.home.requirements != lag(C6M_Stay.at.home.requirements, default = first(C6M_Stay.at.home.requirements))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C6M_Stay.at.home.requirements)

ind_stayhome <- ind_stringency%>%
  mutate(C6M_Stay.at.home.requirements = ifelse(C6M_Stay.at.home.requirements == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C6M_Stay.at.home.requirements != lag(C6M_Stay.at.home.requirements, default = first(C6M_Stay.at.home.requirements))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C6M_Stay.at.home.requirements)

moz_stayhome <- moz_stringency%>%
  mutate(C6M_Stay.at.home.requirements = ifelse(C6M_Stay.at.home.requirements == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C6M_Stay.at.home.requirements != lag(C6M_Stay.at.home.requirements, default = first(C6M_Stay.at.home.requirements))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  select(Start_date = Date, End_date, Status = C6M_Stay.at.home.requirements)

# Combine all the measurements
gt_restriction <- bind_rows(
  gt_school %>% mutate(type = "School Closure"),
  gt_work %>% mutate(type = "Workplace Closure"),
  gt_event %>% mutate(type = "Event Restrictions"),
  gt_gather %>% mutate(type = "Gathering Restrictions"),
  gt_transport %>% mutate(type = "Transport Closure"),
  gt_stayhome %>% mutate(type = "Stay Home Requirement")
)

ind_restriction <- bind_rows(
  ind_school %>% mutate(type = "School Closure"),
  ind_work %>% mutate(type = "Workplace Closure"),
  ind_event %>% mutate(type = "Event Restrictions"),
  ind_gather %>% mutate(type = "Gathering Restrictions"),
  ind_transport %>% mutate(type = "Transport Closure"),
  ind_stayhome %>% mutate(type = "Stay Home Requirement")
)

moz_restriction <- bind_rows(
  moz_school %>% mutate(type = "School Closure"),
  moz_work %>% mutate(type = "Workplace Closure"),
  moz_event %>% mutate(type = "Event Restrictions"),
  moz_gather %>% mutate(type = "Gathering Restrictions"),
  moz_transport %>% mutate(type = "Transport Closure"),
  moz_stayhome %>% mutate(type = "Stay Home Requirement")
)

# Count the number of contacts
gt_contact_count <- gt_contact%>%
  left_join(gt_participant, by = "rec_id")%>%
  select(-study_site.y)%>%
  rename(study_site = study_site.x)%>%
  group_by(date_particpant_enrolled)%>%
  summarise(contacts = n())%>%
  rename(date_participant_enrolled = date_particpant_enrolled)

ind_contact_count <- ind_contact%>%
  left_join(ind_participant, by = "rec_id")%>%
  select(-study_site.y)%>%
  rename(study_site = study_site.x)%>%
  group_by(date_participant_enrolled)%>%
  summarise(contacts = n())

moz_contact_count <- moz_contact%>%
  left_join(moz_participant, by = "rec_id")%>%
  select(-study_site.y)%>%
  rename(study_site = study_site.x)%>%
  group_by(date_participant_enrolled)%>%
  summarise(contacts = n())

# Overlay the stringency index and pandemic restrictions on the number of contacts

# Supplemental Figure 5
gt_str_date_plot <- ggplot(gt_contact_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = contacts), stat = "identity")+
  geom_line(data = gt_stringency, aes(y = StringencyIndex_Average * (max(gt_contact_count$contacts) / 100)), color = "red")+
  geom_rect(data = gt_restriction %>% filter(Status == 1),
            aes(xmin = pmax(as.Date("2021-01-01"), Start_date), xmax = End_date, ymin = 0, ymax = max(gt_contact_count$contacts), fill = type),
            alpha = 0.1, inherit.aes = FALSE) +
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    name = "Number of contacts",
    sec.axis = sec_axis(~ . * (100 / max(gt_contact_count$contacts)), name = "Stringency Index (Average)")) +
  labs(title = "Guatemala") +
  xlab("Participant enrollment date")+
  ylab("Number of contacts")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "none",
        plot.background = element_rect(color = "white"))+
  facet_wrap(~type)

# Supplemental Figure 6
ind_str_date_plot <- ggplot(ind_contact_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = contacts), stat = "identity")+
  geom_line(data = ind_stringency, aes(y = StringencyIndex_Average * (max(ind_contact_count$contacts) / 100)), color = "red")+
  geom_rect(data = ind_restriction %>% filter(Status == 1),
            aes(xmin = pmax(as.Date("2021-01-01"), Start_date), xmax = End_date, ymin = 0, ymax = max(ind_contact_count$contacts), fill = type),
            alpha = 0.1, inherit.aes = FALSE) +
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    name = "Number of contacts",
    sec.axis = sec_axis(~ . * (100 / max(ind_contact_count$contacts)), name = "Stringency Index (Average)")) +
  labs(title = "India") +
  xlab("Participant enrollment date")+
  ylab("Number of contacts")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "none",
        plot.background = element_rect(color = "white"))+
  facet_wrap(~type)

# Supplemental Figure 7
moz_str_date_plot <- ggplot(moz_contact_count, aes(x = date_participant_enrolled))+
  geom_bar(aes(y = contacts), stat = "identity")+
  geom_line(data = moz_stringency, aes(y = StringencyIndex_Average * (max(moz_contact_count$contacts) / 100)), color = "red")+
  geom_rect(data = moz_restriction %>% filter(Status == 1),
            aes(xmin = pmax(as.Date("2021-01-01"), Start_date), xmax = End_date, ymin = 0, ymax = max(moz_contact_count$contacts), fill = type),
            alpha = 0.1, inherit.aes = FALSE) +
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    name = "Number of contacts",
    sec.axis = sec_axis(~ . * (100 / max(moz_contact_count$contacts)), name = "Stringency Index (Average)")) +
  labs(title = "Mozambique",
       fill = "Policy") +
  xlab("Participant enrollment date")+
  ylab("Number of contacts")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "none",
        plot.background = element_rect(color = "white"))+
  facet_wrap(~type)


###########################
## Supplemental Figure 8 ##
###########################

# Modify the Prem data
p_moz_mod <- p_contact%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "all")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")))

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

p_gt_mod <- p_contact%>%
  filter(iso3c == "GTM" & setting == "overall" & location_contact == "all")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")))

p_pak_mod <- p_contact%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "all")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")))

# Create a table
# Prem
## Mozambique
p_moz_table <- p_moz_mod%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "Mozambique")

## India
p_ind_table <- p_ind_mod%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "India")

## Guatemala
p_gt_table <- p_gt_mod%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "Guatemala")

## Pakistan
p_pak_table <- p_pak_mod%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "Pakistan")

# Table of the contact rates of four countries from Prem dataset
p_age_table <- rbind(p_ind_table, p_gt_table, p_moz_table, p_pak_table)%>%
  pivot_wider(names_from = country, values_from = contact_rate)%>%
  mutate(participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-14y", "15-19y",
                                                              "20-24y", "25-29y", "30-34y", "35-39y",
                                                              "40-44y", "45-49y", "50-54y", "55-59y",
                                                              "60-64y", "65-69y", "70-74y", "75-79y")))%>%
  arrange(participant_age)

# GlobalMix
## Mozamqbiue
gm_moz_table_prep <- moz_contact%>%
  left_join(moz_participant, by = "rec_id")%>%
  filter(participant_age == "<6mo"|participant_age == "6-11mo"|participant_age == "1-4y")%>%
  mutate(participant_age = "0-4y")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())

gm_moz_table <- moz_contact%>%
  left_join(moz_participant, by = "rec_id")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  filter(participant_age %in% c("5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())%>%
  bind_rows(gm_moz_table_prep)%>%
  mutate(lower_ci = contact_rate - 1.96 * sd/sqrt(n), # Lower bound of 95% CI
         upper_ci = contact_rate + 1.96 * sd/sqrt(n), # Upper bound of 95% CI
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  arrange(participant_age)%>%
  mutate(country = "Mozambique")

## India
gm_ind_table_prep <- ind_contact%>%
  left_join(ind_participant, by = "rec_id")%>%
  filter(participant_age == "<6mo"|participant_age == "6-11mo"|participant_age == "1-4y")%>%
  mutate(participant_age = "0-4y")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())

gm_ind_table <- ind_contact%>%
  left_join(ind_participant, by = "rec_id")%>%
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

## Guatemala
gm_gt_table_prep <- gt_contact%>%
  left_join(gt_participant, by = "rec_id")%>%
  filter(participant_age == "<6mo"|participant_age == "6-11mo"|participant_age == "1-4y")%>%
  mutate(participant_age = "0-4y")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())

gm_gt_table <- gt_contact%>%
  left_join(gt_participant, by = "rec_id")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  filter(participant_age %in% c("5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())%>%
  bind_rows(gm_gt_table_prep)%>%
  mutate(lower_ci = contact_rate - 1.96 * sd/sqrt(n), # Lower bound of 95% CI
         upper_ci = contact_rate + 1.96 * sd/sqrt(n), # Upper bound of 95% CI
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  arrange(participant_age)%>%
  mutate(country = "Guatemala")


## Pakistan
gm_pak_table_prep <- pak_contact%>%
  left_join(pak_participant, by = "rec_id")%>%
  filter(participant_age == "<6mo"|participant_age == "6-11mo"|participant_age == "1-4y")%>%
  mutate(participant_age = "0-4y")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())

gm_pak_table <- pak_contact%>%
  left_join(pak_participant, by = "rec_id")%>%
  group_by(rec_id, participant_age)%>%
  summarise(num_contacts = n())%>%
  filter(participant_age %in% c("5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(num_contacts/2),
            sd = sd(num_contacts/2, na.rm = T),
            n = n())%>%
  bind_rows(gm_pak_table_prep)%>%
  mutate(lower_ci = contact_rate - 1.96 * sd/sqrt(n), # Lower bound of 95% CI
         upper_ci = contact_rate + 1.96 * sd/sqrt(n), # Upper bound of 95% CI
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  arrange(participant_age)%>%
  mutate(country = "Pakistan")

## Plot in a line graph
# The age category is different, so take a midpoint of the age

# Convert age categories to midpoints
get_midpoint <- function(participant_age) {
  age_range <- strsplit(gsub("[^0-9-]", "", participant_age), "-")[[1]]
  if (length(age_range) == 1) {
    return(as.numeric(age_range[1]) + 1)  # for "60+y", assuming 60 is the start
  } else {
    return(mean(as.numeric(age_range)))
  }
}

# Apply to both datasets
p_ind_table$age_midpoint <- sapply(p_ind_table$participant_age, get_midpoint)
p_moz_table$age_midpoint <- sapply(p_moz_table$participant_age, get_midpoint)
p_gt_table$age_midpoint <- sapply(p_gt_table$participant_age, get_midpoint)
p_pak_table$age_midpoint <- sapply(p_pak_table$participant_age, get_midpoint)
gm_ind_table$age_midpoint <- sapply(gm_ind_table$participant_age, get_midpoint)
gm_moz_table$age_midpoint <- sapply(gm_moz_table$participant_age, get_midpoint)
gm_gt_table$age_midpoint <- sapply(gm_gt_table$participant_age, get_midpoint)
gm_pak_table$age_midpoint <- sapply(gm_pak_table$participant_age, get_midpoint)

# Add a source column to distinguish between the datasets
p_ind_table <- p_ind_table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)
p_moz_table <- p_moz_table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)
p_gt_table <- p_gt_table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)
p_pak_table <- p_pak_table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)
gm_ind_table <- gm_ind_table%>%
  mutate(dataset = "Current study")%>%
  select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)
gm_moz_table<- gm_moz_table%>%
  mutate(dataset = "Current study")%>%
  select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)
gm_gt_table <- gm_gt_table%>%
  mutate(dataset = "Current study")%>%
  select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)
gm_pak_table<- gm_pak_table%>%
  mutate(dataset = "Current study")%>%
  select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)

# Combine the datasets
ind_age_table <- rbind(p_ind_table, gm_ind_table)
moz_age_table <- rbind(p_moz_table, gm_moz_table)
gt_age_table <- rbind(p_gt_table, gm_gt_table)
pak_age_table <- rbind(p_pak_table, gm_pak_table)

# Plot the data
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

gt_age_plot <- ggplot(gt_age_table, aes(x = age_midpoint, y = contact_rate, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 0.7) +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  scale_y_continuous("Contact Rate") +
  ylim(0,25)+
  labs(title = "Guatemala",
       color = "Dataset") +
  ylab("Contact Rate")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))

moz_age_plot <- ggplot(moz_age_table, aes(x = age_midpoint, y = contact_rate, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 0.7) +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  scale_y_continuous("Contact Rate") +
  ylim(0,25)+
  labs(title = "Mozambique",
       color = "Dataset") +
  ylab("Contact Rate")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))

pak_age_plot <- ggplot(pak_age_table, aes(x = age_midpoint, y = contact_rate, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 0.7) +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  scale_y_continuous("Contact Rate") +
  ylim(0,25)+
  labs(title = "Pakistan",
       color = "Dataset") +
  ylab("Contact Rate")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))

title_grob <- textGrob("A",
                       gp = gpar(fontsize = 30, fontface = "bold"),
                       just = "left",
                       x = 0.02,
                       y = 0.5)

age_line_plot <- grid.arrange(ind_age_plot, gt_age_plot,moz_age_plot, pak_age_plot,ncol = 2, top = title_grob)



# Panel B
# Prepare Prem data
p_moz_school <- p_contact%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_ind_school <- p_contact%>%
  filter(iso3c == "IND" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_gt_school <- p_contact%>%
  filter(iso3c == "GTM" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_school <- p_contact%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

p_moz_work <- p_contact%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "work")%>%
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
p_gt_work <- p_contact%>%
  filter(iso3c == "GTM" & setting == "overall" & location_contact == "work")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_work <- p_contact%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "work")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

p_moz_home <- p_contact%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "home")%>%
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
p_gt_home <- p_contact%>%
  filter(iso3c == "GTM" & setting == "overall" & location_contact == "home")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_home <- p_contact%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "home")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

p_moz_other <- p_contact%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "others")%>%
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
p_gt_other <- p_contact%>%
  filter(iso3c == "GTM" & setting == "overall" & location_contact == "others")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_other <- p_contact%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "others")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

# Combine the location
## India
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

## Guatemala
p_gt_home_lab <- p_gt_home%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Home")
p_gt_school_lab <- p_gt_school%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "School")
p_gt_work_lab <- p_gt_work%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Work")
p_gt_other_lab <- p_gt_other%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Other")

p_gt_loc <- rbind(p_gt_home_lab, p_gt_school_lab, p_gt_work_lab, p_gt_other_lab)%>%
  group_by(location)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(contact_rate_tot = sum(contact_rate),
         percentage = contact_rate/contact_rate_tot,
         country = "Guatemala")

## Mozambique
p_moz_home_lab <- p_moz_home%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Home")
p_moz_school_lab <- p_moz_school%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "School")
p_moz_work_lab <- p_moz_work%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Work")
p_moz_other_lab <- p_moz_other%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Other")

p_moz_loc <- rbind(p_moz_home_lab, p_moz_school_lab, p_moz_work_lab, p_moz_other_lab)%>%
  group_by(location)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(contact_rate_tot = sum(contact_rate),
         percentage = contact_rate/contact_rate_tot,
         country = "Mozambique")

## Pakistan
p_pak_home_lab <- p_pak_home%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Home")
p_pak_school_lab <- p_pak_school%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "School")
p_pak_work_lab <- p_pak_work%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Work")
p_pak_other_lab <- p_pak_other%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Other")

p_pak_loc <- rbind(p_pak_home_lab, p_pak_school_lab, p_pak_work_lab, p_pak_other_lab)%>%
  group_by(location)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(contact_rate_tot = sum(contact_rate),
         percentage = contact_rate/contact_rate_tot,
         country = "Pakistan")

# Combine four country
p_com_loc <- rbind(p_moz_loc, p_ind_loc, p_gt_loc, p_pak_loc)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other")),
         dataset = "Prem et al., 2021")

# GlobalMix
moz_location <- moz_contact%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(moz_participant, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo.we, by = c("participant_age", "study_site"))%>%
  mutate(country = "Mozambique",
         rec_id = as.character(rec_id))

ind_location <- ind_contact%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(ind_participant, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<1y",
                                     participant_age == "6-11mo" ~ "<1y",
                                     TRUE ~ participant_age))%>%
  left_join(in.we, by = c("participant_age", "study_site"))%>%
  mutate(country = "India")

gt_location <- gt_contact%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(gt_participant, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gt.we, by = c("participant_age", "study_site"))%>%
  mutate(country = "Guatemala")

pak_location <- pak_contact%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(pak_participant, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pa.we, by = c("participant_age", "study_site"))%>%
  mutate(country = "Pakistan")

gm_com_loc <- rbind(moz_location, ind_location, gt_location, pak_location)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other", "Unreported")))%>%
  filter(!is.na(psweight))%>%
  as_survey(weights = c(psweight))%>%
  group_by(country, location)%>%
  summarise(count = survey_total())%>%
  mutate(percentage = count/sum(count),
         dataset = "Current study")

# Combine the two datasets
gm_com_loc_2 <- gm_com_loc%>%
  select(country, location, percentage, dataset)
p_com_loc_2 <- p_com_loc%>%
  select(country, location, percentage, dataset)

loc_combined <- rbind(gm_com_loc_2, p_com_loc_2)%>%
  mutate(country = factor(country, levels = c("India", "Guatemala", "Mozambique", "Pakistan")))

# Plot it
loc_combined_plot <- ggplot(loc_combined, aes(x = dataset, y = percentage, fill = location))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~ country, nrow = 1) +
  labs(y = "Proportion", x = "Dataset", fill = "Contact location", title = "B") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0, size = 30, face = "bold"))+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20))

# Two-panel plot
multi_plot <- grid.arrange(age_line_plot, loc_combined_plot, ncol = 2)

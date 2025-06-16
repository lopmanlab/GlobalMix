# Supplemental figures 4-7

# Load package
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)
library(survey)
library(srvyr)
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


# Supp table 8 old code #

# Table of the contact rates of four countries from Prem dataset
# p_age_table <- rbind(p_ind_table, p_gt_table, p_moz_table, p_pak_table)%>%
#   pivot_wider(names_from = country, values_from = contact_rate)%>%
#   mutate(participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-14y", "15-19y",
#                                                               "20-24y", "25-29y", "30-34y", "35-39y",
#                                                               "40-44y", "45-49y", "50-54y", "55-59y",
#                                                               "60-64y", "65-69y", "70-74y", "75-79y")))%>%
#   arrange(participant_age)




# Supplemental contact matrices comparison
# Modify the Prem data
# Take weighted mean for aggregating age groups
pop <- read.csv("./Other/popage_total2020.csv")%>%
  rename(country = Region..subregion..country.or.area..,
         year = Reference.date..as.of.1.July.)%>%
  filter(country == "Mozambique"|country == "India"| country == "Guatemala"|country == "Pakistan")

moz_pop <- pop%>%
  filter(country == "Mozambique")%>%
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


p_moz_o <- p_moz_mod%>%
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
  left_join(moz_pop, by = c("participant_age" = "age_group"))%>%
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
p_moz_o  %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(contact_rate_we, 8.5))) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(contact_rate_we, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.1) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) -> p.mat.mo

# GlobalMix data
mo.co.pa.counts.7 <-  full_join(moz_contact, moz_participant, 
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
moz_participant%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "0-4y",
                                     participant_age == "6-11mo" ~ "0-4y",
                                     participant_age == "1-4y" ~ "0-4y",
                                     TRUE ~ participant_age),
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  count(participant_age) %>%
  filter(!is.na(participant_age)) -> o.denoms.byage.mo.7

# Create all combinations of age-age categories
o.denoms.byage.mo.7 %>% 
  tidyr::expand(participant_age, participant_age) %>%
  setNames(c("participant_age", "contact_age")) -> allagecombs.7

#create dfs for contact rate calculation, which include denominators for each age group
mo.co.pa.counts.7  %>%
  group_by(participant_age, contact_age) %>%
  count(participant_age, contact_age, name = "num_contacts") %>%
  right_join(allagecombs.7, by=c("participant_age", "contact_age"))  %>%
  left_join(o.denoms.byage.mo.7, by="participant_age") %>%
  mutate(num_contacts = num_contacts/2,
         num_contacts = replace_na(num_contacts, 0)) %>%
  arrange(participant_age, contact_age) %>%
  mutate(c.rate = (num_contacts) / n) -> mo.co.pa.counts.formatrix.o.7

# Step 2. Adjust for reciprocity
adjust_for_reciprocity(mo.co.pa.counts.formatrix.o.7, o.denoms.byage.mo.7, 7) -> mo.co.pa.counts.formatrix.o.sym.7

# Step 3. Plot matrices
mo.co.pa.counts.formatrix.o.sym.7  %>%  
  subset(!is.na(contact_age)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill = pmin(c.rate.sym, 3.5))) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "daily contacts") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = round(c.rate.sym, digits = 1)), 
                  color = "black", 
                  bg.color = "white", 
                  size = 3, 
                  bg.r = 0.1) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = label_wrap(10)) -> mat.mo.o.sym.7

# Supplemental figures 9-12

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
## Supplemental Figure 9 ##
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
## Supplemental Figure 10-12 ##
#############################

# School closure
gt_school <- gt_stringency%>%
  mutate(C1M_School.closing = ifelse(C1M_School.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C1M_School.closing != lag(C1M_School.closing, default = first(C1M_School.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C1M_School.closing)

ind_school <- ind_stringency%>%
  mutate(C1M_School.closing = ifelse(C1M_School.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C1M_School.closing != lag(C1M_School.closing, default = first(C1M_School.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C1M_School.closing)

moz_school <- moz_stringency%>%
  mutate(C1M_School.closing = ifelse(C1M_School.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C1M_School.closing != lag(C1M_School.closing, default = first(C1M_School.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C1M_School.closing)

# Workplace closure
gt_work <- gt_stringency%>%
  mutate(C2M_Workplace.closing = ifelse(C2M_Workplace.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C2M_Workplace.closing != lag(C2M_Workplace.closing, default = first(C2M_Workplace.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C2M_Workplace.closing)

ind_work <- ind_stringency%>%
  mutate(C2M_Workplace.closing = ifelse(C2M_Workplace.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C2M_Workplace.closing != lag(C2M_Workplace.closing, default = first(C2M_Workplace.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C2M_Workplace.closing)

moz_work <- moz_stringency%>%
  mutate(C2M_Workplace.closing = ifelse(C2M_Workplace.closing == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C2M_Workplace.closing != lag(C2M_Workplace.closing, default = first(C2M_Workplace.closing))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C2M_Workplace.closing)


# Public events restriction
gt_event <- gt_stringency%>%
  mutate(C3M_Cancel.public.events = ifelse(C3M_Cancel.public.events == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C3M_Cancel.public.events != lag(C3M_Cancel.public.events, default = first(C3M_Cancel.public.events))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C3M_Cancel.public.events)

ind_event <- ind_stringency%>%
  mutate(C3M_Cancel.public.events = ifelse(C3M_Cancel.public.events == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C3M_Cancel.public.events != lag(C3M_Cancel.public.events, default = first(C3M_Cancel.public.events))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C3M_Cancel.public.events)

moz_event <- moz_stringency%>%
  mutate(C3M_Cancel.public.events = ifelse(C3M_Cancel.public.events == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C3M_Cancel.public.events != lag(C3M_Cancel.public.events, default = first(C3M_Cancel.public.events))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C3M_Cancel.public.events)

# Gathering restriction
gt_gather <- gt_stringency%>%
  mutate(C4M_Restrictions.on.gatherings = ifelse(C4M_Restrictions.on.gatherings == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C4M_Restrictions.on.gatherings != lag(C4M_Restrictions.on.gatherings, default = first(C4M_Restrictions.on.gatherings))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C4M_Restrictions.on.gatherings)

ind_gather <- ind_stringency%>%
  mutate(C4M_Restrictions.on.gatherings = ifelse(C4M_Restrictions.on.gatherings == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C4M_Restrictions.on.gatherings != lag(C4M_Restrictions.on.gatherings, default = first(C4M_Restrictions.on.gatherings))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C4M_Restrictions.on.gatherings)

moz_gather <- moz_stringency%>%
  mutate(C4M_Restrictions.on.gatherings = ifelse(C4M_Restrictions.on.gatherings == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C4M_Restrictions.on.gatherings != lag(C4M_Restrictions.on.gatherings, default = first(C4M_Restrictions.on.gatherings))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C4M_Restrictions.on.gatherings)

# Public transportation closure
gt_transport <- gt_stringency%>%
  mutate(C5M_Close.public.transport = ifelse(C5M_Close.public.transport == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C5M_Close.public.transport != lag(C5M_Close.public.transport, default = first(C5M_Close.public.transport))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C5M_Close.public.transport)

ind_transport <- ind_stringency%>%
  mutate(C5M_Close.public.transport = ifelse(C5M_Close.public.transport == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C5M_Close.public.transport != lag(C5M_Close.public.transport, default = first(C5M_Close.public.transport))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C5M_Close.public.transport)

moz_transport <- moz_stringency%>%
  mutate(C5M_Close.public.transport = ifelse(C5M_Close.public.transport == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C5M_Close.public.transport != lag(C5M_Close.public.transport, default = first(C5M_Close.public.transport))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C5M_Close.public.transport)

# Stay-at-home requirement
gt_stayhome <- gt_stringency%>%
  mutate(C6M_Stay.at.home.requirements = ifelse(C6M_Stay.at.home.requirements == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C6M_Stay.at.home.requirements != lag(C6M_Stay.at.home.requirements, default = first(C6M_Stay.at.home.requirements))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(gt_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C6M_Stay.at.home.requirements)

ind_stayhome <- ind_stringency%>%
  mutate(C6M_Stay.at.home.requirements = ifelse(C6M_Stay.at.home.requirements == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C6M_Stay.at.home.requirements != lag(C6M_Stay.at.home.requirements, default = first(C6M_Stay.at.home.requirements))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(ind_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C6M_Stay.at.home.requirements)

moz_stayhome <- moz_stringency%>%
  mutate(C6M_Stay.at.home.requirements = ifelse(C6M_Stay.at.home.requirements == 0, 0, 1),
         Date = ymd(Date))%>%
  mutate(Status_change = C6M_Stay.at.home.requirements != lag(C6M_Stay.at.home.requirements, default = first(C6M_Stay.at.home.requirements))) %>%
  filter(Status_change | row_number() == 1) %>% # Include the first period
  mutate(End_date = if_else(is.na(lead(Date)), max(moz_stringency$Date), lead(Date) - 1)) %>% # Adjust End_date to be the day before the next Start_date
  dplyr::select(Start_date = Date, End_date, Status = C6M_Stay.at.home.requirements)

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
  dplyr::select(-study_site.y)%>%
  rename(study_site = study_site.x)%>%
  group_by(date_particpant_enrolled)%>%
  summarise(contacts = n())%>%
  rename(date_participant_enrolled = date_particpant_enrolled)

ind_contact_count <- ind_contact%>%
  left_join(ind_participant, by = "rec_id")%>%
  dplyr::select(-study_site.y)%>%
  rename(study_site = study_site.x)%>%
  group_by(date_participant_enrolled)%>%
  summarise(contacts = n())

moz_contact_count <- moz_contact%>%
  left_join(moz_participant, by = "rec_id")%>%
  dplyr::select(-study_site.y)%>%
  rename(study_site = study_site.x)%>%
  group_by(date_participant_enrolled)%>%
  summarise(contacts = n())

# Overlay the stringency index and pandemic restrictions on the number of contacts

# Supplemental Figure 10
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

# Supplemental Figure 11
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

# Supplemental Figure 12
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
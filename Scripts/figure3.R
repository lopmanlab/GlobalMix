# ------------------------------------------------------------------------------
# Figure 3
# ------------------------------------------------------------------------------

# ---------- Load libraries 
library(ggplot2)
library(patchwork)
library(dplyr)
library(readr)
library(ggtext)


# ---------- Load dataset
df.ve <- read_csv("./Other/ve_5sep2024.csv") 
df.ve$Age.Group <- as.factor(df.ve$Age.Group)
df.ve$Age.Group <- factor(df.ve$Age.Group, levels=c("<6 months", "6-11 months", "1-4 years", "5-9 years", "10-19 years",
                                                    "20-29 years", "30-39 years", "40-59 years", "60+ years"))

# ---------- Make subsets 
# --- infant vaccine strategy 
df.lt1 <- df.ve %>%
  filter(Strategy == "lt1")

# by country
df.lt1.moz <- df.lt1 %>%
  filter(Country=="Mozambique")
df.lt1.gt <- df.lt1 %>%
  filter(Country=="Guatemala")
df.lt1.ind <- df.lt1 %>%
  filter(Country=="India")
df.lt1.pak <- df.lt1 %>%
  filter(Country=="Pakistan")

# by site 
df.lt1.rur.moz <- df.lt1.moz %>%
  filter(Site=="Rural")
df.lt1.rur.gt <- df.lt1.gt %>%
  filter(Site=="Rural")
df.lt1.rur.ind <- df.lt1.ind %>%
  filter(Site=="Rural")
df.lt1.rur.pak <- df.lt1.pak %>%
  filter(Site=="Rural")

df.lt1.urb.moz <- df.lt1.moz %>%
  filter(Site=="Urban")
df.lt1.urb.gt <- df.lt1.gt %>%
  filter(Site=="Urban")
df.lt1.urb.ind <- df.lt1.ind %>%
  filter(Site=="Urban")
df.lt1.urb.pak <- df.lt1.pak %>%
  filter(Site=="Urban")

# ---- adolescent and you adult vaccine strategy 
df.aya <- df.ve %>%
  filter(Strategy == "aya")

# by country
df.aya.moz <- df.aya %>%
  filter(Country=="Mozambique")
df.aya.gt <- df.aya %>%
  filter(Country=="Guatemala")
df.aya.ind <- df.aya %>%
  filter(Country=="India")
df.aya.pak <- df.aya %>%
  filter(Country=="Pakistan")

# by site 
df.aya.rur.moz <- df.aya.moz %>%
  filter(Site=="Rural")
df.aya.rur.gt <- df.aya.gt %>%
  filter(Site=="Rural")
df.aya.rur.ind <- df.aya.ind %>%
  filter(Site=="Rural")
df.aya.rur.pak <- df.aya.pak %>%
  filter(Site=="Rural")

df.aya.urb.moz <- df.aya.moz %>%
  filter(Site=="Urban")
df.aya.urb.gt <- df.aya.gt %>%
  filter(Site=="Urban")
df.aya.urb.ind <- df.aya.ind %>%
  filter(Site=="Urban")
df.aya.urb.pak <- df.aya.pak %>%
  filter(Site=="Urban")


# ---------- Plots for infant vaccine strategy 
lt1.rur.moz <- ggplot(df.lt1.rur.moz, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(
    subtitle = "Mozambique",
    y = NULL,
    x = NULL,
    fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(c(0,50))
lt1.rur.gt <- ggplot(df.lt1.rur.gt, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "<b>Infant Vaccination</b><br>Rural Site",
       subtitle = "Guatemala",
       x = NULL,
       y = "Vaccine Effect (%)",
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_markdown())+ ylim(c(0,50))
lt1.rur.ind <- ggplot(df.lt1.rur.ind, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "India",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ ylim(c(0,50))
lt1.rur.pak <- ggplot(df.lt1.rur.pak, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "Pakistan",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ ylim(c(0,50))
lt1.urb.moz <- ggplot(df.lt1.urb.moz, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    subtitle = "Mozambique",
    y = NULL,
    x = NULL,
    fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ ylim(c(0,50))
lt1.urb.gt <- ggplot(df.lt1.urb.gt, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Urban Site",
       subtitle = "Guatemala",
       y = "Vaccine Effect (%)",
       x = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_markdown())+ ylim(c(0,50))
lt1.urb.ind <- ggplot(df.lt1.urb.ind, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "India",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ ylim(c(0,50))
lt1.urb.pak <- ggplot(df.lt1.urb.pak, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "Pakistan",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ ylim(c(0,50))



# ---------- Plots for adolescents and young adults vaccine strategy 

aya.rur.moz <- ggplot(df.aya.rur.moz, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    subtitle = "Mozambique",
    y = NULL,
    x = NULL,
    fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(c(0,50))
aya.rur.gt <- ggplot(df.aya.rur.gt, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "<b>Adolescent and Young Adult Vaccination</b><br>Rural Site",
       subtitle = "Guatemala",
       y = "Vaccine Effect (%)",
       x = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_markdown()) + ylim(c(0,50))
aya.rur.ind <- ggplot(df.aya.rur.ind, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "India",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(c(0,50))
aya.rur.pak <- ggplot(df.aya.rur.pak, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "Pakistan",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(c(0,50))
aya.urb.moz <- ggplot(df.aya.urb.moz, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    subtitle = "Mozambique",
    y = NULL,
    x = NULL,
    fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") + ylim(c(0,50))
aya.urb.gt <- ggplot(df.aya.urb.gt, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Urban Site",
       subtitle = "Guatemala",
       y = "Vaccine Effect (%)",
       x = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_markdown()) + ylim(c(0,50))
aya.urb.ind <- ggplot(df.aya.urb.ind, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "India",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + ylim(c(0,50))
aya.urb.pak <- ggplot(df.aya.urb.pak, aes(x = Age.Group, y = Value_50_50, fill = Vaccine.Effects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(subtitle = "Pakistan",
       x = NULL,
       y = NULL,
       fill = "Effect Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ ylim(c(0,50))


# ---------- Compile all plots
lt1.rur.gt + lt1.rur.ind + lt1.rur.moz + lt1.rur.pak + 
  lt1.urb.gt + lt1.urb.ind + lt1.urb.moz + lt1.urb.pak + 
  aya.rur.gt + aya.rur.ind + aya.rur.moz + aya.rur.pak + 
  aya.urb.gt + aya.urb.ind + aya.urb.moz + aya.urb.pak + plot_layout(ncol=4)
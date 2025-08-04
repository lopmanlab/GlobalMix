# Packages needed for data wrangling
library(dplyr)
library(tidyr)
library(cowplot)
library(ggpubr)
library(viridis)
library(lubridate)
library(reshape2)
library(ggplot2)
library(scales)
library(srvyr)
library(survey)
library(shadowtext)
library(gridExtra)
library(grid)
library(here)
library(plotly)
library(lme4)
require(MASS)
library(lmerTest)

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Function for making age-structured matrices symmetric

# New function - matching the equation with socialmixr
adjust_for_reciprocity <- function(df, denoms, numframe) {
  
  #filter NA values
  df %>%
    filter(!is.na(contact_age)) -> df.no.na
  
  #create matrix
  matrix(df.no.na$c.rate, numframe, numframe) -> mat
  
  #create indexing
  n <- nrow(mat)
  t <- as.numeric(denoms$n)
  
  # Adjust for reciprocity
  for (i in 1:n) {
    for (j in 1:n) {
      mat[i, j] <- ((mat[i,j]*t[i] + mat[j,i]*t[j])/(2*t[i])) 
      
      # add row and column names for re-appending to df
      rownames(mat) <- as.factor(unique(df$participant_age))
      colnames(mat) <- as.factor(unique(df$participant_age))
    }
  }
  
  #melt matrix back to single column to join with df
  mat %>%
    reshape2::melt() %>%
    rename(c.rate.sym = value,
           participant_age = Var1,
           contact_age = Var2) -> sym.mat
  
  #join w df
  left_join(sym.mat, df, by = c("participant_age", "contact_age")) -> sym.df
  
  #return df with unsym and NEW sym contact rates
  return(sym.df)
  
}


# Function to convert age categories to midpoints
get_midpoint <- function(participant_age) {
  age_range <- strsplit(gsub("[^0-9-]", "", participant_age), "-")[[1]]
  if (length(age_range) == 1) {
    return(as.numeric(age_range[1]) + 1)  # for "60+y", assuming 60 is the start
  } else {
    return(mean(as.numeric(age_range)))
  }
}

##########################################################################
##########################################################################
##########################################################################
##########################################################################


# CREATE FOUR-COUNTRY SUMMARY FIGURES

########################
# FIGURE 1
########################

ggarrange(
  ggarrange(gt.co.pa.byage.plot, in.co.pa.byage.plot, mo.co.pa.byage.plot, pa.co.pa.byage.plot, ncol = 4, labels = "A"),
  ggarrange(mat.gt.o.sym, mat.in.o.sym, mat.mo.o.sym, mat.pa.o.sym, ncol = 4, labels = "B"),
  ggarrange(loc.gt, loc.in, loc.mo, loc.pa, ncol = 4, labels = "C"),
  nrow = 3
) -> fig1

########################
# FIGURE 2
########################

ggarrange(conthours.loc.gt.u5, conthours.loc.in.u5, conthours.loc.mo.u5, conthours.loc.pa.u5,
          nrow = 2, ncol = 2) -> fig2

########################
# FIGURE 3
########################

# See modeling file

########################
# FIGURE 4
########################
# Panel A
title_grob <- textGrob("A",
                       gp = gpar(fontsize = 30, fontface = "bold"),
                       just = "left",
                       x = 0.02,
                       y = 0.5)

age_line_plot <- grid.arrange(gt_age_plot, ind_age_plot,moz_age_plot, pak_age_plot,ncol = 2, top = title_grob)

# Panel B
# Combine four country
p_com_loc <- rbind(p_moz_loc, p_ind_loc, p_gt_loc, p_pak_loc)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other")),
         dataset = "Prem et al., 2021")
gm_com_loc <- rbind(moz_location, ind_location, gt_location, pak_location)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other", "Unreported")))%>%
  filter(!is.na(psweight))%>%
  as_survey(weights = c(psweight))%>%
  group_by(country, location)%>%
  summarise(count = survey_total())%>%
  mutate(percentage = count/sum(count),
         dataset = "GlobalMix")

# Combine the two datasets
gm_com_loc_2 <- gm_com_loc%>%
  dplyr::select(country, location, percentage, dataset)
p_com_loc_2 <- p_com_loc%>%
  dplyr::select(country, location, percentage, dataset)

loc_combined <- rbind(gm_com_loc_2, p_com_loc_2)%>%
  mutate(country = factor(country, levels = c("Guatemala", "India",  "Mozambique", "Pakistan")))

# Plot
loc_combined_plot <- ggplot(loc_combined, aes(x = dataset, y = percentage, fill = location))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~ country, nrow = 1) +
  labs(y = "Proportion", x = "Dataset", fill = "Contact location", title = "B") +
  scale_fill_manual(values = c("Home"="#F8766D",
                               "Market / essential" = "#E69F00",
                               "Other" = "#7CAE00",
                               "School" = "#00BFC4",
                               "Transit" = "#56B4E9",
                               "Work" = "#C77CFF",
                               "Worship" = "#F564E3",
                               "Unreported" = "#8B4513"))+
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

########################
# SUPPLEMENTAL FIGURES
########################

# Supp 1 - Site-specific contact matrix
ggarrange(
  ggarrange(mat.gt.r.sym, mat.in.r.sym, mat.mo.r.sym, mat.pa.r.sym, ncol = 4, labels = "Rural", label.y = 1.0),
  ggarrange(mat.gt.u.sym, mat.in.u.sym, mat.mo.u.sym, mat.pa.u.sym, ncol = 4, labels = "Urban", label.y = 1.0),
  nrow = 2
) -> sfig1


# Supp 2 - Location of contact by site
ggarrange(
  ggarrange(loc.gt.r, loc.in.r, loc.mo.r, loc.pa.r, ncol = 4, labels = "Rural", label.x = 0.01, label.y = 1.0),
  ggarrange(loc.gt.u, loc.in.u, loc.mo.u, loc.pa.u, ncol = 4, labels = "Urban", label.x = 0.01, label.y = 1.0),
  nrow = 2
) -> sfig2


# Supp 3 - Locations and duration of contact by location by age 
ggarrange(dur.loc.gt, dur.loc.in, dur.loc.mo, dur.loc.pa,
          nrow = 2, ncol = 2,
          common.legend = T, legend = "right") -> sfig3


# Supp 4 - Exposure-hours of all contact
ggarrange(conthours.loc.all.gt, conthours.loc.all.in, conthours.loc.all.mo, conthours.loc.all.pa,
          nrow = 2, ncol = 2) -> sfig4

# Supp 5 - Comparison of contact matrix
ggarrange(
  ggarrange(mat.gt.o.sym.7, mat.in.o.sym.7, mat.mo.o.sym.7, mat.pa.o.sym.7, labels = "GlobalMix", ncol = 4, nrow = 1, label.x = 0.05, label.y = 1.0),
  ggarrange(p.mat.gt, p.mat.in, p.mat.mo, p.mat.pa, ncol = 4, nrow = 1, labels = "Prem et al., 2021", label.x = 0.01, label.y = 1.0), nrow = 2) -> sfig5


#####################
# RESULTS TEXT INPUT
#####################

# Mean contact hours
combined <- rbind(gt.co.pa.counts%>%mutate(country = "G"), in.co.pa.counts%>%mutate(country = "I"), mo.co.pa.counts%>%mutate(country = "M"), pa.co.pa.counts%>%mutate(country = "P"))%>%
  filter(!is.na(duration_contact))
combined%>%
  group_by(location)%>%
  summarise(cont_time = sum(cont_time)/60,
            n = n())%>%
  mutate(mean_cont_time = cont_time/n)

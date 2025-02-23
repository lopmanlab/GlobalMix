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

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Function for making age-structured matrices symmetric

# Function to adjust for reciprocity, works for 9x9 matrices
adjust_for_reciprocity <- function(df, denoms) {
  
  #filter NA values, so get 9x9 matrix
  df %>%
    filter(!is.na(contact_age)) -> df.no.na
  
  #create matrix
  matrix(df.no.na$num_contacts, 9, 9) -> mat
  
  #create indexing
  n <- nrow(mat)
  t <- as.numeric(denoms$n)
  
  # # Ensure the matrix is square
  # if (n != ncol(matrix)) {
  #   stop("Input matrix is not square.")
  # }
  
  # Adjust for reciprocity
  for (i in 1:n) {
    for (j in 1:n) {
      # do not adjust the diagonal
      # if (i != j) {
      mat[i, j] <- ((mat[i, j] + mat[j, i]) / 
                      (df$n[i] + df$n[j]))
      # }
      
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
fig2


########################
# FIGURE 3
########################

# See modeling file


########################
# SUPP FIGURES
########################


# Sup 1 - Locations and duration of contact by location by age 
ggarrange(dur.loc.gt, dur.loc.in, dur.loc.mo, dur.loc.pa,
          nrow = 2, ncol = 2,
          common.legend = T, legend = "right") -> sfig1
sfig1

# Sup 2
ggarrange(conthours.loc.all.gt, conthours.loc.all.in, conthours.loc.all.mo, conthours.loc.all.pa,
          nrow = 2, ncol = 2) -> sfig2
sfig2

# Sup 3 - High-risk contacts
ggarrange(hr.loc.gt, hr.loc.in, hr.loc.mo, hr.loc.pa, nrow = 2, ncol = 2, common.legend = T, legend = "right") -> sfig3

# For supplemental figures 4-8, see supp4-8 file

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


##########################################################################
##########################################################################
##########################################################################
##########################################################################

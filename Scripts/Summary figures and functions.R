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

ggarrange(gt.co.pa.byage.plot, in.co.pa.byage.plot, mo.co.pa.byage.plot, pa.co.pa.byage.plot, 
          mat.gt.o.sym,  mat.in.o.sym, mat.mo.o.sym, mat.pa.o.sym,
          loc.gt, loc.in, loc.mo, loc.pa,
          nrow = 3, ncol = 4) -> fig1
fig1


########################
# FIGURE 2
########################

ggarrange(conthours.loc.gt, conthours.loc.in, conthours.loc.mo, conthours.loc.pa,
          nrow = 2, ncol = 2) -> fig2
fig2


########################
# FIGURE 3
########################

# See modeling file


########################
# SUPP FIGURES
########################


# Locations and duration of contact by location by age 
ggarrange(dur.loc.gt, dur.loc.in, dur.loc.mo, dur.loc.pa,
          nrow = 2, ncol = 2) -> sfig1
sfig1

# Type of contact by location
ggarrange(phys.loc.gt, phys.loc.in, phys.loc.mo, phys.loc.pa,
          known.loc.gt, known.loc.in, known.loc.mo, known.loc.pa,
          indoor.loc.gt, indoor.loc.in, indoor.loc.mo, indoor.loc.pa,
          nrow = 3, ncol = 4) -> sfig2
sfig2

#Contact by location with non-hh members for kids u5
ggarrange(conthours.loc.gt.u5, conthours.loc.in.u5, conthours.loc.mo.u5, conthours.loc.pa.u5,
          nrow = 2, ncol = 2) -> sfig3
sfig3


# sfig4-8: see supp4-8 file

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# ------------------------------------------------------------------------------
# 
# GlobalMix incidence by site
# Simple, non-calibrated SIR
# 
# Sara Kim
# October 24, 2024
#
# ------------------------------------------------------------------------------

# --- Library
library(EpiModel)
library(ggplot2)
library(patchwork)
library(ggtext)
library(tidyr)
library(dplyr)
library(EpiEstim)


# --- Model structure
sirmod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # 1. track total sizes of each age group
    num.1 <- s.num.1 + i.num.1 + r.num.1
    num.2 <- s.num.2 + i.num.2 + r.num.2
    num.3 <- s.num.3 + i.num.3 + r.num.3
    num.4 <- s.num.4 + i.num.4 + r.num.4
    num.5 <- s.num.5 + i.num.5 + r.num.5
    num.6 <- s.num.6 + i.num.6 + r.num.6
    num.7 <- s.num.7 + i.num.7 + r.num.7
    num.8 <- s.num.8 + i.num.8 + r.num.8
    num.9 <- s.num.9 + i.num.9 + r.num.9
    
    # 2. define age-specific forces of infection
    # force of infection for <6m0 contacts (infecting the <60mo year olds)

    lambda.1 <- q*c11*((i.num.1)/num.1) + 
      q*c12*((i.num.2)/num.2) + # c12 is contact between <6mo with 6-11mo
      q*c13*((i.num.3)/num.3) +  # c13 is contact between <6mo with 1-4 y/o, etc
      q*c14*((i.num.4)/num.4) +
      q*c15*((i.num.5)/num.5) + 
      q*c16*((i.num.6)/num.6) + 
      q*c17*((i.num.7)/num.7) + 
      q*c18*((i.num.8)/num.8) + 
      q*c19*((i.num.9)/num.9)
    
    
    # force of infection for 6-11mo contacts (infecting the 6-11 mo)
    lambda.2 <- q*c21*((i.num.1)/num.1) + 
      q*c22*((i.num.2)/num.2) + 
      q*c23*((i.num.3)/num.3) + 
      q*c24*((i.num.4)/num.4) +
      q*c25*((i.num.5)/num.5) + 
      q*c26*((i.num.6)/num.6) +
      q*c27*((i.num.7)/num.7) +
      q*c28*((i.num.8)/num.8) +
      q*c29*((i.num.9)/num.9)
  
    
    # force of infection for 1-4 y contacts (infecting the 1-4y )
    lambda.3 <- q*c31*((i.num.1)/num.1) + 
      q*c32*((i.num.2)/num.2) + 
      q*c33*((i.num.3)/num.3) + 
      q*c34*((i.num.4)/num.4) +
      q*c35*((i.num.5)/num.5) + 
      q*c36*((i.num.6)/num.6) +
      q*c37*((i.num.7)/num.7) +
      q*c38*((i.num.8)/num.8) +
      q*c39*((i.num.9)/num.9)
 
    
    # force of infection for 5-9y contacts (infecting the 5-9y)
    lambda.4 <- q*c41*((i.num.1)/num.1) + 
      q*c42*((i.num.2)/num.2) + 
      q*c43*((i.num.3)/num.3) + 
      q*c44*((i.num.4)/num.4) +
      q*c45*((i.num.5)/num.5) + 
      q*c46*((i.num.6)/num.6) +
      q*c47*((i.num.7)/num.7) +
      q*c48*((i.num.8)/num.8) +
      q*c49*((i.num.9)/num.9)
    
    
    # force of infection for 10-19 contacts (infecting the 10-19 year olds)
    lambda.5 <- q*c51*((i.num.1)/num.1) + 
      q*c52*((i.num.2)/num.2) + 
      q*c53*((i.num.3)/num.3) + 
      q*c54*((i.num.4)/num.4) +
      q*c55*((i.num.5)/num.5) + 
      q*c56*((i.num.6)/num.6) +
      q*c57*((i.num.7)/num.7) +
      q*c58*((i.num.8)/num.8) +
      q*c59*((i.num.9)/num.9)
    
    
    # force of infection for 20-29y contacts (infecting the 20-29 year olds)
    lambda.6 <- q*c61*((i.num.1)/num.1) + 
      q*c62*((i.num.2)/num.2) + 
      q*c63*((i.num.3)/num.3) + 
      q*c64*((i.num.4)/num.4) +
      q*c65*((i.num.5)/num.5) + 
      q*c66*((i.num.6)/num.6) +
      q*c67*((i.num.7)/num.7) +
      q*c68*((i.num.8)/num.8) +
      q*c69*((i.num.9)/num.9)
    
    
    # force of infection for 30-39y contacts (infecting the 30-39 year olds)
    lambda.7 <- q*c71*((i.num.1)/num.1) + 
      q*c72*((i.num.2)/num.2) + 
      q*c73*((i.num.3)/num.3) + 
      q*c74*((i.num.4)/num.4) +
      q*c75*((i.num.5)/num.5) + 
      q*c76*((i.num.6)/num.6) +
      q*c77*((i.num.7)/num.7) +
      q*c78*((i.num.8)/num.8) +
      q*c79*((i.num.9)/num.9)
  
    
    # force of infection for 40-59y contacts (infecting the 40-59 year olds)
    lambda.8 <- q*c81*((i.num.1)/num.1) + 
      q*c82*((i.num.2)/num.2) + 
      q*c83*((i.num.3)/num.3) + 
      q*c84*((i.num.4)/num.4) +
      q*c85*((i.num.5)/num.5) + 
      q*c86*((i.num.6)/num.6) +
      q*c87*((i.num.7)/num.7) +
      q*c88*((i.num.8)/num.8) +
      q*c89*((i.num.9)/num.9)
    
    
    # force of infection for 60+y contacts (infecting the 60+ year olds)
    lambda.9 <- q*c91*((i.num.1)/num.1) + 
      q*c92*((i.num.2 )/num.2) + 
      q*c93*((i.num.3)/num.3) + 
      q*c94*((i.num.4)/num.4) +
      q*c95*((i.num.5)/num.5) + 
      q*c96*((i.num.6)/num.6) +
      q*c97*((i.num.7)/num.7) +
      q*c98*((i.num.8)/num.8) +
      q*c99*((i.num.9)/num.9)
    
    
    # 3. differential equations 
    dS.1 <- -lambda.1*s.num.1
    dI.1 <- lambda.1*s.num.1 - gamma*i.num.1
    dR.1 <- gamma*i.num.1

    
    dS.2 <- -lambda.2*s.num.2
    dI.2 <- lambda.2*s.num.2 - gamma*i.num.2
    dR.2 <- gamma*i.num.2
    
    dS.3 <- -lambda.3*s.num.3
    dI.3 <- lambda.3*s.num.3 - gamma*i.num.3
    dR.3 <- gamma*i.num.3
    
    dS.4 <- -lambda.4*s.num.4
    dI.4 <- lambda.4*s.num.4 - gamma*i.num.4
    dR.4 <- gamma*i.num.4
    
    dS.5 <- -lambda.5*s.num.5
    dI.5 <- lambda.5*s.num.5 - gamma*i.num.5
    dR.5 <- gamma*i.num.5
    
    dS.6 <- -lambda.6*s.num.6
    dI.6 <- lambda.6*s.num.6 - gamma*i.num.6
    dR.6 <- gamma*i.num.6
    
    dS.7 <- -lambda.7*s.num.7
    dI.7 <- lambda.7*s.num.7 - gamma*i.num.7
    dR.7 <- gamma*i.num.7
    
    dS.8 <- -lambda.8*s.num.8
    dI.8 <- lambda.8*s.num.8 - gamma*i.num.8
    dR.8 <- gamma*i.num.8
    
    dS.9 <- -lambda.9*s.num.9
    dI.9 <- lambda.9*s.num.9 - gamma*i.num.9
    dR.9 <- gamma*i.num.9
    
    
    # 4. List outputs
    list(c(dS.1, dI.1, dR.1, 
           dS.2, dI.2, dR.2, 
           dS.3, dI.3, dR.3, 
           dS.4, dI.4, dR.4,
           dS.5, dI.5, dR.5,
           dS.6, dI.6, dR.6, 
           dS.7, dI.7, dR.7, 
           dS.8, dI.8, dR.8, 
           dS.9, dI.9, dR.9, 
           si.flow.1 = lambda.1*s.num.1,
           si.flow.2 = lambda.2*s.num.2,
           si.flow.3 = lambda.3*s.num.3,
           si.flow.4 = lambda.4*s.num.4,
           si.flow.5 = lambda.5*s.num.5,
           si.flow.6 = lambda.6*s.num.6,
           si.flow.7 = lambda.7*s.num.7,
           si.flow.8 = lambda.8*s.num.8,
           si.flow.9 = lambda.9*s.num.9,
           si.flow = lambda.1*s.num.1 + lambda.2*s.num.2 + lambda.3*s.num.3 + lambda.4*s.num.4 + 
             lambda.5*s.num.5 + lambda.6*s.num.6 + lambda.7*s.num.7 + lambda.8*s.num.8 + lambda.9*s.num.9
    ))
  })
}

# ---------- Guatemala specific parameters and conditions ---------- # 

# --- Parameters
# - Urban
param.urban.gt <- param.dcm(gamma = 1/7, q = 0.015, 
                         c11=0.10, c12=0.00, c13=0.88, c14=1.33, c15=2.37, c16=2.88, c17=1.92, c18=1.75, c19=0.63,
                         c21=0.00, c22=0.02, c23=0.71, c24=1.41, c25=2.57, c26=2.93, c27=2.60, c28=1.83, c29=0.48,
                         c31=0.02, c32=0.03, c33=1.00, c34=2.20, c35=1.80, c36=2.58, c37=1.91, c38=1.86, c39=1.11,
                         c41=0.00, c42=0.00, c43=0.81, c44=3.21, c45=2.81, c46=1.88, c47=2.02, c48=1.56, c49=0.52,
                         c51=0.00, c52=0.00, c53=0.26, c54=0.94, c55=5.16, c56=1.73, c57=1.82, c58=2.78, c59=0.91,
                         c61=0.07, c62=0.16, c63=0.54, c64=0.70, c65=1.70, c66=3.56, c67=2.34, c68=2.82, c69=1.10,
                         c71=0.00, c72=0.00, c73=0.38, c74=1.09, c75=2.23, c76=1.91, c77=2.64, c78=2.66, c79=1.23,
                         c81=0.03, c82=0.03, c83=0.16, c84=0.67, c85=2.34, c86=2.33, c87=1.89, c88=3.47, c89=1.38,
                         c91=0.01, c92=0.03, c93=0.15, c94=0.22, c95=0.79, c96=1.67, c97=1.88, c98=3.24, c99=2.51)
# - Rural
param.rural.gt <- param.dcm(gamma = 1/7, q = 0.015, 
                         c11=0.00, c12=0.00, c13=1.27, c14=1.41, c15=2.34, c16=2.56, c17=1.85, c18=1.80, c19=0.61,
                         c21=0.00, c22=0.00, c23=0.56, c24=1.29, c25=2.82, c26=2.71, c27=1.96, c28=2.02, c29=0.56,
                         c31=0.02, c32=0.02, c33=1.85, c34=2.60, c35=2.16, c36=2.45, c37=1.85, c38=1.73, c39=0.51,
                         c41=0.06, c42=0.00, c43=0.82, c44=4.43, c45=2.82, c46=1.42, c47=2.38, c48=1.52, c49=0.58,
                         c51=0.05, c52=0.04, c53=0.52, c54=1.07, c55=5.62, c56=1.67, c57=2.29, c58=2.25, c59=0.65,
                         c61=0.06, c62=0.03, c63=0.55, c64=1.41, c65=2.27, c66=2.98, c67=1.83, c68=3.38, c69=0.84,
                         c71=0.00, c72=0.11, c73=0.32, c74=1.00, c75=2.26, c76=1.82, c77=2.89, c78=3.06, c79=1.21,
                         c81=0.00, c82=0.00, c83=0.31, c84=0.67, c85=2.16, c86=2.64, c87=1.93, c88=3.79, c89=1.59,
                         c91=0.00, c92=0.02, c93=0.27, c94=0.72, c95=1.09, c96=1.67, c97=2.00, c98=3.23, c99=2.44)

# --- Initial conditions
# - Urban
init.urb.gt <- init.dcm(s.num.1 = 240000, i.num.1 = 1,  r.num.1 = 0,
                     s.num.2 = 210000, i.num.2 = 1,  r.num.2 = 0, 
                     s.num.3 = 1090000, i.num.3 = 1,  r.num.3 = 0, 
                     s.num.4 = 1290000, i.num.4 = 1,  r.num.4 = 0, 
                     s.num.5 = 2170000, i.num.5 = 1,  r.num.5 = 0, 
                     s.num.6 = 1780000, i.num.6 = 1, r.num.6 = 0, 
                     s.num.7 = 1290000, i.num.7 = 1,  r.num.7 = 0, 
                     s.num.8 = 1190000, i.num.8 = 1,  r.num.8 = 0, 
                     s.num.9 = 740000, i.num.9 = 1, r.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)
# - Rural
init.rur.gt <- init.dcm(s.num.1 = 220000, i.num.1 = 1, r.num.1 = 0, 
                     s.num.2 = 190000, i.num.2 = 1,  r.num.2 = 0,
                     s.num.3 = 1000000, i.num.3 = 1,  r.num.3 = 0, 
                     s.num.4 = 1180000, i.num.4 = 1,  r.num.4 = 0, 
                     s.num.5 = 2010000, i.num.5 = 1,  r.num.5 = 0, 
                     s.num.6 = 1640000, i.num.6 = 1,  r.num.6 = 0, 
                     s.num.7 = 1180000, i.num.7 = 1,  r.num.7 = 0, 
                     s.num.8 = 1090000, i.num.8 = 1, r.num.8 = 0, 
                     s.num.9 = 680000, i.num.9 = 1, r.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)

#--- Controls 
control <- control.dcm(nstep=730, new.mod = sirmod)

# --- Compile model GlobalMix urban
sim.no.urb.gt <- dcm(param.urban.gt, init.urb.gt, control)
df.no.urb.gt <- as.data.frame(sim.no.urb.gt)

# --- Compile model GlobalMix rural
sim.no.rur.gt <- dcm(param.rural.gt, init.rur.gt, control)
df.no.rur.gt <- as.data.frame(sim.no.rur.gt)



# ---------- India specific parameters and conditions ---------- #

# --- Parameters
# - Urban
param.urban.ind <- param.dcm(gamma = 1/7, q = 0.015, 
                         c11=0.15, c12=0.04, c13=1.07, c14=0.51, c15=1.02, c16=3.55, c17=1.93, c18=2.60, c19=0.82,
                         c21=0.07, c22=0.07, c23=1.09, c24=0.96, c25=0.96, c26=3.76, c27=2.42, c28=2.47, c29=0.80,
                         c31=0.22, c32=0.22, c33=2.38, c34=1.37, c35=1.14, c36=3.17, c37=2.42, c38=1.98, c39=0.82,
                         c41=0.03, c42=0.03, c43=0.92, c44=6.89, c45=2.89, c46=1.57, c47=3.65, c48=2.29, c49=0.86,
                         c51=0.06, c52=0.00, c53=0.30, c54=0.77, c55=9.66, c56=1.55, c57=2.86, c58=2.62, c59=0.71,
                         c61=0.06, c62=0.16, c63=1.03, c64=0.68, c65=1.54, c66=5.52, c67=2.11, c68=2.83, c69=0.78,
                         c71=0.10, c72=0.06, c73=0.92, c74=1.16, c75=2.25, c76=2.33, c77=4.37, c78=2.97, c79=0.87,
                         c81=0.03, c82=0.00, c83=0.80, c84=0.50, c85=1.73, c86=2.69, c87=3.16, c88=5.42, c89=1.00,
                         c91=0.03, c92=0.00, c93=0.47, c94=0.58, c95=1.16, c96=2.27, c97=2.24, c98=4.23, c99=2.08)
# - Rural
param.rural.ind <- param.dcm(gamma = 1/7, q = 0.015, 
                         c11=0.31, c12=0.10, c13=0.79, c14=0.34, c15=1.10, c16=2.76, c17=2.07, c18=2.52, c19=1.09,
                         c21=0.07, c22=0.00, c23=0.35, c24=0.20, c25=0.98, c26=2.24, c27=1.35, c28=2.28, c29=1.06,
                         c31=0.26, c32=0.13, c33=2.25, c34=1.64, c35=1.08, c36=2.33, c37=3.15, c38=2.54, c39=1.82,
                         c41=0.08, c42=0.11, c43=1.56, c44=6.60, c45=2.98, c46=1.05, c47=3.51, c48=2.08, c49=1.51,
                         c51=0.03, c52=0.03, c53=0.50, c54=0.90, c55=10.06, c56=1.82, c57=2.49, c58=3.39, c59=1.56,
                         c61=0.06, c62=0.00, c63=0.65, c64=0.63, c65=1.53, c66=4.58, c67=2.39, c68=3.74, c69=1.42,
                         c71=0.03, c72=0.03, c73=1.11, c74=1.05, c75=2.05, c76=1.35, c77=3.70, c78=3.40, c79=1.98,
                         c81=0.06, c82=0.01, c83=0.51, c84=0.55, c85=1.38, c86=2.17, c87=2.32, c88=4.46, c89=1.89,
                         c91=0.00, c92=0.00, c93=0.24, c94=0.60, c95=1.13, c96=0.84, c97=1.89, c98=4.08, c99=2.37)

# --- Initial conditions
# - Urban
init.urb.ind <- init.dcm(s.num.1 = 10900000,i.num.1 = 1, r.num.1 = 0, 
                     s.num.2 = 10900000, i.num.2 = 1,r.num.2 = 0, 
                     s.num.3 = 44700000, i.num.3 = 1, r.num.3 = 0, 
                     s.num.4 = 59600000, i.num.4 = 1, r.num.4 = 0, 
                     s.num.5 = 109300000, i.num.5 = 1, r.num.5 = 0, 
                     s.num.6 = 89500000, i.num.6 = 1, r.num.6 = 0, 
                     s.num.7 = 64600000, i.num.7 = 1, r.num.7 = 0, 
                     s.num.8 = 69600000, i.num.8 = 1, r.num.8 = 0,  
                     s.num.9 = 39800000, i.num.9 = 1, r.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)
# - Rural
init.rur.ind <- init.dcm(s.num.1 = 20300000, i.num.1 = 1, r.num.1 = 0, 
                     s.num.2 = 20300000, i.num.2 = 1, r.num.2 = 0, 
                     s.num.3 = 83100000, i.num.3 = 1,  r.num.3 = 0, 
                     s.num.4 = 110800000, i.num.4 = 1,  r.num.4 = 0, 
                     s.num.5 = 203100000, i.num.5 = 1,  r.num.5 = 0, 
                     s.num.6 = 166100000, i.num.6 = 1,  r.num.6 = 0, 
                     s.num.7 = 120000000,  i.num.7 = 1,  r.num.7 = 0, 
                     s.num.8 = 129200000, i.num.8 = 1,  r.num.8 = 0, 
                     s.num.9 = 73800000, i.num.9 = 1, r.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)

# --- Compile model GlobalMix urban
sim.no.urb.ind <- dcm(param.urban.ind, init.urb.ind, control)
df.no.urb.ind <- as.data.frame(sim.no.urb.ind)

# --- Compile model GlobalMix rural
sim.no.rur.ind <- dcm(param.rural.ind, init.rur.ind, control)
df.no.rur.ind <- as.data.frame(sim.no.rur.ind)


# ---------- Mozambique specific parameters and conditions ---------- #

# --- Parameters
# - Urban
param.urban.moz <- param.dcm(gamma = 1/7, q = 0.015,
                         c11=0.02, c12=0.05, c13=0.38, c14=0.80, c15=1.39, c16=1.30, c17=1.09, c18=0.48, c19=0.33,
                         c21=0.02, c22=0.08, c23=0.97, c24=1.22, c25=1.88, c26=1.95, c27=1.52, c28=0.80, c29=0.20,
                         c31=0.00, c32=0.01, c33=2.36, c34=2.38, c35=1.82, c36=1.99, c37=0.96, c38=0.76, c39=0.25,
                         c41=0.00, c42=0.03, c43=1.03, c44=4.09, c45=2.14, c46=1.55, c47=0.84, c48=0.34, c49=0.12,
                         c51=0.00, c52=0.03, c53=0.61, c54=1.43, c55=7.93, c56=2.16, c57=1.11, c58=0.78, c59=0.19,
                         c61=0.03, c62=0.03, c63=0.61, c64=0.93, c65=1.64, c66=3.66, c67=1.79, c68=1.05, c69=0.21,
                         c71=0.07, c72=0.13, c73=0.72, c74=0.72, c75=1.67, c76=2.25, c77=3.16, c78=1.52, c79=0.16,
                         c81=0.01, c82=0.07, c83=0.34, c84=0.68, c85=1.36, c86=1.99, c87=1.93, c88=2.60, c89=0.40,
                         c91=0.02, c92=0.07, c93=0.30, c94=0.72, c95=0.95, c96=1.46, c97=1.31, c98=1.85, c99=1.20)
# - Rural
param.rural.moz <- param.dcm(gamma = 1/7, q = 0.015,
                         c11=0.00, c12=0.03, c13=1.23, c14=2.31, c15=3.55, c16=2.18, c17=1.29, c18=1.31, c19=0.52,
                         c21=0.01, c22=0.05, c23=1.82, c24=2.88, c25=4.10, c26=2.35, c27=1.34, c28=1.35, c29=0.38,
                         c31=0.02, c32=0.19, c33=2.46, c34=3.76, c35=3.87, c36=1.73, c37=1.13, c38=0.92, c39=0.27,
                         c41=0.12, c42=0.19, c43=2.25, c44=7.11, c45=4.47, c46=0.91, c47=0.66, c48=0.53, c49=0.36,
                         c51=0.04, c52=0.08, c53=0.83, c54=2.38, c55=10.98, c56=2.04, c57=1.34, c58=0.94, c59=0.42,
                         c61=0.06, c62=0.05, c63=0.83, c64=1.22, c65=4.81, c66=5.61, c67=2.28, c68=2.17, c69=0.47,
                         c71=0.05, c72=0.02, c73=0.95, c74=1.48, c75=2.33, c76=3.31, c77=3.45, c78=3.22, c79=0.83,
                         c81=0.02, c82=0.07, c83=0.65, c84=1.16, c85=2.99, c86=3.16, c87=3.00, c88=4.64, c89=1.15,
                         c91=0.05, c92=0.03, c93=0.60, c94=0.79, c95=1.60, c96=1.65, c97=1.59, c98=3.30, c99=1.59)


# --- Initial conditions
# - Urban
init.urb.moz <- init.dcm(s.num.1 = 300000, i.num.1 = 1, r.num.1 = 0, 
                     s.num.2 = 240000, i.num.2 = 1, r.num.2 = 0, 
                     s.num.3 = 1590000, i.num.3 = 1, r.num.3 = 0, 
                     s.num.4 = 1830000, i.num.4 = 1, r.num.4 = 0, 
                     s.num.5 = 3050000, i.num.5 = 1, r.num.5 = 0, 
                     s.num.6 = 2200000, i.num.6 = 1, r.num.6 = 0, 
                     s.num.7 = 1470000, i.num.7 = 1, r.num.7 = 0, 
                     s.num.8 = 1220000, i.num.8 = 1, r.num.8 = 0, 
                     s.num.9 = 610000, i.num.9 = 1, r.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)

# - Rural
init.rur.moz <- init.dcm(s.num.1 = 520000, i.num.1 = 1, r.num.1 = 0,
                     s.num.2 = 420000, i.num.2 = 1, r.num.2 = 0,
                     s.num.3 = 2700000, i.num.3 = 1, r.num.3 = 0,
                     s.num.4 = 3120000, i.num.4 = 1, r.num.4 = 0, 
                     s.num.5 = 5200000, i.num.5 = 1, r.num.5 = 0,
                     s.num.6 = 3740000, i.num.6 = 1, r.num.6 = 0,
                     s.num.7 = 2500000, i.num.7 = 1, r.num.7 = 0, 
                     s.num.8 = 2080000, i.num.8 = 1, r.num.8 = 0,
                     s.num.9 = 1040000, i.num.9 = 1, r.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)

# --- Compile model GlobalMix urban
sim.no.urb.moz <- dcm(param.urban.moz, init.urb.moz, control)
df.no.urb.moz <- as.data.frame(sim.no.urb.moz)

# --- Compile model GlobalMix rural
sim.no.rur.moz <- dcm(param.rural.moz, init.rur.moz, control)
df.no.rur.moz <- as.data.frame(sim.no.rur.moz)


# ---------- Pakistan specific parameters and conditions ---------- #

# --- Parameters
# - Urban
param.urban.pak <- param.dcm(gamma = 1/7, q = 0.015, 
                         c11=0.02, c12=0.02, c13=0.89, c14=1.72, c15=1.98, c16=3.10, c17=2.30, c18=1.38, c19=0.39,
                         c21=0.04, c22=0.00, c23=1.11, c24=2.06, c25=2.68, c26=3.17, c27=2.66, c28=1.36, c29=0.30,
                         c31=0.00, c32=0.12, c33=1.86, c34=2.62, c35=2.32, c36=2.66, c37=2.30, c38=1.24, c39=0.24,
                         c41=0.14, c42=0.06, c43=1.52, c44=5.10, c45=3.79, c46=2.49, c47=3.30, c48=1.48, c49=0.32,
                         c51=0.05, c52=0.06, c53=0.67, c54=2.02, c55=7.30, c56=3.34, c57=3.95, c58=2.90, c59=0.44,
                         c61=0.09, c62=0.15, c63=0.97, c64=1.36, c65=2.66, c66=5.69, c67=4.87, c68=3.10, c69=0.46,
                         c71=0.03, c72=0.08, c73=1.54, c74=1.81, c75=4.05, c76=4.27, c77=8.34, c78=4.81, c79=0.65,
                         c81=0.15, c82=0.12, c83=0.82, c84=1.22, c85=2.84, c86=3.64, c87=4.48, c88=4.97, c89=0.51,
                         c91=0.00, c92=0.06, c93=0.43, c94=0.71, c95=3.63, c96=3.24, c97=4.14, c98=5.96, c99=2.20)
# - Rural
param.rural.pak <- param.dcm(gamma = 1/7, q = 0.015, 
                         c11=0.01, c12=0.04, c13=1.80, c14=2.90, c15=4.82, c16=4.04, c17=3.55, c18=2.80, c19=1.16,
                         c21=0.00, c22=0.00, c23=1.91, c24=3.38, c25=5.38, c26=5.06, c27=3.13, c28=3.09, c29=1.11,
                         c31=0.18, c32=0.31, c33=4.98, c34=6.67, c35=5.93, c36=5.18, c37=5.22, c38=3.36, c39=1.51,
                         c41=0.11, c42=0.45, c43=3.37, c44=8.58, c45=8.29, c46=4.95, c47=5.18, c48=4.37, c49=1.63,
                         c51=0.13, c52=0.26, c53=1.77, c54=3.55, c55=9.88, c56=5.76, c57=3.85, c58=4.85, c59=1.15,
                         c61=0.19, c62=0.19, c63=1.97, c64=2.65, c65=4.79, c66=6.21, c67=4.24, c68=4.65, c69=1.22,
                         c71=0.16, c72=0.08, c73=2.13, c74=3.51, c75=5.43, c76=5.16, c77=6.41, c78=5.08, c79=2.02,
                         c81=0.05, c82=0.20, c83=1.68, c84=3.04, c85=6.21, c86=4.97, c87=4.29, c88=5.22, c89=1.77,
                         c91=0.22, c92=0.17, c93=2.51, c94=2.73, c95=5.11, c96=4.81, c97=4.65, c98=5.59, c99=2.57)

# --- Initial conditions
# - Urban
init.urb.pak <- init.dcm(s.num.1 = 2160000, i.num.1 = 1, r.num.1 = 0,  
                     s.num.2 = 1990000, i.num.2 = 1, r.num.2 = 0, 
                     s.num.3 = 11230000, i.num.3 = 1, r.num.3 = 0, 
                     s.num.4 = 12960000, i.num.4 = 1, r.num.4 = 0, 
                     s.num.5 = 19010000, i.num.5 = 1, r.num.5 = 0, 
                     s.num.6 = 15550000, i.num.6 = 1, r.num.6 = 0,  
                     s.num.7 = 10370000, i.num.7 = 1, r.num.7 = 0, 
                     s.num.8 = 8640000, i.num.8 = 1, r.num.8 = 0,
                     s.num.9 = 5620000, i.num.9 = 1, r.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)
# - Rural
init.rur.pak <- init.dcm(s.num.1 = 3840000, i.num.1 = 1, r.num.1 = 0, 
                     s.num.2 = 3530000, i.num.2 = 1, r.num.2 = 0, 
                     s.num.3 = 19970000, i.num.3 = 1, r.num.3 = 0, 
                     s.num.4 = 23040000, i.num.4 = 1, r.num.4 = 0, 
                     s.num.5 = 33790000, i.num.5 = 1, r.num.5 = 0, 
                     s.num.6 = 27650000, i.num.6 = 1, r.num.6 = 0,
                     s.num.7 = 18430000, i.num.7 = 1, r.num.7 = 0, 
                     s.num.8 = 15360000, i.num.8 = 1, r.num.8 = 0, 
                     s.num.9 = 9980000, i.num.9 = 1, r.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, si.flow = 0)

# --- Compile model GlobalMix urban
sim.no.urb.pak <- dcm(param.urban.pak, init.urb.pak, control)
df.no.urb.pak <- as.data.frame(sim.no.urb.pak)

# --- Compile model GlobalMix rural
sim.no.rur.pak <- dcm(param.rural.pak, init.rur.pak, control)
df.no.rur.pak <- as.data.frame(sim.no.rur.pak)



# ---------- Calculate risk for each age group x site ---------- #

# Guatemala rural
gt.rur.1 <- sum(df.no.rur.gt$si.flow.1) / init.rur.gt$s.num.1
gt.rur.2 <- sum(df.no.rur.gt$si.flow.2) / init.rur.gt$s.num.2
gt.rur.3 <- sum(df.no.rur.gt$si.flow.3) / init.rur.gt$s.num.3
gt.rur.4 <- sum(df.no.rur.gt$si.flow.4) / init.rur.gt$s.num.4
gt.rur.5 <- sum(df.no.rur.gt$si.flow.5) / init.rur.gt$s.num.5
gt.rur.6 <- sum(df.no.rur.gt$si.flow.6) / init.rur.gt$s.num.6
gt.rur.7 <- sum(df.no.rur.gt$si.flow.7) / init.rur.gt$s.num.7
gt.rur.8 <- sum(df.no.rur.gt$si.flow.8) / init.rur.gt$s.num.8
gt.rur.9 <- sum(df.no.rur.gt$si.flow.9) / init.rur.gt$s.num.9

gt.rur.all <- (sum(df.no.rur.gt$si.flow.1) + sum(df.no.rur.gt$si.flow.2) +
                  sum(df.no.rur.gt$si.flow.3) + sum(df.no.rur.gt$si.flow.4) +
                  sum(df.no.rur.gt$si.flow.5) + sum(df.no.rur.gt$si.flow.6) + 
                  sum(df.no.rur.gt$si.flow.7) + sum(df.no.rur.gt$si.flow.8) +
                  sum(df.no.rur.gt$si.flow.9)) / (init.rur.gt$s.num.1 + init.rur.gt$s.num.2 + init.rur.gt$s.num.3 +
                                                     init.rur.gt$s.num.4 + init.rur.gt$s.num.5 + init.rur.gt$s.num.6 +
                                                     init.rur.gt$s.num.7 + init.rur.gt$s.num.8 + init.rur.gt$s.num.9)

gt.rur.lt5 <- (sum(df.no.rur.gt$si.flow.1) + sum(df.no.rur.gt$si.flow.2) +
                  sum(df.no.rur.gt$si.flow.3)) / (sum(df.no.rur.gt$si.flow.1) + sum(df.no.rur.gt$si.flow.2) +
                                                     sum(df.no.rur.gt$si.flow.3) + sum(df.no.rur.gt$si.flow.4) +
                                                     sum(df.no.rur.gt$si.flow.5) + sum(df.no.rur.gt$si.flow.6) + 
                                                     sum(df.no.rur.gt$si.flow.7) + sum(df.no.rur.gt$si.flow.8) +
                                                     sum(df.no.rur.gt$si.flow.9))

gt.rur.lt6m <- (sum(df.no.rur.gt$si.flow.1)) / (sum(df.no.rur.gt$si.flow.1) + sum(df.no.rur.gt$si.flow.2) +
                                                   sum(df.no.rur.gt$si.flow.3) + sum(df.no.rur.gt$si.flow.4) +
                                                   sum(df.no.rur.gt$si.flow.5) + sum(df.no.rur.gt$si.flow.6) + 
                                                   sum(df.no.rur.gt$si.flow.7) + sum(df.no.rur.gt$si.flow.8) +
                                                   sum(df.no.rur.gt$si.flow.9))
gt_rur_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    gt.rur.1,
    gt.rur.2,
    gt.rur.3,
    gt.rur.4,
    gt.rur.5,
    gt.rur.6,
    gt.rur.7,
    gt.rur.8,
    gt.rur.9
  ),
  Site = rep("Rural", 9)
)

# Guatemala urban
gt.urb.1 <- sum(df.no.urb.gt$si.flow.1) / init.urb.gt$s.num.1
gt.urb.2 <- sum(df.no.urb.gt$si.flow.2) / init.urb.gt$s.num.2
gt.urb.3 <- sum(df.no.urb.gt$si.flow.3) / init.urb.gt$s.num.3
gt.urb.4 <- sum(df.no.urb.gt$si.flow.4) / init.urb.gt$s.num.4
gt.urb.5 <- sum(df.no.urb.gt$si.flow.5) / init.urb.gt$s.num.5
gt.urb.6 <- sum(df.no.urb.gt$si.flow.6) / init.urb.gt$s.num.6
gt.urb.7 <- sum(df.no.urb.gt$si.flow.7) / init.urb.gt$s.num.7
gt.urb.8 <- sum(df.no.urb.gt$si.flow.8) / init.urb.gt$s.num.8
gt.urb.9 <- sum(df.no.urb.gt$si.flow.9) / init.urb.gt$s.num.9

gt.urb.all <- (sum(df.no.urb.gt$si.flow.1) + sum(df.no.urb.gt$si.flow.2) +
                 sum(df.no.urb.gt$si.flow.3) + sum(df.no.urb.gt$si.flow.4) +
                 sum(df.no.urb.gt$si.flow.5) + sum(df.no.urb.gt$si.flow.6) + 
                 sum(df.no.urb.gt$si.flow.7) + sum(df.no.urb.gt$si.flow.8) +
                 sum(df.no.urb.gt$si.flow.9)) / (init.urb.gt$s.num.1 + init.urb.gt$s.num.2 + init.urb.gt$s.num.3 +
                                                   init.urb.gt$s.num.4 + init.urb.gt$s.num.5 + init.urb.gt$s.num.6 +
                                                   init.urb.gt$s.num.7 + init.urb.gt$s.num.8 + init.urb.gt$s.num.9)

gt.urb.lt5 <- (sum(df.no.urb.gt$si.flow.1) + sum(df.no.urb.gt$si.flow.2) +
                 sum(df.no.urb.gt$si.flow.3)) / (sum(df.no.urb.gt$si.flow.1) + sum(df.no.urb.gt$si.flow.2) +
                                                   sum(df.no.urb.gt$si.flow.3) + sum(df.no.urb.gt$si.flow.4) +
                                                   sum(df.no.urb.gt$si.flow.5) + sum(df.no.urb.gt$si.flow.6) + 
                                                   sum(df.no.urb.gt$si.flow.7) + sum(df.no.urb.gt$si.flow.8) +
                                                   sum(df.no.urb.gt$si.flow.9))

gt.urb.lt6m <- (sum(df.no.urb.gt$si.flow.1)) / (sum(df.no.urb.gt$si.flow.1) + sum(df.no.urb.gt$si.flow.2) +
                                                  sum(df.no.urb.gt$si.flow.3) + sum(df.no.urb.gt$si.flow.4) +
                                                  sum(df.no.urb.gt$si.flow.5) + sum(df.no.urb.gt$si.flow.6) + 
                                                  sum(df.no.urb.gt$si.flow.7) + sum(df.no.urb.gt$si.flow.8) +
                                                  sum(df.no.urb.gt$si.flow.9))

gt_urb_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    gt.urb.1,
    gt.urb.2,
    gt.urb.3,
    gt.urb.4,
    gt.urb.5,
    gt.urb.6,
    gt.urb.7,
    gt.urb.8,
    gt.urb.9
  ),
  Site = rep("Urban", 9)
)

gt.values <- rbind(gt_rur_values, gt_urb_values)
gt.plot <- ggplot(gt.values, aes(Category, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position bars side by side
  labs(
    title = "Guatemala",
    x = " ",
    y = " "
  ) +
  scale_fill_manual(values = c("Rural" = "aquamarine4", "Urban" = "steelblue3")) +  # Custom colors
  theme_minimal() +
  ylim(c(0, 1)) +   guides(fill = "none")
  

# India rural
ind.rur.1 <- sum(df.no.rur.ind$si.flow.1) / init.rur.ind$s.num.1
ind.rur.2 <- sum(df.no.rur.ind$si.flow.2) / init.rur.ind$s.num.2
ind.rur.3 <- sum(df.no.rur.ind$si.flow.3) / init.rur.ind$s.num.3
ind.rur.4 <- sum(df.no.rur.ind$si.flow.4) / init.rur.ind$s.num.4
ind.rur.5 <- sum(df.no.rur.ind$si.flow.5) / init.rur.ind$s.num.5
ind.rur.6 <- sum(df.no.rur.ind$si.flow.6) / init.rur.ind$s.num.6
ind.rur.7 <- sum(df.no.rur.ind$si.flow.7) / init.rur.ind$s.num.7
ind.rur.8 <- sum(df.no.rur.ind$si.flow.8) / init.rur.ind$s.num.8
ind.rur.9 <- sum(df.no.rur.ind$si.flow.9) / init.rur.ind$s.num.9

ind.rur.all <- (sum(df.no.rur.ind$si.flow.1) + sum(df.no.rur.ind$si.flow.2) +
                  sum(df.no.rur.ind$si.flow.3) + sum(df.no.rur.ind$si.flow.4) +
                  sum(df.no.rur.ind$si.flow.5) + sum(df.no.rur.ind$si.flow.6) + 
                  sum(df.no.rur.ind$si.flow.7) + sum(df.no.rur.ind$si.flow.8) +
                  sum(df.no.rur.ind$si.flow.9)) / (init.rur.ind$s.num.1 + init.rur.ind$s.num.2 + init.rur.ind$s.num.3 +
                                                     init.rur.ind$s.num.4 + init.rur.ind$s.num.5 + init.rur.ind$s.num.6 +
                                                     init.rur.ind$s.num.7 + init.rur.ind$s.num.8 + init.rur.ind$s.num.9)

ind.rur.lt5 <- (sum(df.no.rur.ind$si.flow.1) + sum(df.no.rur.ind$si.flow.2) +
                 sum(df.no.rur.ind$si.flow.3)) / (sum(df.no.rur.ind$si.flow.1) + sum(df.no.rur.ind$si.flow.2) +
                                                    sum(df.no.rur.ind$si.flow.3) + sum(df.no.rur.ind$si.flow.4) +
                                                    sum(df.no.rur.ind$si.flow.5) + sum(df.no.rur.ind$si.flow.6) + 
                                                    sum(df.no.rur.ind$si.flow.7) + sum(df.no.rur.ind$si.flow.8) +
                                                    sum(df.no.rur.ind$si.flow.9))

ind.rur.lt6m <- (sum(df.no.rur.ind$si.flow.1)) / (sum(df.no.rur.ind$si.flow.1) + sum(df.no.rur.ind$si.flow.2) +
                                                  sum(df.no.rur.ind$si.flow.3) + sum(df.no.rur.ind$si.flow.4) +
                                                  sum(df.no.rur.ind$si.flow.5) + sum(df.no.rur.ind$si.flow.6) + 
                                                  sum(df.no.rur.ind$si.flow.7) + sum(df.no.rur.ind$si.flow.8) +
                                                  sum(df.no.rur.ind$si.flow.9))


ind_rur_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    ind.rur.1,
    ind.rur.2,
    ind.rur.3,
    ind.rur.4,
    ind.rur.5,
    ind.rur.6,
    ind.rur.7,
    ind.rur.8,
    ind.rur.9
  ),
  Site = rep("Rural", 9)
)


# India urban
ind.urb.1 <- sum(df.no.urb.ind$si.flow.1) / init.urb.ind$s.num.1
ind.urb.2 <- sum(df.no.urb.ind$si.flow.2) / init.urb.ind$s.num.2
ind.urb.3 <- sum(df.no.urb.ind$si.flow.3) / init.urb.ind$s.num.3
ind.urb.4 <- sum(df.no.urb.ind$si.flow.4) / init.urb.ind$s.num.4
ind.urb.5 <- sum(df.no.urb.ind$si.flow.5) / init.urb.ind$s.num.5
ind.urb.6 <- sum(df.no.urb.ind$si.flow.6) / init.urb.ind$s.num.6
ind.urb.7 <- sum(df.no.urb.ind$si.flow.7) / init.urb.ind$s.num.7
ind.urb.8 <- sum(df.no.urb.ind$si.flow.8) / init.urb.ind$s.num.8
ind.urb.9 <- sum(df.no.urb.ind$si.flow.9) / init.urb.ind$s.num.9

ind.urb.all <- (sum(df.no.urb.ind$si.flow.1) + sum(df.no.urb.ind$si.flow.2) +
                  sum(df.no.urb.ind$si.flow.3) + sum(df.no.urb.ind$si.flow.4) +
                  sum(df.no.urb.ind$si.flow.5) + sum(df.no.urb.ind$si.flow.6) + 
                  sum(df.no.urb.ind$si.flow.7) + sum(df.no.urb.ind$si.flow.8) +
                  sum(df.no.urb.ind$si.flow.9)) / (init.urb.ind$s.num.1 + init.urb.ind$s.num.2 + init.urb.ind$s.num.3 +
                                                     init.urb.ind$s.num.4 + init.urb.ind$s.num.5 + init.urb.ind$s.num.6 +
                                                     init.urb.ind$s.num.7 + init.urb.ind$s.num.8 + init.urb.ind$s.num.9)

ind.urb.lt5 <- (sum(df.no.urb.ind$si.flow.1) + sum(df.no.urb.ind$si.flow.2) +
                  sum(df.no.urb.ind$si.flow.3)) / (sum(df.no.urb.ind$si.flow.1) + sum(df.no.urb.ind$si.flow.2) +
                                                     sum(df.no.urb.ind$si.flow.3) + sum(df.no.urb.ind$si.flow.4) +
                                                     sum(df.no.urb.ind$si.flow.5) + sum(df.no.urb.ind$si.flow.6) + 
                                                     sum(df.no.urb.ind$si.flow.7) + sum(df.no.urb.ind$si.flow.8) +
                                                     sum(df.no.urb.ind$si.flow.9))

ind.urb.lt6m <- (sum(df.no.urb.ind$si.flow.1)) / (sum(df.no.urb.ind$si.flow.1) + sum(df.no.urb.ind$si.flow.2) +
                                                    sum(df.no.urb.ind$si.flow.3) + sum(df.no.urb.ind$si.flow.4) +
                                                    sum(df.no.urb.ind$si.flow.5) + sum(df.no.urb.ind$si.flow.6) + 
                                                    sum(df.no.urb.ind$si.flow.7) + sum(df.no.urb.ind$si.flow.8) +
                                                    sum(df.no.urb.ind$si.flow.9))




ind_urb_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    ind.urb.1,
    ind.urb.2,
    ind.urb.3,
    ind.urb.4,
    ind.urb.5,
    ind.urb.6,
    ind.urb.7,
    ind.urb.8,
    ind.urb.9
  ),
  Site=rep("Urban",9)
)

ind.values <- rbind(ind_rur_values, ind_urb_values)

ind.plot <- ggplot(ind.values, aes(Category, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position bars side by side
  labs(
    title = "India",
    x = " ",
    y = " "
  ) +
  scale_fill_manual(values = c("Rural" = "aquamarine4", "Urban" = "steelblue3")) +  # Custom colors
  theme_minimal() +
  ylim(c(0, 1)) 


# Mozambique rural
moz.rur.1 <- sum(df.no.rur.moz$si.flow.1) / init.rur.moz$s.num.1
moz.rur.2 <- sum(df.no.rur.moz$si.flow.2) / init.rur.moz$s.num.2
moz.rur.3 <- sum(df.no.rur.moz$si.flow.3) / init.rur.moz$s.num.3
moz.rur.4 <- sum(df.no.rur.moz$si.flow.4) / init.rur.moz$s.num.4
moz.rur.5 <- sum(df.no.rur.moz$si.flow.5) / init.rur.moz$s.num.5
moz.rur.6 <- sum(df.no.rur.moz$si.flow.6) / init.rur.moz$s.num.6
moz.rur.7 <- sum(df.no.rur.moz$si.flow.7) / init.rur.moz$s.num.7
moz.rur.8 <- sum(df.no.rur.moz$si.flow.8) / init.rur.moz$s.num.8
moz.rur.9 <- sum(df.no.rur.moz$si.flow.9) / init.rur.moz$s.num.9

moz.rur.all <- (sum(df.no.rur.moz$si.flow.1) + sum(df.no.rur.moz$si.flow.2) +
                 sum(df.no.rur.moz$si.flow.3) + sum(df.no.rur.moz$si.flow.4) +
                 sum(df.no.rur.moz$si.flow.5) + sum(df.no.rur.moz$si.flow.6) + 
                 sum(df.no.rur.moz$si.flow.7) + sum(df.no.rur.moz$si.flow.8) +
                 sum(df.no.rur.moz$si.flow.9)) / (init.rur.moz$s.num.1 + init.rur.moz$s.num.2 + init.rur.moz$s.num.3 +
                                                   init.rur.moz$s.num.4 + init.rur.moz$s.num.5 + init.rur.moz$s.num.6 +
                                                   init.rur.moz$s.num.7 + init.rur.moz$s.num.8 + init.rur.moz$s.num.9)

moz.rur.lt5 <- (sum(df.no.rur.moz$si.flow.1) + sum(df.no.rur.moz$si.flow.2) +
                  sum(df.no.rur.moz$si.flow.3)) / (sum(df.no.rur.moz$si.flow.1) + sum(df.no.rur.moz$si.flow.2) +
                                                     sum(df.no.rur.moz$si.flow.3) + sum(df.no.rur.moz$si.flow.4) +
                                                     sum(df.no.rur.moz$si.flow.5) + sum(df.no.rur.moz$si.flow.6) + 
                                                     sum(df.no.rur.moz$si.flow.7) + sum(df.no.rur.moz$si.flow.8) +
                                                     sum(df.no.rur.moz$si.flow.9))

moz.rur.lt6m <- (sum(df.no.rur.moz$si.flow.1)) / (sum(df.no.rur.moz$si.flow.1) + sum(df.no.rur.moz$si.flow.2) +
                                                    sum(df.no.rur.moz$si.flow.3) + sum(df.no.rur.moz$si.flow.4) +
                                                    sum(df.no.rur.moz$si.flow.5) + sum(df.no.rur.moz$si.flow.6) + 
                                                    sum(df.no.rur.moz$si.flow.7) + sum(df.no.rur.moz$si.flow.8) +
                                                    sum(df.no.rur.moz$si.flow.9))



moz_rur_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    moz.rur.1,
    moz.rur.2,
    moz.rur.3,
    moz.rur.4,
    moz.rur.5,
    moz.rur.6,
    moz.rur.7,
    moz.rur.8,
    moz.rur.9
  ),
  Site = rep("Rural",9)
)

# Mozambique urban
moz.urb.1 <- sum(df.no.urb.moz$si.flow.1) / init.urb.moz$s.num.1
moz.urb.2 <- sum(df.no.urb.moz$si.flow.2) / init.urb.moz$s.num.2
moz.urb.3 <- sum(df.no.urb.moz$si.flow.3) / init.urb.moz$s.num.3
moz.urb.4 <- sum(df.no.urb.moz$si.flow.4) / init.urb.moz$s.num.4
moz.urb.5 <- sum(df.no.urb.moz$si.flow.5) / init.urb.moz$s.num.5
moz.urb.6 <- sum(df.no.urb.moz$si.flow.6) / init.urb.moz$s.num.6
moz.urb.7 <- sum(df.no.urb.moz$si.flow.7) / init.urb.moz$s.num.7
moz.urb.8 <- sum(df.no.urb.moz$si.flow.8) / init.urb.moz$s.num.8
moz.urb.9 <- sum(df.no.urb.moz$si.flow.9) / init.urb.moz$s.num.9

moz.urb.all <- (sum(df.no.urb.moz$si.flow.1) + sum(df.no.urb.moz$si.flow.2) +
                  sum(df.no.urb.moz$si.flow.3) + sum(df.no.urb.moz$si.flow.4) +
                  sum(df.no.urb.moz$si.flow.5) + sum(df.no.urb.moz$si.flow.6) + 
                  sum(df.no.urb.moz$si.flow.7) + sum(df.no.urb.moz$si.flow.8) +
                  sum(df.no.urb.moz$si.flow.9)) / (init.urb.moz$s.num.1 + init.urb.moz$s.num.2 + init.urb.moz$s.num.3 +
                                                     init.urb.moz$s.num.4 + init.urb.moz$s.num.5 + init.urb.moz$s.num.6 +
                                                     init.urb.moz$s.num.7 + init.urb.moz$s.num.8 + init.urb.moz$s.num.9)

moz.urb.lt5 <- (sum(df.no.urb.moz$si.flow.1) + sum(df.no.urb.moz$si.flow.2) +
                  sum(df.no.urb.moz$si.flow.3)) / (sum(df.no.urb.moz$si.flow.1) + sum(df.no.urb.moz$si.flow.2) +
                                                     sum(df.no.urb.moz$si.flow.3) + sum(df.no.urb.moz$si.flow.4) +
                                                     sum(df.no.urb.moz$si.flow.5) + sum(df.no.urb.moz$si.flow.6) + 
                                                     sum(df.no.urb.moz$si.flow.7) + sum(df.no.urb.moz$si.flow.8) +
                                                     sum(df.no.urb.moz$si.flow.9))

moz.urb.lt6m <- (sum(df.no.urb.moz$si.flow.1)) / (sum(df.no.urb.moz$si.flow.1) + sum(df.no.urb.moz$si.flow.2) +
                                                    sum(df.no.urb.moz$si.flow.3) + sum(df.no.urb.moz$si.flow.4) +
                                                    sum(df.no.urb.moz$si.flow.5) + sum(df.no.urb.moz$si.flow.6) + 
                                                    sum(df.no.urb.moz$si.flow.7) + sum(df.no.urb.moz$si.flow.8) +
                                                    sum(df.no.urb.moz$si.flow.9))


moz_urb_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    moz.urb.1,
    moz.urb.2,
    moz.urb.3,
    moz.urb.4,
    moz.urb.5,
    moz.urb.6,
    moz.urb.7,
    moz.urb.8,
    moz.urb.9
  ),
  Site = rep("Urban",9)
)

moz.values <- rbind(moz_rur_values, moz_urb_values)

moz.plot <- ggplot(moz.values, aes(Category, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position bars side by side
  labs(
    title = "Mozambique",
    x = " ",
    y = " "
  ) +
  scale_fill_manual(values = c("Rural" = "aquamarine4", "Urban" = "steelblue3")) +  # Custom colors
  theme_minimal() +
  ylim(c(0, 1)) +   guides(fill = "none")


# Pakistan rural
pak.rur.1 <- sum(df.no.rur.pak$si.flow.1) / init.rur.pak$s.num.1
pak.rur.2 <- sum(df.no.rur.pak$si.flow.2) / init.rur.pak$s.num.2
pak.rur.3 <- sum(df.no.rur.pak$si.flow.3) / init.rur.pak$s.num.3
pak.rur.4 <- sum(df.no.rur.pak$si.flow.4) / init.rur.pak$s.num.4
pak.rur.5 <- sum(df.no.rur.pak$si.flow.5) / init.rur.pak$s.num.5
pak.rur.6 <- sum(df.no.rur.pak$si.flow.6) / init.rur.pak$s.num.6
pak.rur.7 <- sum(df.no.rur.pak$si.flow.7) / init.rur.pak$s.num.7
pak.rur.8 <- sum(df.no.rur.pak$si.flow.8) / init.rur.pak$s.num.8
pak.rur.9 <- sum(df.no.rur.pak$si.flow.9) / init.rur.pak$s.num.9

pak.rur.all <- (sum(df.no.rur.pak$si.flow.1) + sum(df.no.rur.pak$si.flow.2) +
                  sum(df.no.rur.pak$si.flow.3) + sum(df.no.rur.pak$si.flow.4) +
                  sum(df.no.rur.pak$si.flow.5) + sum(df.no.rur.pak$si.flow.6) + 
                  sum(df.no.rur.pak$si.flow.7) + sum(df.no.rur.pak$si.flow.8) +
                  sum(df.no.rur.pak$si.flow.9)) / (init.rur.pak$s.num.1 + init.rur.pak$s.num.2 + init.rur.pak$s.num.3 +
                                                     init.rur.pak$s.num.4 + init.rur.pak$s.num.5 + init.rur.pak$s.num.6 +
                                                     init.rur.pak$s.num.7 + init.rur.pak$s.num.8 + init.rur.pak$s.num.9)

pak.rur.lt5 <- (sum(df.no.rur.pak$si.flow.1) + sum(df.no.rur.pak$si.flow.2) +
                  sum(df.no.rur.pak$si.flow.3)) / (sum(df.no.rur.pak$si.flow.1) + sum(df.no.rur.pak$si.flow.2) +
                                                     sum(df.no.rur.pak$si.flow.3) + sum(df.no.rur.pak$si.flow.4) +
                                                     sum(df.no.rur.pak$si.flow.5) + sum(df.no.rur.pak$si.flow.6) + 
                                                     sum(df.no.rur.pak$si.flow.7) + sum(df.no.rur.pak$si.flow.8) +
                                                     sum(df.no.rur.pak$si.flow.9))

pak.rur.lt6m <- (sum(df.no.rur.pak$si.flow.1)) / (sum(df.no.rur.pak$si.flow.1) + sum(df.no.rur.pak$si.flow.2) +
                                                    sum(df.no.rur.pak$si.flow.3) + sum(df.no.rur.pak$si.flow.4) +
                                                    sum(df.no.rur.pak$si.flow.5) + sum(df.no.rur.pak$si.flow.6) + 
                                                    sum(df.no.rur.pak$si.flow.7) + sum(df.no.rur.pak$si.flow.8) +
                                                    sum(df.no.rur.pak$si.flow.9))


pak_rur_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    pak.rur.1,
    pak.rur.2,
    pak.rur.3,
    pak.rur.4,
    pak.rur.5,
    pak.rur.6,
    pak.rur.7,
    pak.rur.8,
    pak.rur.9
  ),
  Site = rep("Rural", 9)
)

# Pakistan urban
pak.urb.1 <- sum(df.no.urb.pak$si.flow.1) / init.urb.pak$s.num.1
pak.urb.2 <- sum(df.no.urb.pak$si.flow.2) / init.urb.pak$s.num.2
pak.urb.3 <- sum(df.no.urb.pak$si.flow.3) / init.urb.pak$s.num.3
pak.urb.4 <- sum(df.no.urb.pak$si.flow.4) / init.urb.pak$s.num.4
pak.urb.5 <- sum(df.no.urb.pak$si.flow.5) / init.urb.pak$s.num.5
pak.urb.6 <- sum(df.no.urb.pak$si.flow.6) / init.urb.pak$s.num.6
pak.urb.7 <- sum(df.no.urb.pak$si.flow.7) / init.urb.pak$s.num.7
pak.urb.8 <- sum(df.no.urb.pak$si.flow.8) / init.urb.pak$s.num.8
pak.urb.9 <- sum(df.no.urb.pak$si.flow.9) / init.urb.pak$s.num.9

pak.urb.all <- (sum(df.no.urb.pak$si.flow.1) + sum(df.no.urb.pak$si.flow.2) +
                  sum(df.no.urb.pak$si.flow.3) + sum(df.no.urb.pak$si.flow.4) +
                  sum(df.no.urb.pak$si.flow.5) + sum(df.no.urb.pak$si.flow.6) + 
                  sum(df.no.urb.pak$si.flow.7) + sum(df.no.urb.pak$si.flow.8) +
                  sum(df.no.urb.pak$si.flow.9)) / (init.urb.pak$s.num.1 + init.urb.pak$s.num.2 + init.urb.pak$s.num.3 +
                                                     init.urb.pak$s.num.4 + init.urb.pak$s.num.5 + init.urb.pak$s.num.6 +
                                                     init.urb.pak$s.num.7 + init.urb.pak$s.num.8 + init.urb.pak$s.num.9)

pak.urb.lt5 <- (sum(df.no.urb.pak$si.flow.1) + sum(df.no.urb.pak$si.flow.2) +
                  sum(df.no.urb.pak$si.flow.3)) / (sum(df.no.urb.pak$si.flow.1) + sum(df.no.urb.pak$si.flow.2) +
                                                     sum(df.no.urb.pak$si.flow.3) + sum(df.no.urb.pak$si.flow.4) +
                                                     sum(df.no.urb.pak$si.flow.5) + sum(df.no.urb.pak$si.flow.6) + 
                                                     sum(df.no.urb.pak$si.flow.7) + sum(df.no.urb.pak$si.flow.8) +
                                                     sum(df.no.urb.pak$si.flow.9))

pak.urb.lt6m <- (sum(df.no.urb.pak$si.flow.1)) / (sum(df.no.urb.pak$si.flow.1) + sum(df.no.urb.pak$si.flow.2) +
                                                    sum(df.no.urb.pak$si.flow.3) + sum(df.no.urb.pak$si.flow.4) +
                                                    sum(df.no.urb.pak$si.flow.5) + sum(df.no.urb.pak$si.flow.6) + 
                                                    sum(df.no.urb.pak$si.flow.7) + sum(df.no.urb.pak$si.flow.8) +
                                                    sum(df.no.urb.pak$si.flow.9))


pak_urb_values <- data.frame(
  Category = factor(c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    pak.urb.1,
    pak.urb.2,
    pak.urb.3,
    pak.urb.4,
    pak.urb.5,
    pak.urb.6,
    pak.urb.7,
    pak.urb.8,
    pak.urb.9
  ),
  Site = rep("Urban", 9)
)

pak.values <- rbind(pak_rur_values, pak_urb_values)
pak.plot <- ggplot(pak.values, aes(Category, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position bars side by side
  labs(
    title = "Pakistan",
    x = " ",
    y = " "
  ) +
  scale_fill_manual(values = c("Rural" = "aquamarine4", "Urban" = "steelblue3")) +  # Custom colors
  theme_minimal() +
  ylim(c(0, 1)) +   guides(fill = "none")

# ---- compile plots
gt.plot + ind.plot +
  moz.plot + pak.plot + plot_layout(ncol=2)



# ------- Calculate R effective 
# -- Guatemala
# Rural
gt.rur.2 <- df.no.rur.gt %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
gt.rur.epiestim <- merge(x=gt.rur.2, y=all.dates, by="dates", all="TRUE")
gt.rur.epiestim <- gt.rur.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(gt.rur.epiestim$I, 
                             method="parametric_si",
                             config = make_config(list(
                             mean_si = 2.6, 
                             std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)
# Urban
gt.urb.2 <- df.no.urb.gt %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
gt.urb.epiestim <- merge(x=gt.urb.2, y=all.dates, by="dates", all="TRUE")
gt.urb.epiestim <- gt.urb.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(gt.urb.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)

# -- India
# Rural
ind.rur.2 <- df.no.rur.ind %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
ind.rur.epiestim <- merge(x=ind.rur.2, y=all.dates, by="dates", all="TRUE")
ind.rur.epiestim <- ind.rur.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(ind.rur.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)
# Urban
ind.urb.2 <- df.no.urb.ind %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
ind.urb.epiestim <- merge(x=ind.urb.2, y=all.dates, by="dates", all="TRUE")
ind.urb.epiestim <- ind.urb.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(ind.urb.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)

# -- Mozambique
# Rural
moz.rur.2 <- df.no.rur.moz %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
moz.rur.epiestim <- merge(x=moz.rur.2, y=all.dates, by="dates", all="TRUE")
moz.rur.epiestim <- moz.rur.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(moz.rur.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)
# Urban
moz.urb.2 <- df.no.urb.moz %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
moz.urb.epiestim <- merge(x=moz.urb.2, y=all.dates, by="dates", all="TRUE")
moz.urb.epiestim <- moz.urb.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(moz.urb.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)

# -- Pakistan
# Rural
pak.rur.2 <- df.no.rur.pak %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
pak.rur.epiestim <- merge(x=pak.rur.2, y=all.dates, by="dates", all="TRUE")
pak.rur.epiestim <- pak.rur.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(pak.rur.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)
# Urban
pak.urb.2 <- df.no.urb.pak %>%
  rename(dates= time, I = si.flow) %>%
  dplyr::select(dates, I)
all.dates <- as.data.frame(seq(1,730,1))
names(all.dates) <- "dates"
pak.urb.epiestim <- merge(x=pak.urb.2, y=all.dates, by="dates", all="TRUE")
pak.urb.epiestim <- pak.urb.epiestim %>% mutate(I = ifelse(is.na(I), 0, I)) 

estimates <- estimate_R(pak.urb.epiestim$I, 
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 2.6, 
                          std_si = 1.5))
)
median(estimates$R$`Mean(R)`)
max(estimates$R$`Mean(R)`)


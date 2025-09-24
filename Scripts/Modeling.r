# ------------------------------------------------------------------------------
#
# SIR model comparing contact patterns in GlobalMix vs Prem 
#
# Sara Kim
# May 30, 2025
#
# ------------------------------------------------------------------------------

# --- Library
library(EpiModel)
library(ggplot2)
library(patchwork)
library(ggtext)
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
    
    s.num <- s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
      s.num.7 + s.num.8 + s.num.9
    
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


# --- Guatemala specific parameters and conditions 

# - Parameters
# Urban
param.urban.gt <- param.dcm(gamma = 1/14, q = 0.015,
                            c11=0.0, c12=0.0, c13=0.3, c14=0.3, c15=1.2, c16=0.9, c17=0.4, c18=0.5, c19=0.2,
                            c21=0.0, c22=0.0, c23=0.2, c24=0.3, c25=1.1, c26=0.8, c27=0.5, c28=0.5, c29=0.2,
                            c31=0.3, c32=0.3, c33=0.5, c34=0.7, c35=0.8, c36=0.7, c37=0.5, c38=0.5, c39=0.3,
                            c41=0.5, c42=0.5, c43=1.0, c44=1.6, c45=1.6, c46=0.7, c47=0.7, c48=0.6, c49=0.2,
                            c51=0.9, c52=1.0, c53=0.7, c54=1.1, c55=2.6, c56=0.7, c57=0.8, c58=1.0, c59=0.3,
                            c61=1.1, c62=1.1, c63=1.0, c64=0.8, c65=1.0, c66=1.8, c67=0.9, c68=1.3, c69=0.7,
                            c71=0.7, c72=1.0, c73=0.8, c74=0.9, c75=1.3, c76=1.2, c77=1.3, c78=1.4, c79=0.9,
                            c81=0.7, c82=0.7, c83=0.7, c84=0.7, c85=1.5, c86=1.3, c87=1.2, c88=1.7, c89=1.2,
                            c91=0.2, c92=0.2, c93=0.4, c94=0.2, c95=0.5, c96=0.6, c97=0.6, c98=0.9, c99=1.3)
# Rural
param.rural.gt <- param.dcm(gamma = 1/14, q = 0.015,
                            c11=0.0, c12=0.0, c13=0.3, c14=0.4, c15=1.0, c16=0.7, c17=0.5, c18=0.5, c19=0.2,
                            c21=0.0, c22=0.0, c23=0.2, c24=0.5, c25=1.6, c26=1.0, c27=0.7, c28=0.7, c29=0.2,
                            c31=0.5, c32=0.2, c33=0.9, c34=1.0, c35=1.1, c36=0.9, c37=0.6, c38=0.6, c39=0.2,
                            c41=0.5, c42=0.5, c43=1.1, c44=2.2, c45=1.4, c46=0.7, c47=0.8, c48=0.5, c49=0.3,
                            c51=0.9, c52=1.1, c53=0.8, c54=1.1, c55=2.8, c56=0.8, c57=0.9, c58=0.9, c59=0.4,
                            c61=1.0, c62=1.0, c63=1.0, c64=0.7, c65=1.1, c66=1.5, c67=0.9, c68=1.5, c69=0.6,
                            c71=0.7, c72=0.7, c73=0.7, c74=1.0, c75=1.3, c76=0.9, c77=1.4, c78=1.2, c79=0.8,
                            c81=0.7, c82=0.8, c83=0.7, c84=0.7, c85=1.3, c86=1.6, c87=1.4, c88=1.9, c89=1.2,
                            c91=0.2, c92=0.2, c93=0.2, c94=0.3, c95=0.5, c96=0.5, c97=0.7, c98=1.0, c99=1.2)

# - Initial conditions
# Urban
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
# Rural
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

# - Controls
control <- control.dcm(nstep=730, new.mod = sirmod)

# - Compile model GlobalMix urban
sim.no.urb.gt <- dcm(param.urban.gt, init.urb.gt, control)
df.no.urb.gt <- as.data.frame(sim.no.urb.gt)

# - Compile model GlobalMix rural
sim.no.rur.gt <- dcm(param.rural.gt, init.rur.gt, control)
df.no.rur.gt <- as.data.frame(sim.no.rur.gt)

# --- India specific parameters and conditions 

# - Parameters
# Urban
param.urban.ind <- param.dcm(gamma = 1/14, q = 0.015,
                             c11=0.1, c12=0.0, c13=0.4, c14=0.2, c15=0.6, c16=1.0, c17=0.6, c18=0.8, c19=0.2,
                             c21=0.0, c22=0.0, c23=0.4, c24=0.3, c25=0.6, c26=1.1, c27=0.7, c28=0.7, c29=0.2,
                             c31=0.4, c32=0.4, c33=1.2, c34=0.6, c35=0.7, c36=1.0, c37=0.8, c38=0.7, c39=0.3,
                             c41=0.2, c42=0.4, c43=0.6, c44=3.4, c45=1.7, c46=0.6, c47=1.2, c48=0.7, c49=0.4,
                             c51=0.4, c52=0.4, c53=0.4, c54=1.1, c55=4.8, c56=0.6, c57=0.9, c58=0.8, c59=0.4,
                             c61=1.3, c62=1.4, c63=1.3, c64=0.7, c65=1.0, c66=2.8, c67=1.1, c68=1.4, c69=0.8,
                             c71=0.7, c72=0.9, c73=1.0, c74=1.5, c75=1.7, c76=1.1, c77=2.2, c78=1.5, c79=0.8,
                             c81=1.0, c82=0.9, c83=0.8, c84=0.9, c85=1.4, c86=1.4, c87=1.5, c88=2.7, c89=1.3,
                             c91=0.3, c92=0.3, c93=0.4, c94=0.4, c95=0.6, c96=0.6, c97=0.6, c98=0.9, c99=1.0)
# Rural
param.rural.ind <- param.dcm(gamma = 1/14, q = 0.015,
                             c11=0.2, c12=0.0, c13=0.3, c14=0.1, c15=0.6, c16=0.8, c17=0.6, c18=0.8, c19=0.3,
                             c21=0.0, c22=0.0, c23=0.1, c24=0.1, c25=0.6, c26=0.6, c27=0.4, c28=0.8, c29=0.3,
                             c31=0.3, c32=0.1, c33=1.1, c34=0.8, c35=0.7, c36=0.8, c37=1.1, c38=0.9, c39=0.5,
                             c41=0.1, c42=0.1, c43=0.8, c44=3.3, c45=1.7, c46=0.4, c47=1.1, c48=0.7, c49=0.5,
                             c51=0.4, c52=0.4, c53=0.4, c54=1.2, c55=5.0, c56=0.6, c57=0.8, c58=0.8, c59=0.5,
                             c61=1.0, c62=0.8, c63=1.0, c64=0.5, c65=1.1, c66=2.3, c67=0.9, c68=1.6, c69=0.6,
                             c71=0.8, c72=0.5, c73=1.3, c74=1.4, c75=1.4, c76=1.1, c77=1.8, c78=1.5, c79=1.0,
                             c81=0.9, c82=0.9, c83=1.0, c84=0.8, c85=1.6, c86=1.6, c87=1.5, c88=2.2, c89=1.4,
                             c91=0.4, c92=0.4, c93=0.7, c94=0.6, c95=0.9, c96=0.6, c97=1.0, c98=1.3, c99=1.2)

# - Initial conditions
# Urban
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
# Rural
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

# - Compile model GlobalMix urban
sim.no.urb.ind <- dcm(param.urban.ind, init.urb.ind, control)
df.no.urb.ind <- as.data.frame(sim.no.urb.ind)

# - Compile model GlobalMix rural
sim.no.rur.ind <- dcm(param.rural.ind, init.rur.ind, control)
df.no.rur.ind <- as.data.frame(sim.no.rur.ind)


# --- Mozambique specific parameters and conditions 

# - Parameters
# Urban
param.urban.moz <- param.dcm(gamma = 1/14, q = 0.015,
                             c11=0.0, c12=0.0, c13=0.1, c14=0.2, c15=0.7, c16=0.3, c17=0.3, c18=0.2, c19=0.1,
                             c21=0.0, c22=0.0, c23=0.3, c24=0.3, c25=0.9, c26=0.5, c27=0.4, c28=0.4, c29=0.1,
                             c31=0.1, c32=0.4, c33=1.2, c34=0.7, c35=0.9, c36=0.6, c37=0.4, c38=0.4, c39=0.1,
                             c41=0.3, c42=0.5, c43=1.1, c44=2.0, c45=1.5, c46=0.6, c47=0.4, c48=0.3, c49=0.2,
                             c51=0.5, c52=0.7, c53=0.7, c54=0.9, c55=4.0, c56=0.7, c57=0.6, c58=0.5, c59=0.3,
                             c61=0.5, c62=0.7, c63=0.8, c64=0.7, c65=1.2, c66=1.8, c67=1.0, c68=1.0, c69=0.4,
                             c71=0.4, c72=0.6, c73=0.5, c74=0.4, c75=0.8, c76=1.0, c77=1.6, c78=1.2, c79=0.4,
                             c81=0.2, c82=0.3, c83=0.3, c84=0.2, c85=0.5, c86=0.5, c87=0.7, c88=1.3, c89=0.5,
                             c91=0.1, c92=0.1, c93=0.1, c94=0.1, c95=0.3, c96=0.3, c97=0.2, c98=0.6, c99=0.6)
# Rural
param.rural.moz <- param.dcm(gamma = 1/14, q = 0.015,
                             c11=0.0, c12=0.0, c13=0.3, c14=0.6, c15=1.8, c16=0.6, c17=0.3, c18=0.5, c19=0.1,
                             c21=0.0, c22=0.0, c23=0.4, c24=0.6, c25=1.6, c26=0.5, c27=0.3, c28=0.4, c29=0.1,
                             c31=0.5, c32=0.7, c33=1.2, c34=1.5, c35=2.1, c36=0.6, c37=0.5, c38=0.5, c39=0.2,
                             c41=0.9, c42=1.1, c43=1.7, c44=3.6, c45=2.8, c46=0.5, c47=0.5, c48=0.5, c49=0.3,
                             c51=1.3, c52=1.5, c53=1.5, c54=1.8, c55=5.5, c56=1.5, c57=0.8, c58=0.9, c59=0.5,
                             c61=0.8, c62=0.9, c63=0.8, c64=0.5, c65=1.9, c66=2.8, c67=1.4, c68=1.5, c69=0.5,
                             c71=0.5, c72=0.5, c73=0.5, c74=0.4, c75=1.1, c76=1.3, c77=1.7, c78=1.9, c79=0.6,
                             c81=0.5, c82=0.5, c83=0.4, c84=0.3, c85=0.9, c86=1.1, c87=1.5, c88=2.3, c89=1.0,
                             c91=0.2, c92=0.1, c93=0.2, c94=0.2, c95=0.6, c96=0.4, c97=0.5, c98=1.0, c99=0.8)

# - Initial conditions
# Urban
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

# Rural
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

# - Compile model GlobalMix urban
sim.no.urb.moz <- dcm(param.urban.moz, init.urb.moz, control)
df.no.urb.moz <- as.data.frame(sim.no.urb.moz)

# - Compile model GlobalMix rural
sim.no.rur.moz <- dcm(param.rural.moz, init.rur.moz, control)
df.no.rur.moz <- as.data.frame(sim.no.rur.moz)


# --- Pakistan specific parameters and conditions 

# - Parameters
# Urban
param.urban.pak <- param.dcm(gamma = 1/14, q = 0.015,
                             c11=0.0, c12=0.0, c13=0.2, c14=0.4, c15=1.0, c16=0.9, c17=0.6, c18=0.4, c19=0.1,
                             c21=0.0, c22=0.0, c23=0.3, c24=0.5, c25=1.2, c26=0.7, c27=0.7, c28=0.3, c29=0.1,
                             c31=0.4, c32=0.4, c33=0.8, c34=1.0, c35=1.2, c36=0.9, c37=1.0, c38=0.5, c39=0.2,
                             c41=0.6, c42=0.8, c43=1.1, c44=2.5, c45=2.5, c46=1.0, c47=1.4, c48=0.7, c49=0.3,
                             c51=0.8, c52=1.0, c53=0.8, c54=1.5, c55=3.7, c56=1.1, c57=1.6, c58=1.1, c59=1.0,
                             c61=1.4, c62=1.1, c63=1.0, c64=1.1, c65=1.9, c66=2.9, c67=2.5, c68=1.7, c69=0.9,
                             c71=0.9, c72=1.0, c73=1.0, c74=1.4, c75=2.4, c76=2.3, c77=4.2, c78=2.2, c79=1.2,
                             c81=0.6, c82=0.5, c83=0.6, c84=0.7, c85=1.8, c86=1.6, c87=2.4, c88=2.5, c89=1.6,
                             c91=0.2, c92=0.1, c93=0.1, c94=0.2, c95=1.0, c96=0.5, c97=0.8, c98=0.9, c99=1.1)
# Rural
param.rural.pak <- param.dcm(gamma = 1/14, q = 0.015,
                             c11=0.0, c12=0.0, c13=0.5, c14=0.8, c15=2.6, c16=1.1, c17=1.0, c18=0.9, c19=0.4,
                             c21=0.0, c22=0.0, c23=0.5, c24=1.0, c25=2.8, c26=1.3, c27=0.8, c28=1.1, c29=0.3,
                             c31=0.7, c32=0.7, c33=2.1, c34=2.3, c35=3.3, c36=1.7, c37=1.7, c38=1.4, c39=0.9,
                             c41=1.1, c42=1.3, c43=2.5, c44=4.2, c45=4.8, c46=1.8, c47=2.1, c48=2.0, c49=1.1,
                             c51=1.9, c52=2.0, c53=2.2, c54=3.3, c55=4.9, c56=1.9, c57=1.8, c58=2.3, c59=1.4,
                             c61=1.6, c62=1.9, c63=2.0, c64=2.2, c65=3.4, c66=3.1, c67=2.3, c68=2.7, c69=1.5,
                             c71=1.5, c72=1.1, c73=2.0, c74=2.4, c75=2.8, c76=2.2, c77=3.2, c78=2.7, c79=1.7,
                             c81=1.1, c82=1.2, c83=1.3, c84=1.9, c85=3.0, c86=2.2, c87=2.3, c88=2.6, c89=1.8,
                             c91=0.5, c92=0.4, c93=0.8, c94=1.0, c95=1.7, c96=1.1, c97=1.3, c98=1.5, c99=1.3)

# - Initial conditions
# Urban
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
# Rural
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

# - Compile model GlobalMix urban
sim.no.urb.pak <- dcm(param.urban.pak, init.urb.pak, control)
df.no.urb.pak <- as.data.frame(sim.no.urb.pak)

# - Compile model GlobalMix rural
sim.no.rur.pak <- dcm(param.rural.pak, init.rur.pak, control)
df.no.rur.pak <- as.data.frame(sim.no.rur.pak)

# --- Update model to compare to POLYMOD
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
    
    
    s.num <- s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
      s.num.7 
    
    # 2. define age-specific forces of infection
    # force of infection for <6m0 contacts (infecting the <60mo year olds)
    
    lambda.1 <- q*c11*((i.num.1)/num.1) +
      q*c12*((i.num.2)/num.2) + # c12 is contact between <6mo with 6-11mo
      q*c13*((i.num.3)/num.3) +  # c13 is contact between <6mo with 1-4 y/o, etc
      q*c14*((i.num.4)/num.4) +
      q*c15*((i.num.5)/num.5) +
      q*c16*((i.num.6)/num.6) +
      q*c17*((i.num.7)/num.7)
    
    
    # force of infection for 6-11mo contacts (infecting the 6-11 mo)
    lambda.2 <- q*c21*((i.num.1)/num.1) +
      q*c22*((i.num.2)/num.2) +
      q*c23*((i.num.3)/num.3) +
      q*c24*((i.num.4)/num.4) +
      q*c25*((i.num.5)/num.5) +
      q*c26*((i.num.6)/num.6) +
      q*c27*((i.num.7)/num.7) 
    
    
    # force of infection for 1-4 y contacts (infecting the 1-4y )
    lambda.3 <- q*c31*((i.num.1)/num.1) +
      q*c32*((i.num.2)/num.2) +
      q*c33*((i.num.3)/num.3) +
      q*c34*((i.num.4)/num.4) +
      q*c35*((i.num.5)/num.5) +
      q*c36*((i.num.6)/num.6) +
      q*c37*((i.num.7)/num.7) 
    
    
    # force of infection for 5-9y contacts (infecting the 5-9y)
    lambda.4 <- q*c41*((i.num.1)/num.1) +
      q*c42*((i.num.2)/num.2) +
      q*c43*((i.num.3)/num.3) +
      q*c44*((i.num.4)/num.4) +
      q*c45*((i.num.5)/num.5) +
      q*c46*((i.num.6)/num.6) +
      q*c47*((i.num.7)/num.7) 
    
    
    # force of infection for 10-19 contacts (infecting the 10-19 year olds)
    lambda.5 <- q*c51*((i.num.1)/num.1) +
      q*c52*((i.num.2)/num.2) +
      q*c53*((i.num.3)/num.3) +
      q*c54*((i.num.4)/num.4) +
      q*c55*((i.num.5)/num.5) +
      q*c56*((i.num.6)/num.6) +
      q*c57*((i.num.7)/num.7) 
    
    
    # force of infection for 20-29y contacts (infecting the 20-29 year olds)
    lambda.6 <- q*c61*((i.num.1)/num.1) +
      q*c62*((i.num.2)/num.2) +
      q*c63*((i.num.3)/num.3) +
      q*c64*((i.num.4)/num.4) +
      q*c65*((i.num.5)/num.5) +
      q*c66*((i.num.6)/num.6) +
      q*c67*((i.num.7)/num.7) 
    
    
    # force of infection for 30-39y contacts (infecting the 30-39 year olds)
    lambda.7 <- q*c71*((i.num.1)/num.1) +
      q*c72*((i.num.2)/num.2) +
      q*c73*((i.num.3)/num.3) +
      q*c74*((i.num.4)/num.4) +
      q*c75*((i.num.5)/num.5) +
      q*c76*((i.num.6)/num.6) +
      q*c77*((i.num.7)/num.7) 
    
    
    
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
    
    
    # 4. List outputs
    list(c(dS.1, dI.1, dR.1,
           dS.2, dI.2, dR.2,
           dS.3, dI.3, dR.3,
           dS.4, dI.4, dR.4,
           dS.5, dI.5, dR.5,
           dS.6, dI.6, dR.6,
           dS.7, dI.7, dR.7,
           si.flow.1 = lambda.1*s.num.1,
           si.flow.2 = lambda.2*s.num.2,
           si.flow.3 = lambda.3*s.num.3,
           si.flow.4 = lambda.4*s.num.4,
           si.flow.5 = lambda.5*s.num.5,
           si.flow.6 = lambda.6*s.num.6,
           si.flow.7 = lambda.7*s.num.7,
           si.flow = lambda.1*s.num.1 + lambda.2*s.num.2 + lambda.3*s.num.3 + lambda.4*s.num.4 +
             lambda.5*s.num.5 + lambda.6*s.num.6 + lambda.7*s.num.7
    ))
  })
}


# --- Guatemala specific parameters and conditions 

# - Parameters
# globalmix
param.gm.gt <- param.dcm(gamma = 1/14, q = 0.015,
                         c11=0.5, c12=0.4, c13=0.5, c14=0.4, c15=0.3, c16=0.2, c17=0.1, 
                         c21=1.0, c22=1.9, c23=1.5, c24=0.7, c25=0.8, c26=0.6, c27=0.3, 
                         c31=1.0, c32=1.1, c33=2.7, c34=0.8, c35=0.8, c36=1.0, c37=0.4, 
                         c41=1.2, c42=0.7, c43=1.0, c44=1.6, c45=0.9, c46=1.4, c47=0.7, 
                         c51=0.9, c52=1.0, c53=1.3, c54=1.0, c55=1.4, c56=1.3, c57=0.9, 
                         c61=0.8, c62=0.7, c63=1.4, c64=1.5, c65=1.3, c66=1.8, c67=1.2, 
                         c71=0.3, c72=0.3, c73=0.5, c74=0.6, c75=0.7, c76=0.9, c77=1.2)
# prem
param.prem.gt <- param.dcm(gamma = 1/14, q = 0.015,
                           c11=2.5, c12=1.5, c13=1.8, c14=2.2, c15=1.9, c16=1.6, c17=0.8,
                           c21=1.8, c22=5.9, c23=2.7, c24=1.5, c25=2.1, c26=1.9, c27=0.8, 
                           c31=0.7, c32=1.8, c33=11.4, c34=2.0, c35=1.6, c36=2.1, c37=0.7, 
                           c41=1.0, c42=0.7, c43=2.8, c44=5.5, c45=2.1, c46=2.4, c47=0.8, 
                           c51=1.1, c52=1.3, c53=2.1, c54=2.7, c55=3.4, c56=3.1, c57=1.0, 
                           c61=0.7, c62=0.8, c63=2.3, c64=2.5, c65=2.4, c66=3.7, c67=1.0, 
                           c71=0.5, c72=0.6, c73=1.2, c74=1.3, c75=1.6, c76=2.0, c77=1.2)

# - Initial conditions
init.gt <- init.dcm(s.num.1 = 2950000, i.num.1 = 1,  r.num.1 = 0,
                    s.num.2 = 2470000, i.num.2 = 1,  r.num.2 = 0,
                    s.num.3 = 4180000, i.num.3 = 1,  r.num.3 = 0,
                    s.num.4 = 3420000, i.num.4 = 1, r.num.4 = 0,
                    s.num.5 = 2470000, i.num.5 = 1,  r.num.5 = 0,
                    s.num.6 = 2280000, i.num.6 = 1,  r.num.6 = 0,
                    s.num.7 = 1420000, i.num.7 = 1, r.num.7 = 0,
                    si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                    si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow = 0)


# - Controls
control <- control.dcm(nstep=730, new.mod = sirmod)

# - Compile model GlobalMix 
sim.gm.gt <- dcm(param.gm.gt, init.gt, control)
df.gm.gt <- as.data.frame(sim.gm.gt)

# - Compile model prem
sim.prem.gt <- dcm(param.prem.gt, init.gt, control)
df.prem.gt <- as.data.frame(sim.prem.gt)

# --- India specific parameters and conditions 

# - Parameters
# globalmix
param.gm.ind <- param.dcm(gamma = 1/14, q = 0.015,
                          c11=0.8, c12=0.4, c13=0.3, c14=0.5, c15=0.5, c16=0.4, c17=0.2,
                          c21=0.8, c22=3.4, c23=1.7, c24=0.5, c25=1.2, c26=0.7, c27=0.4, 
                          c31=0.5, c32=1.2, c33=4.9, c34=0.6, c35=0.9, c36=0.8, c37=0.4, 
                          c41=1.5, c42=0.6, c43=1.0, c44=2.5, c45=1.0, c46=1.5, c47=0.7, 
                          c51=1.2, c52=1.5, c53=1.6, c54=1.1, c55=2.0, c56=1.5, c57=0.9,
                          c61=1.1, c62=0.9, c63=1.5, c64=1.5, c65=1.5, c66=2.5, c67=1.4, 
                          c71=0.5, c72=0.5, c73=0.7, c74=0.6, c75=0.8, c76=1.1, c77=1.1)
# prem
param.prem.ind <- param.dcm(gamma = 1/14, q = 0.015,
                            c11=2.2, c12=1.3, c13=1.2, c14=2.5, c15=2.1, c16=2.0, c17=0.9, 
                            c21=1.6, c22=5.9, c23=2.3, c24=1.6, c25=2.6, c26=2.2, c27=1.0, 
                            c31=0.4, c32=1.5, c33=10.7, c34=1.8, c35=1.8, c36=2.5, c37=0.8, 
                            c41=0.9, c42=0.5, c43=2.4, c44=5.2, c45=2.2, c46=2.8, c47=0.8, 
                            c51=0.8, c52=1.2, c53=1.9, c54=2.6, c55=3.5, c56=3.4, c57=1.1, 
                            c61=0.6, c62=0.6, c63=1.9, c64=2.5, c65=2.5, c66=4.0, c67=1.1, 
                            c71=0.5, c72=0.6, c73=1.1, c74=1.4, c75=1.9, c76=2.5, c77=1.4)

# - Initial conditions
init.ind <- init.dcm(s.num.1 = 190200000,i.num.1 = 1, r.num.1 = 0,
                     s.num.2 = 170400000, i.num.2 = 1,r.num.2 = 0,
                     s.num.3 = 312400000, i.num.3 = 1, r.num.3 = 0,
                     s.num.4 = 255600000, i.num.4 = 1, r.num.4 = 0,
                     s.num.5 = 184600000, i.num.5 = 1, r.num.5 = 0,
                     s.num.6 = 198800000, i.num.6 = 1, r.num.6 = 0,
                     s.num.7 = 113600000, i.num.7 = 1, r.num.7 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow = 0)

# - Compile model GlobalMix
sim.gm.ind <- dcm(param.gm.ind, init.ind, control)
df.gm.ind <- as.data.frame(sim.gm.ind)

# - Compile model prem
sim.prem.ind <- dcm(param.prem.ind, init.ind, control)
df.prem.ind <- as.data.frame(sim.prem.ind)


# --- Mozambique specific parameters and conditions 

# - Parameters
# globalmix
param.gm.moz <- param.dcm(gamma = 1/14, q = 0.015,
                          c11=0.8, c12=0.6, c13=0.6, c14=0.3, c15=0.3, c16=0.3, c17=0.2, 
                          c21=1.6, c22=2.8, c23=2.2, c24=0.6, c25=0.5, c26=0.4, c27=0.3, 
                          c31=1.2, c32=1.4, c33=4.7, c34=1.1, c35=0.7, c36=0.7, c37=0.4,
                          c41=1.1, c42=0.6, c43=1.6, c44=2.3, c45=1.2, c46=1.3, c47=0.5, 
                          c51=0.9, c52=0.4, c53=1.0, c54=1.1, c55=1.7, c56=1.6, c57=0.5, 
                          c61=0.5, c62=0.2, c63=0.6, c64=0.8, c65=1.1, c66=1.7, c67=0.8, 
                          c71=0.3, c72=0.2, c73=0.4, c74=0.3, c75=0.4, c76=0.8, c77=0.7)
# prem
param.prem.moz <- param.dcm(gamma = 1/14, q = 0.015,
                            c11=3.0, c12=2.1, c13=1.9, c14=1.9, c15=1.7, c16=1.5, c17=0.7, 
                            c21=2.4, c22=7.4, c23=3.1, c24=1.4, c25=1.9, c26=1.9, c27=0.8, 
                            c31=0.9, c32=2.4, c33=11.2, c34=1.8, c35=1.3, c36=1.8, c37=0.7, 
                            c41=1.3, c42=0.9, c43=3.1, c44=5.3, c45=2.1, c46=2.3, c47=0.8, 
                            c51=1.7, c52=1.8, c53=2.3, c54=2.7, c55=3.3, c56=3.3, c57=0.9, 
                            c61=1.0, c62=1.2, c63=2.6, c64=2.4, c65=2.5, c66=3.9, c67=1.0, 
                            c71=0.9, c72=1.0, c73=1.5, c74=1.3, c75=1.4, c76=2.0, c77=1.0)

# - Initial conditions
init.moz <- init.dcm(s.num.1 = 5770000, i.num.1 = 1, r.num.1 = 0,
                     s.num.2 = 4950000, i.num.2 = 1, r.num.2 = 0,
                     s.num.3 = 8250000, i.num.3 = 1, r.num.3 = 0,
                     s.num.4 = 5940000, i.num.4 = 1, r.num.4 = 0,
                     s.num.5 = 3970000, i.num.5 = 1, r.num.5 = 0,
                     s.num.6 = 3300000, i.num.6 = 1, r.num.6 = 0,
                     s.num.7 = 1650000, i.num.7 = 1, r.num.7 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow = 0)


# - Compile model GlobalMix
sim.gm.moz <- dcm(param.gm.moz, init.moz, control)
df.gm.moz <- as.data.frame(sim.gm.moz)

# - Compile model prem
sim.prem.moz <- dcm(param.prem.moz, init.moz, control)
df.prem.moz <- as.data.frame(sim.prem.moz)


# --- Pakistan specific parameters and conditions 

# - Parameters
# globalmix
param.gm.pak <- param.dcm(gamma = 1/14, q = 0.015,
                          c11=1.0, c12=0.9, c13=1.0, c14=0.8, c15=0.8, c16=0.6, c17=0.5, 
                          c21=2.2, c22=3.4, c23=3.7, c24=1.4, c25=1.8, c26=1.4, c27=0.7, 
                          c31=1.7, c32=2.4, c33=4.3, c34=1.5, c35=1.7, c36=1.7, c37=1.2,
                          c41=2.1, c42=1.6, c43=2.6, c44=3.0, c45=2.4, c46=2.2, c47=1.2, 
                          c51=1.9, c52=1.9, c53=2.6, c54=2.3, c55=3.7, c56=2.4, c57=1.4, 
                          c61=1.3, c62=1.3, c63=2.5, c64=1.9, c65=2.4, c66=2.6, c67=1.7, 
                          c71=0.9, c72=0.6, c73=1.3, c74=0.8, c75=1.0, c76=1.2, c77=1.2)
# prem
param.prem.pak <- param.dcm(gamma = 1/14, q = 0.015,
                            c11=3.2, c12=2.1, c13=2.0, c14=2.7, c15=2.4, c16=2.0, c17=0.9, 
                            c21=2.1, c22=6.2, c23=3.2, c24=1.8, c25=2.5, c26=2.2, c27=0.9, 
                            c31=0.8, c32=2.1, c33=9.8, c34=2.1, c35=1.6, c36=2.1, c37=0.7, 
                            c41=1.3, c42=0.8, c43=3.0, c44=6.0, c45=2.3, c46=2.6, c47=0.8, 
                            c51=1.5, c52=1.8, c53=2.2, c54=3.0, c55=3.4, c56=3.2, c57=1.1, 
                            c61=0.9, c62=1.1, c63=2.6, c64=2.8, c65=2.5, c66=3.7, c67=1.0, 
                            c71=1.1, c72=1.1, c73=1.6, c74=1.7, c75=2.2, c76=2.3, c77=1.2)

# - Initial conditions
init.pak <- init.dcm(s.num.1 = 42720000, i.num.1 = 1, r.num.1 = 0,
                     s.num.2 = 36000000, i.num.2 = 1, r.num.2 = 0,
                     s.num.3 = 52800000, i.num.3 = 1, r.num.3 = 0,
                     s.num.4 = 42870000, i.num.4 = 1, r.num.4 = 0,
                     s.num.5 = 28800000, i.num.5 = 1, r.num.5 = 0,
                     s.num.6 = 24000000, i.num.6 = 1, r.num.6 = 0,
                     s.num.7 = 15600000, i.num.7 = 1, r.num.7 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow = 0)


# - Compile model GlobalMix 
sim.gm.pak <- dcm(param.gm.pak, init.pak, control)
df.gm.pak <- as.data.frame(sim.gm.pak)

# - Compile model prem
sim.prem.pak <- dcm(param.prem.pak, init.pak, control)
df.prem.pak <- as.data.frame(sim.prem.pak)


# ----- Absolute Risk 

# Guatemala rural
gt.rur.1 <- (sum(df.no.rur.gt$si.flow.1) + sum(df.no.rur.gt$si.flow.2) + sum(df.no.rur.gt$si.flow.3)) / (init.rur.gt$s.num.1 + 
                                                                                                           init.rur.gt$s.num.2 + init.rur.gt$s.num.3)
gt.rur.4 <- sum(df.no.rur.gt$si.flow.4) / init.rur.gt$s.num.4
gt.rur.5 <- sum(df.no.rur.gt$si.flow.5) / init.rur.gt$s.num.5
gt.rur.6 <- sum(df.no.rur.gt$si.flow.6) / init.rur.gt$s.num.6
gt.rur.7 <- sum(df.no.rur.gt$si.flow.7) / init.rur.gt$s.num.7
gt.rur.8 <- sum(df.no.rur.gt$si.flow.8) / init.rur.gt$s.num.8
gt.rur.9 <- sum(df.no.rur.gt$si.flow.9) / init.rur.gt$s.num.9

gt.rur.all <- sum(df.no.rur.gt$si.flow) / (init.rur.gt$s.num.1 + init.rur.gt$s.num.2 + init.rur.gt$s.num.3 + init.rur.gt$s.num.4 +
                                        init.rur.gt$s.num.5 + init.rur.gt$s.num.6 + init.rur.gt$s.num.7 + init.rur.gt$s.num.8 +
                                        init.rur.gt$s.num.9)

gt_rur_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    gt.rur.1,
    gt.rur.4,
    gt.rur.5,
    gt.rur.6,
    gt.rur.7,
    gt.rur.8,
    gt.rur.9
  ),
  Site = rep("GlobalMix, rural", 7)
)

# Guatemala urban
gt.urb.1 <- (sum(df.no.urb.gt$si.flow.1) + sum(df.no.urb.gt$si.flow.2) + sum(df.no.urb.gt$si.flow.3)) / (init.urb.gt$s.num.1 + 
                                                                                                           init.urb.gt$s.num.2 + init.urb.gt$s.num.3)
gt.urb.4 <- sum(df.no.urb.gt$si.flow.4) / init.urb.gt$s.num.4
gt.urb.5 <- sum(df.no.urb.gt$si.flow.5) / init.urb.gt$s.num.5
gt.urb.6 <- sum(df.no.urb.gt$si.flow.6) / init.urb.gt$s.num.6
gt.urb.7 <- sum(df.no.urb.gt$si.flow.7) / init.urb.gt$s.num.7
gt.urb.8 <- sum(df.no.urb.gt$si.flow.8) / init.urb.gt$s.num.8
gt.urb.9 <- sum(df.no.urb.gt$si.flow.9) / init.urb.gt$s.num.9

gt.urb.all <- sum(df.no.urb.gt$si.flow) / (init.urb.gt$s.num.1 + init.urb.gt$s.num.2 + init.urb.gt$s.num.3 + init.urb.gt$s.num.4 +
                                             init.urb.gt$s.num.5 + init.urb.gt$s.num.6 + init.urb.gt$s.num.7 + init.urb.gt$s.num.8 +
                                             init.urb.gt$s.num.9)

gt_urb_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    gt.urb.1,
    gt.urb.4,
    gt.urb.5,
    gt.urb.6,
    gt.urb.7,
    gt.urb.8,
    gt.urb.9
  ),
  Site = rep("GlobalMix, urban", 7)
)

# Guatemala PREM
gt.prem.1 <- sum(df.prem.gt$si.flow.1) / init.gt$s.num.1
gt.prem.2 <- sum(df.prem.gt$si.flow.2) / init.gt$s.num.2
gt.prem.3 <- sum(df.prem.gt$si.flow.3) / init.gt$s.num.3
gt.prem.4 <- sum(df.prem.gt$si.flow.4) / init.gt$s.num.4
gt.prem.5 <- sum(df.prem.gt$si.flow.5) / init.gt$s.num.5
gt.prem.6 <- sum(df.prem.gt$si.flow.6) / init.gt$s.num.6
gt.prem.7 <- sum(df.prem.gt$si.flow.7) / init.gt$s.num.7


gt_prem_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    gt.prem.1,
    gt.prem.2,
    gt.prem.3,
    gt.prem.4,
    gt.prem.5,
    gt.prem.6,
    gt.prem.7
  ),
  Site = rep("Prem et al., 2021", 7)
)

gt.values <- rbind(gt_rur_values, gt_urb_values, gt_prem_values)

# India rural
ind.rur.1 <- (sum(df.no.rur.ind$si.flow.1) + sum(df.no.rur.ind$si.flow.2) + sum(df.no.rur.ind$si.flow.3)) / (init.rur.ind$s.num.1 +
                                                                                                               init.rur.ind$s.num.2 + init.rur.ind$s.num.3)
ind.rur.4 <- sum(df.no.rur.ind$si.flow.4) / init.rur.ind$s.num.4
ind.rur.5 <- sum(df.no.rur.ind$si.flow.5) / init.rur.ind$s.num.5
ind.rur.6 <- sum(df.no.rur.ind$si.flow.6) / init.rur.ind$s.num.6
ind.rur.7 <- sum(df.no.rur.ind$si.flow.7) / init.rur.ind$s.num.7
ind.rur.8 <- sum(df.no.rur.ind$si.flow.8) / init.rur.ind$s.num.8
ind.rur.9 <- sum(df.no.rur.ind$si.flow.9) / init.rur.ind$s.num.9

ind.rur.all <- sum(df.no.rur.ind$si.flow) / (init.rur.ind$s.num.1 + init.rur.ind$s.num.2 + init.rur.ind$s.num.3 + init.rur.ind$s.num.4 +
                                             init.rur.ind$s.num.5 + init.rur.ind$s.num.6 + init.rur.ind$s.num.7 + init.rur.ind$s.num.8 +
                                             init.rur.ind$s.num.9)

ind_rur_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    ind.rur.1,
    ind.rur.4,
    ind.rur.5,
    ind.rur.6,
    ind.rur.7,
    ind.rur.8,
    ind.rur.9
  ),
  Site = rep("GlobalMix, rural", 7)
)

# India urban
ind.urb.1 <- (sum(df.no.urb.ind$si.flow.1) + sum(df.no.urb.ind$si.flow.2) +  sum(df.no.urb.ind$si.flow.3)) / (init.urb.ind$s.num.1 +
                                                                                                                init.urb.ind$s.num.2 + init.urb.ind$s.num.3)
ind.urb.4 <- sum(df.no.urb.ind$si.flow.4) / init.urb.ind$s.num.4
ind.urb.5 <- sum(df.no.urb.ind$si.flow.5) / init.urb.ind$s.num.5
ind.urb.6 <- sum(df.no.urb.ind$si.flow.6) / init.urb.ind$s.num.6
ind.urb.7 <- sum(df.no.urb.ind$si.flow.7) / init.urb.ind$s.num.7
ind.urb.8 <- sum(df.no.urb.ind$si.flow.8) / init.urb.ind$s.num.8
ind.urb.9 <- sum(df.no.urb.ind$si.flow.9) / init.urb.ind$s.num.9

ind.urb.all <- sum(df.no.urb.ind$si.flow) / (init.urb.ind$s.num.1 + init.urb.ind$s.num.2 + init.urb.ind$s.num.3 + init.urb.ind$s.num.4 +
                                               init.urb.ind$s.num.5 + init.urb.ind$s.num.6 + init.urb.ind$s.num.7 + init.urb.ind$s.num.8 +
                                               init.urb.ind$s.num.9)

ind_urb_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    ind.urb.1,
    ind.urb.4,
    ind.urb.5,
    ind.urb.6,
    ind.urb.7,
    ind.urb.8,
    ind.urb.9
  ),
  Site=rep("GlobalMix, urban",7)
)

# India PREM
ind.prem.1 <- sum(df.prem.ind$si.flow.1) / init.ind$s.num.1
ind.prem.2 <- sum(df.prem.ind$si.flow.2) / init.ind$s.num.2
ind.prem.3 <- sum(df.prem.ind$si.flow.3) / init.ind$s.num.3
ind.prem.4 <- sum(df.prem.ind$si.flow.4) / init.ind$s.num.4
ind.prem.5 <- sum(df.prem.ind$si.flow.5) / init.ind$s.num.5
ind.prem.6 <- sum(df.prem.ind$si.flow.6) / init.ind$s.num.6
ind.prem.7 <- sum(df.prem.ind$si.flow.7) / init.ind$s.num.7


ind_prem_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    ind.prem.1,
    ind.prem.2,
    ind.prem.3,
    ind.prem.4,
    ind.prem.5,
    ind.prem.6,
    ind.prem.7
  ),
  Site = rep("Prem et al., 2021", 7)
)

ind.values <- rbind(ind_rur_values, ind_urb_values, ind_prem_values)

# Mozambique rural
moz.rur.1 <- (sum(df.no.rur.moz$si.flow.1) + sum(df.no.rur.moz$si.flow.2) + sum(df.no.rur.moz$si.flow.3)) / (init.rur.moz$s.num.1 +
                                                                                                               init.rur.moz$s.num.2 + init.rur.moz$s.num.3)
moz.rur.4 <- sum(df.no.rur.moz$si.flow.4) / init.rur.moz$s.num.4
moz.rur.5 <- sum(df.no.rur.moz$si.flow.5) / init.rur.moz$s.num.5
moz.rur.6 <- sum(df.no.rur.moz$si.flow.6) / init.rur.moz$s.num.6
moz.rur.7 <- sum(df.no.rur.moz$si.flow.7) / init.rur.moz$s.num.7
moz.rur.8 <- sum(df.no.rur.moz$si.flow.8) / init.rur.moz$s.num.8
moz.rur.9 <- sum(df.no.rur.moz$si.flow.9) / init.rur.moz$s.num.9

moz.rur.all <- sum(df.no.rur.moz$si.flow) / (init.rur.moz$s.num.1 + init.rur.moz$s.num.2 + init.rur.moz$s.num.3 + init.rur.moz$s.num.4 +
                                               init.rur.moz$s.num.5 + init.rur.moz$s.num.6 + init.rur.moz$s.num.7 + init.rur.moz$s.num.8 +
                                               init.rur.moz$s.num.9)

moz_rur_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    moz.rur.1,
    moz.rur.4,
    moz.rur.5,
    moz.rur.6,
    moz.rur.7,
    moz.rur.8,
    moz.rur.9
  ),
  Site = rep("GlobalMix, rural",7)
)

# Mozambique urban
moz.urb.1 <- (sum(df.no.urb.moz$si.flow.1) + sum(df.no.urb.moz$si.flow.2) + sum(df.no.urb.moz$si.flow.3)) / (init.urb.moz$s.num.1 +
                                                                                                               init.urb.moz$s.num.2 + init.urb.moz$s.num.3)
moz.urb.4 <- sum(df.no.urb.moz$si.flow.4) / init.urb.moz$s.num.4
moz.urb.5 <- sum(df.no.urb.moz$si.flow.5) / init.urb.moz$s.num.5
moz.urb.6 <- sum(df.no.urb.moz$si.flow.6) / init.urb.moz$s.num.6
moz.urb.7 <- sum(df.no.urb.moz$si.flow.7) / init.urb.moz$s.num.7
moz.urb.8 <- sum(df.no.urb.moz$si.flow.8) / init.urb.moz$s.num.8
moz.urb.9 <- sum(df.no.urb.moz$si.flow.9) / init.urb.moz$s.num.9

moz.urb.all <- sum(df.no.urb.moz$si.flow) / (init.urb.moz$s.num.1 + init.urb.moz$s.num.2 + init.urb.moz$s.num.3 + init.urb.moz$s.num.4 +
                                               init.urb.moz$s.num.5 + init.urb.moz$s.num.6 + init.urb.moz$s.num.7 + init.urb.moz$s.num.8 +
                                               init.urb.moz$s.num.9)

moz_urb_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    moz.urb.1,
    moz.urb.4,
    moz.urb.5,
    moz.urb.6,
    moz.urb.7,
    moz.urb.8,
    moz.urb.9
  ),
  Site = rep("GlobalMix, urban",7)
)

# Mozambique PREM
moz.prem.1 <- sum(df.prem.moz$si.flow.1) / init.moz$s.num.1
moz.prem.2 <- sum(df.prem.moz$si.flow.2) / init.moz$s.num.2
moz.prem.3 <- sum(df.prem.moz$si.flow.3) / init.moz$s.num.3
moz.prem.4 <- sum(df.prem.moz$si.flow.4) / init.moz$s.num.4
moz.prem.5 <- sum(df.prem.moz$si.flow.5) / init.moz$s.num.5
moz.prem.6 <- sum(df.prem.moz$si.flow.6) / init.moz$s.num.6
moz.prem.7 <- sum(df.prem.moz$si.flow.7) / init.moz$s.num.7


moz_prem_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    moz.prem.1,
    moz.prem.2,
    moz.prem.3,
    moz.prem.4,
    moz.prem.5,
    moz.prem.6,
    moz.prem.7
  ),
  Site = rep("Prem et al., 2021", 7)
)

moz.values <- rbind(moz_rur_values, moz_urb_values, moz_prem_values)


# Pakistan rural
pak.rur.1 <- (sum(df.no.rur.pak$si.flow.1) + sum(df.no.rur.pak$si.flow.2) + sum(df.no.rur.pak$si.flow.3)) / (init.rur.pak$s.num.1 +
                                                                                                               init.rur.pak$s.num.2 + init.rur.pak$s.num.3)
pak.rur.4 <- sum(df.no.rur.pak$si.flow.4) / init.rur.pak$s.num.4
pak.rur.5 <- sum(df.no.rur.pak$si.flow.5) / init.rur.pak$s.num.5
pak.rur.6 <- sum(df.no.rur.pak$si.flow.6) / init.rur.pak$s.num.6
pak.rur.7 <- sum(df.no.rur.pak$si.flow.7) / init.rur.pak$s.num.7
pak.rur.8 <- sum(df.no.rur.pak$si.flow.8) / init.rur.pak$s.num.8
pak.rur.9 <- sum(df.no.rur.pak$si.flow.9) / init.rur.pak$s.num.9

pak.rur.all <- sum(df.no.rur.pak$si.flow) / (init.rur.pak$s.num.1 + init.rur.pak$s.num.2 + init.rur.pak$s.num.3 + init.rur.pak$s.num.4 +
                                               init.rur.pak$s.num.5 + init.rur.pak$s.num.6 + init.rur.pak$s.num.7 + init.rur.pak$s.num.8 +
                                               init.rur.pak$s.num.9)

pak_rur_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    pak.rur.1,
    pak.rur.4,
    pak.rur.5,
    pak.rur.6,
    pak.rur.7,
    pak.rur.8,
    pak.rur.9
  ),
  Site = rep("GlobalMix, rural", 7)
)

# Pakistan urban
pak.urb.1 <- (sum(df.no.urb.pak$si.flow.1) + sum(df.no.urb.pak$si.flow.2) + sum(df.no.urb.pak$si.flow.3)) / (init.urb.pak$s.num.1 +
                                                                                                               init.urb.pak$s.num.2 + init.urb.pak$s.num.3)
pak.urb.4 <- sum(df.no.urb.pak$si.flow.4) / init.urb.pak$s.num.4
pak.urb.5 <- sum(df.no.urb.pak$si.flow.5) / init.urb.pak$s.num.5
pak.urb.6 <- sum(df.no.urb.pak$si.flow.6) / init.urb.pak$s.num.6
pak.urb.7 <- sum(df.no.urb.pak$si.flow.7) / init.urb.pak$s.num.7
pak.urb.8 <- sum(df.no.urb.pak$si.flow.8) / init.urb.pak$s.num.8
pak.urb.9 <- sum(df.no.urb.pak$si.flow.9) / init.urb.pak$s.num.9

pak.urb.all <- sum(df.no.urb.pak$si.flow) / (init.urb.pak$s.num.1 + init.urb.pak$s.num.2 + init.urb.pak$s.num.3 + init.urb.pak$s.num.4 +
                                               init.urb.pak$s.num.5 + init.urb.pak$s.num.6 + init.urb.pak$s.num.7 + init.urb.pak$s.num.8 +
                                               init.urb.pak$s.num.9)

pak_urb_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    pak.urb.1,
    pak.urb.4,
    pak.urb.5,
    pak.urb.6,
    pak.urb.7,
    pak.urb.8,
    pak.urb.9
  ),
  Site = rep("GlobalMix, urban", 7)
)

# Pakistan PREM
pak.prem.1 <- sum(df.prem.pak$si.flow.1) / init.pak$s.num.1
pak.prem.2 <- sum(df.prem.pak$si.flow.2) / init.pak$s.num.2
pak.prem.3 <- sum(df.prem.pak$si.flow.3) / init.pak$s.num.3
pak.prem.4 <- sum(df.prem.pak$si.flow.4) / init.pak$s.num.4
pak.prem.5 <- sum(df.prem.pak$si.flow.5) / init.pak$s.num.5
pak.prem.6 <- sum(df.prem.pak$si.flow.6) / init.pak$s.num.6
pak.prem.7 <- sum(df.prem.pak$si.flow.7) / init.pak$s.num.7


pak_prem_values <- data.frame(
  Category = factor(c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"),
                    levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
  Value = c(
    pak.prem.1,
    pak.prem.2,
    pak.prem.3,
    pak.prem.4,
    pak.prem.5,
    pak.prem.6,
    pak.prem.7
  ),
  Site = rep("Prem et al., 2021", 7)
)

pak.values <- rbind(pak_rur_values, pak_urb_values, pak_prem_values)

# figure 
gt.values$Country <- "Guatemala"
ind.values$Country <- "India"
moz.values$Country <- "Mozambique"
pak.values$Country <- "Pakistan"

all.values <- rbind(gt.values, ind.values, moz.values, pak.values)

all.plot <- ggplot(all.values, aes(x = Category, y = Value*100, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(
    x = NULL,
    y = "Cumulative Incidence (%)"
  ) +
  scale_fill_manual(values = c("GlobalMix, rural" = "aquamarine4", "GlobalMix, urban" = "steelblue3", "Prem et al., 2021" = "sienna")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 7, face = "bold"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)
  )


# ---- epidemic peak and timing

# Guatemala
df.no.rur.gt <- df.no.rur.gt %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
        gt.rur.inc = si.flow/s.num)
df.no.urb.gt <- df.no.urb.gt %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         gt.urb.inc = si.flow/s.num)
df.prem.gt <- df.prem.gt %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7,
         gt.prem.inc = si.flow/s.num)
# India
df.no.rur.ind <- df.no.rur.ind %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         ind.rur.inc = si.flow/s.num)
df.no.urb.ind <- df.no.urb.ind %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         ind.urb.inc = si.flow/s.num)
df.prem.ind <- df.prem.ind %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7,
         ind.prem.inc = si.flow/s.num)
# Mozambique
df.no.rur.moz <- df.no.rur.moz %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         moz.rur.inc = si.flow/s.num)
df.no.urb.moz <- df.no.urb.moz %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         moz.urb.inc = si.flow/s.num)
df.prem.moz <- df.prem.moz %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7,
         moz.prem.inc = si.flow/s.num)
# Pakistan
df.no.rur.pak <- df.no.rur.pak %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         pak.rur.inc = si.flow/s.num)
df.no.urb.pak <- df.no.urb.pak %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7 + s.num.8 + s.num.9,
         pak.urb.inc = si.flow/s.num)
df.prem.pak <- df.prem.pak %>%
  mutate(s.num = s.num.1 + s.num.2 + s.num.3 + s.num.4 + s.num.5 + s.num.6 +
           s.num.7,
         pak.prem.inc = si.flow/s.num)



df_gt <- bind_rows(
  df.no.rur.gt %>% dplyr::select(time, gt.rur.inc) %>%
    rename(incidence = gt.rur.inc) %>% mutate(area = "GlobalMix, rural", country = "Guatemala"),
  df.no.urb.gt %>% dplyr::select(time, gt.urb.inc) %>%
    rename(incidence = gt.urb.inc) %>% mutate(area = "GlobalMix, urban", country = "Guatemala"),
  df.prem.gt %>% dplyr::select(time, gt.prem.inc) %>%
    rename(incidence = gt.prem.inc) %>% mutate(area = "Prem et al., 2021", country = "Guatemala")
)

# Repeat for each country
df_ind <- bind_rows(
  df.no.rur.ind %>% dplyr::select(time, ind.rur.inc) %>%
    rename(incidence = ind.rur.inc) %>% mutate(area = "GlobalMix, rural", country = "India"),
  df.no.urb.ind %>% dplyr::select(time, ind.urb.inc) %>%
    rename(incidence = ind.urb.inc) %>% mutate(area = "GlobalMix, urban", country = "India"),
  df.prem.ind %>% dplyr::select(time, ind.prem.inc) %>%
    rename(incidence = ind.prem.inc) %>% mutate(area = "Prem et al., 2021", country = "India")
)

df_moz <- bind_rows(
  df.no.rur.moz %>% dplyr::select(time, moz.rur.inc) %>%
    rename(incidence = moz.rur.inc) %>% mutate(area = "GlobalMix, rural", country = "Mozambique"),
  df.no.urb.moz %>% dplyr::select(time, moz.urb.inc) %>%
    rename(incidence = moz.urb.inc) %>% mutate(area = "GlobalMix, urban", country = "Mozambique"),
  df.prem.moz %>% dplyr::select(time, moz.prem.inc) %>%
    rename(incidence = moz.prem.inc) %>% mutate(area = "Prem et al., 2021", country = "Mozambique")
)

df_pak <- bind_rows(
  df.no.rur.pak %>% dplyr::select(time, pak.rur.inc) %>%
    rename(incidence = pak.rur.inc) %>% mutate(area = "GlobalMix, rural", country = "Pakistan"),
  df.no.urb.pak %>% dplyr::select(time, pak.urb.inc) %>%
    rename(incidence = pak.urb.inc) %>% mutate(area = "GlobalMix, urban", country = "Pakistan"),
  df.prem.pak %>% dplyr::select(time, pak.prem.inc) %>%
    rename(incidence = pak.prem.inc) %>% mutate(area = "Prem et al., 2021", country = "Pakistan")
)

# Combine all into one long data frame
df_all <- bind_rows(df_gt, df_ind, df_moz, df_pak)

ggplot(df_all, aes(x = time, y = incidence, color = area)) +
  geom_line() +
  facet_wrap(~ country, ncol = 2) +
  scale_color_manual(
    name = "",
    values = c("GlobalMix, rural" = "aquamarine4", "GlobalMix, urban" = "steelblue3", "Prem et al., 2021" = "sienna")
  ) +
  labs(x = "Time", y = "Incidence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) -> sfig7


# extract max peak and timing
df_gt_rur_peak <- df_all %>%
  filter(area == "Rural" & country == "Guatemala")
max(df_gt_rur_peak$incidence) # 0.01
which.max(df_gt_rur_peak$incidence) #270

df_gt_urb_peak <- df_all %>%
  filter(area == "Urban" & country == "Guatemala")
max(df_gt_urb_peak$incidence) # 0.01
which.max(df_gt_urb_peak$incidence) #302

df_gt_prem_peak <- df_all %>%
  filter(area == "Prem" & country == "Guatemala")
max(df_gt_prem_peak$incidence) # 0.07
which.max(df_gt_prem_peak$incidence) #93

df_ind_rur_peak <- df_all %>%
  filter(area == "Rural" & country == "India")
max(df_ind_rur_peak$incidence) # 0.02
which.max(df_ind_rur_peak$incidence) #285

df_ind_urb_peak <- df_all %>%
  filter(area == "Urban" & country == "India")
max(df_ind_urb_peak$incidence) # 0.02
which.max(df_ind_urb_peak$incidence) #264

df_ind_prem_peak <- df_all %>%
  filter(area == "Prem" & country == "India")
max(df_ind_prem_peak$incidence) # 0.06
which.max(df_ind_prem_peak$incidence) #125

df_moz_rur_peak <- df_all %>%
  filter(area == "Rural" & country == "Mozambique")
max(df_moz_rur_peak$incidence) # 0.02
which.max(df_moz_rur_peak$incidence) #176

df_moz_urb_peak <- df_all %>%
  filter(area == "Urban" & country == "Mozambique")
max(df_moz_urb_peak$incidence) # 0.00
which.max(df_moz_urb_peak$incidence) #490

df_moz_prem_peak <- df_all %>%
  filter(area == "Prem" & country == "Mozambique")
max(df_moz_prem_peak$incidence) # 0.06
which.max(df_moz_prem_peak$incidence) #125

df_pak_rur_peak <- df_all %>%
  filter(area == "Rural" & country == "Pakistan")
max(df_pak_rur_peak$incidence) # 0.09
which.max(df_pak_rur_peak$incidence) #93

df_pak_urb_peak <- df_all %>%
  filter(area == "Urban" & country == "Pakistan")
max(df_pak_urb_peak$incidence) # 0.03
which.max(df_pak_urb_peak$incidence) #163
  


# -----  Get R estimates

process_epi_data <- function(df, country, area, mean_si = 2.6, std_si = 1.5, max_time) {
  df <- df %>%
    mutate(si.flow2 = ifelse(si.flow<1, 0, si.flow)) %>%
    rename(dates = time, I = si.flow2) %>%
    dplyr::select(dates, I) %>%
    filter(dates <= max_time)
  
  all.dates <- data.frame(dates = 1:max_time)
  merged_df <- merge(x = df, y = all.dates, by = "dates", all = TRUE) %>%
    mutate(I = ifelse(is.na(I), 0, I))
  
  estimates <- estimate_R(merged_df$I,
                          method = "parametric_si",
                          config = make_config(list(mean_si = mean_si, std_si = std_si)))
  
  out <- estimates$R %>%
    mutate(t_end = t_end,
           country = country,
           area = area,
           mean_r = `Mean(R)`,
           lq = `Quantile.0.025(R)`,
           uq = `Quantile.0.975(R)`) %>%
    dplyr::select(t_end, mean_r, lq, uq, area, country)
  
  return(out)
}

process_epi_data <- function(df, country, area, mean_si = 2.6, std_si = 1.5, max_time) {
  df <- df %>%
    mutate(si.flow2 = ifelse(si.flow < 1, 0, si.flow)) %>%
    rename(dates = time, I = si.flow2) %>%
    dplyr::select(dates, I) %>%
    filter(dates <= max_time)
  
  all.dates <- data.frame(dates = 2:max_time)
  merged_df <- merge(x = df, y = all.dates, by = "dates", all = TRUE) %>%
    mutate(I = ifelse(is.na(I), 0, I))
  
  # Find when I becomes 0 and stays 0
  zero_run <- which(cumsum(rev(merged_df$I)) == 0)
  if (length(zero_run) > 0) {
    cutoff <- nrow(merged_df) - max(zero_run)  # index before the I values become all 0
    merged_df <- merged_df[1:cutoff, ]
  }
  
  # Only run estimate_R if there's sufficient data
  if (sum(merged_df$I) > 0 && nrow(merged_df) >= 2) {
    estimates <- estimate_R(merged_df$I,
                            method = "parametric_si",
                            config = make_config(list(mean_si = mean_si, std_si = std_si)))
    
    out <- estimates$R %>%
      mutate(t_end = t_end,
             country = country,
             area = area,
             mean_r = `Mean(R)`,
             lq = `Quantile.0.025(R)`,
             uq = `Quantile.0.975(R)`) %>%
      dplyr::select(t_end, mean_r, lq, uq, area, country)
  } else {
    out <- data.frame(t_end = numeric(0), mean_r = numeric(0),
                      lq = numeric(0), uq = numeric(0),
                      area = character(0), country = character(0))
  }
  
  return(out)
}
# Guatemala
df_gt_rur <- process_epi_data(df.no.rur.gt, country = "Guatemala", area = "GlobalMix, rural", max_time = 730)
df_gt_urb <- process_epi_data(df.no.urb.gt, country = "Guatemala", area = "GlobalMix, urban", max_time = 730)
df_gt_prem <- process_epi_data(df.prem.gt, country = "Guatemala", area = "Prem et al., 2021", max_time = 730)

# Combine all Guatemala
df_gt <- bind_rows(df_gt_rur, df_gt_urb, df_gt_prem)

# Repeat for India
df_ind_rur <- process_epi_data(df.no.rur.ind, country = "India", area = "GlobalMix, rural", max_time = 730)
df_ind_urb <- process_epi_data(df.no.urb.ind, country = "India", area = "GlobalMix urban", max_time = 730)
df_ind_prem <- process_epi_data(df.prem.ind, country = "India", area = "Prem et al., 2021", max_time = 730)

df_ind <- bind_rows(df_ind_rur, df_ind_urb, df_ind_prem)

# Same for Mozambique
df_moz_rur <- process_epi_data(df.no.rur.moz, country = "Mozambique", area = "GlobalMix, rural", max_time = 730)
df_moz_urb <- process_epi_data(df.no.urb.moz, country = "Mozambique", area = "GlobalMix, urban", max_time = 730)
df_moz_prem <- process_epi_data(df.prem.moz, country = "Mozambique", area = "Prem et al., 2021", max_time = 730)

df_moz <- bind_rows(df_moz_rur, df_moz_urb, df_moz_prem)

# Same for Pakistan
df_pak_rur <- process_epi_data(df.no.rur.pak, country = "Pakistan", area = "GlobalMix, rural", max_time = 730)
df_pak_urb <- process_epi_data(df.no.urb.pak, country = "Pakistan", area = "GlobalMix, urban", max_time = 730)
df_pak_prem <- process_epi_data(df.prem.pak, country = "Pakistan", area = "Prem et al., 2021", max_time = 730)

df_pak <- bind_rows(df_pak_rur, df_pak_urb, df_pak_prem)


df_all_epi <- bind_rows(df_gt, df_ind, df_moz, df_pak)

ggplot(df_all_epi, aes(x = t_end, y = mean_r, color = area)) +
  geom_line() +
  #geom_ribbon(aes(ymin = lq, ymax = uq, fill = area), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ country, ncol = 2, scales = "free") +
  labs(x = "Time", y = "Estimated R", color = NULL, fill = NULL) + 
  scale_color_manual(values = c("GlobalMix, rural" = "aquamarine4", "GlobalMix, urban" = "steelblue3", "Prem et al., 2021" = "sienna")) +
  #scale_fill_manual(values = c("Rural" = "aquamarine4", "Urban" = "steelblue3", "Prem" = "sienna")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") ->sfig8

# --- proportion of infections
# gt
(sum(df.no.rur.gt$si.flow.1) + sum(df.no.rur.gt$si.flow.2) + sum(df.no.rur.gt$si.flow.3)) / sum(df.no.rur.gt$si.flow) # 12.2
(sum(df.no.urb.gt$si.flow.1) + sum(df.no.urb.gt$si.flow.2) + sum(df.no.urb.gt$si.flow.3)) / sum(df.no.urb.gt$si.flow) # 10.6
# ind
(sum(df.no.rur.ind$si.flow.1) + sum(df.no.rur.ind$si.flow.2) + sum(df.no.rur.ind$si.flow.3)) / sum(df.no.rur.ind$si.flow) # 9.8
(sum(df.no.urb.ind$si.flow.1) + sum(df.no.urb.ind$si.flow.2) + sum(df.no.urb.ind$si.flow.3)) / sum(df.no.urb.ind$si.flow) # 9.9
# moz
(sum(df.no.rur.moz$si.flow.1) + sum(df.no.rur.moz$si.flow.2) + sum(df.no.rur.moz$si.flow.3)) / sum(df.no.rur.moz$si.flow) # 14.4
(sum(df.no.urb.moz$si.flow.1) + sum(df.no.urb.moz$si.flow.2) + sum(df.no.urb.moz$si.flow.3)) / sum(df.no.urb.moz$si.flow) # 12.0
# pak 
(sum(df.no.rur.pak$si.flow.1) + sum(df.no.rur.pak$si.flow.2) + sum(df.no.rur.pak$si.flow.3)) / sum(df.no.rur.pak$si.flow) # 16.6
(sum(df.no.urb.pak$si.flow.1) + sum(df.no.urb.pak$si.flow.2) + sum(df.no.urb.pak$si.flow.3)) / sum(df.no.urb.pak$si.flow) # 13.2

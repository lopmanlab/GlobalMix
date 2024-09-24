# --- Library
library(EpiModel)
library(ggplot2)
library(patchwork)


# --- Model structure
sirmod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # 1. track total sizes of each age group
    num.1 <- s.num.1 + s.v.num.1 + i.num.1 + i.v.num.1 + r.num.1 + r.v.num.1
    num.2 <- s.num.2 + s.v.num.2 + i.num.2 + i.v.num.2 + r.num.2 + r.v.num.2
    num.3 <- s.num.3 + s.v.num.3 + i.num.3 + i.v.num.3 + r.num.3 + r.v.num.3
    num.4 <- s.num.4 + s.v.num.4 + i.num.4 + i.v.num.4 + r.num.4 + r.v.num.4
    num.5 <- s.num.5 + s.v.num.5 + i.num.5 + i.v.num.5 + r.num.5 + r.v.num.5
    num.6 <- s.num.6 + s.v.num.6 + i.num.6 + i.v.num.6 + r.num.6 + r.v.num.6
    num.7 <- s.num.7 + s.v.num.7 + i.num.7 + i.v.num.7 + r.num.7 + r.v.num.7
    num.8 <- s.num.8 + s.v.num.8 + i.num.8 + i.v.num.9 + r.num.8 + r.v.num.8
    num.9 <- s.num.9 + s.v.num.9 + i.num.9 + i.v.num.1 + r.num.9 + r.v.num.9
    
    # 2. define age-specific forces of infection
    # force of infection for <6m0 contacts (infecting the <60mo year olds)
    # unvaccinated 
    lambda.1 <- q*c11*((i.num.1 + i.v.num.1)/num.1) + 
      q*c12*((i.num.2 + i.v.num.2)/num.2) + # c12 is contact between <6mo with 6-11mo
      q*c13*((i.num.3 + i.v.num.3)/num.3) +  # c13 is contact between <6mo with 1-4 y/o, etc
      q*c14*((i.num.4 + i.v.num.4)/num.4) +
      q*c15*((i.num.5 + i.v.num.5)/num.5) + 
      q*c16*((i.num.6 + i.v.num.6)/num.6) + 
      q*c17*((i.num.7 + i.v.num.7)/num.7) + 
      q*c18*((i.num.8 + i.v.num.8)/num.8) + 
      q*c19*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.1 <- (1-psi)*q*c11*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c12*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c13*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c14*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c15*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c16*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c17*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c18*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c19*((i.num.9 + i.v.num.9)/num.9) 
    
    # force of infection for 6-11mo contacts (infecting the 6-11 mo)
    # unvaccinated 
    lambda.2 <- q*c21*((i.num.1 + i.v.num.1)/num.1) + 
      q*c22*((i.num.2 + i.v.num.2)/num.2) + 
      q*c23*((i.num.3 + i.v.num.3)/num.3) + 
      q*c24*((i.num.4 + i.v.num.4)/num.4) +
      q*c25*((i.num.5 + i.v.num.5)/num.5) + 
      q*c26*((i.num.6 + i.v.num.6)/num.6) +
      q*c27*((i.num.7 + i.v.num.7)/num.7) +
      q*c28*((i.num.8 + i.v.num.8)/num.8) +
      q*c29*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.2 <- (1-psi)*q*c21*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c22*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c23*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c24*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c25*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c26*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c27*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c28*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c29*((i.num.9 + i.v.num.9)/num.9)
    
    # force of infection for 1-4 y contacts (infecting the 1-4y )
    # unvaccinated
    lambda.3 <- q*c31*((i.num.1 + i.v.num.1)/num.1) + 
      q*c32*((i.num.2 + i.v.num.2)/num.2) + 
      q*c33*((i.num.3 + i.v.num.3)/num.3) + 
      q*c34*((i.num.4 + i.v.num.4)/num.4) +
      q*c35*((i.num.5 + i.v.num.5)/num.5) + 
      q*c36*((i.num.6 + i.v.num.6)/num.6) +
      q*c37*((i.num.7 + i.v.num.7)/num.7) +
      q*c38*((i.num.8 + i.v.num.8)/num.8) +
      q*c39*((i.num.9 + i.v.num.9)/num.9)
    
    lambda.v.3 <- (1-psi)*q*c31*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c32*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c33*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c34*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c35*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c36*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c37*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c38*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c39*((i.num.9 + i.v.num.9)/num.9)
    
    
    # force of infection for 5-9y contacts (infecting the 5-9y)
    # unvaccinated 
    lambda.4 <- q*c41*((i.num.1 + i.v.num.1)/num.1) + 
      q*c42*((i.num.2 + i.v.num.2)/num.2) + 
      q*c43*((i.num.3 + i.v.num.3)/num.3) + 
      q*c44*((i.num.4 + i.v.num.4)/num.4) +
      q*c45*((i.num.5 + i.v.num.5)/num.5) + 
      q*c46*((i.num.6 + i.v.num.6)/num.6) +
      q*c47*((i.num.7 + i.v.num.7)/num.7) +
      q*c48*((i.num.8 + i.v.num.8)/num.8) +
      q*c49*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.4 <- (1-psi)*q*c41*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c42*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c43*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c44*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c45*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c46*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c47*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c48*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c49*((i.num.9 + i.v.num.9)/num.9)
    
    # force of infection for 10-19 contacts (infecting the 10-19 year olds)
    # unvaccinated
    lambda.5 <- q*c51*((i.num.1 + i.v.num.1)/num.1) + 
      q*c52*((i.num.2 + i.v.num.2)/num.2) + 
      q*c53*((i.num.3 + i.v.num.3)/num.3) + 
      q*c54*((i.num.4 + i.v.num.4)/num.4) +
      q*c55*((i.num.5 + i.v.num.5)/num.5) + 
      q*c56*((i.num.6 + i.v.num.6)/num.6) +
      q*c57*((i.num.7 + i.v.num.7)/num.7) +
      q*c58*((i.num.8 + i.v.num.8)/num.8) +
      q*c59*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.5 <- (1-psi)*q*c51*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c52*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c53*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c54*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c55*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c56*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c57*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c58*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c59*((i.num.9 + i.v.num.9)/num.9)
    
    # force of infection for 20-29y contacts (infecting the 20-29 year olds)
    # unvaccinated
    lambda.6 <- q*c61*((i.num.1 + i.v.num.1)/num.1) + 
      q*c62*((i.num.2 + i.v.num.2)/num.2) + 
      q*c63*((i.num.3 + i.v.num.3)/num.3) + 
      q*c64*((i.num.4 + i.v.num.4)/num.4) +
      q*c65*((i.num.5 + i.v.num.5)/num.5) + 
      q*c66*((i.num.6 + i.v.num.6)/num.6) +
      q*c67*((i.num.7 + i.v.num.7)/num.7) +
      q*c68*((i.num.8 + i.v.num.8)/num.8) +
      q*c69*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.6 <- (1-psi)*q*c61*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c62*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c63*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c64*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c65*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c66*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c67*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c68*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c69*((i.num.9 + i.v.num.9)/num.9)
    
    # force of infection for 30-39y contacts (infecting the 30-39 year olds)
    # unvaccinated
    lambda.7 <- q*c71*((i.num.1 + i.v.num.1)/num.1) + 
      q*c72*((i.num.2 + i.v.num.2)/num.2) + 
      q*c73*((i.num.3 + i.v.num.3)/num.3) + 
      q*c74*((i.num.4 + i.v.num.4)/num.4) +
      q*c75*((i.num.5 + i.v.num.5)/num.5) + 
      q*c76*((i.num.6 + i.v.num.6)/num.6) +
      q*c77*((i.num.7 + i.v.num.7)/num.7) +
      q*c78*((i.num.8 + i.v.num.8)/num.8) +
      q*c79*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.7 <- (1-psi)*q*c71*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c72*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c73*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c74*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c75*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c76*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c77*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c78*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c79*((i.num.9 + i.v.num.9)/num.9)
    
    # force of infection for 40-59y contacts (infecting the 40-59 year olds)
    # unvaccinated
    lambda.8 <- q*c81*((i.num.1 + i.v.num.1)/num.1) + 
      q*c82*((i.num.2 + i.v.num.2)/num.2) + 
      q*c83*((i.num.3 + i.v.num.3)/num.3) + 
      q*c84*((i.num.4 + i.v.num.4)/num.4) +
      q*c85*((i.num.5 + i.v.num.5)/num.5) + 
      q*c86*((i.num.6 + i.v.num.6)/num.6) +
      q*c87*((i.num.7 + i.v.num.7)/num.7) +
      q*c88*((i.num.8 + i.v.num.8)/num.8) +
      q*c89*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.8 <- (1-psi)*q*c81*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c82*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c83*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c84*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c85*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c86*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c87*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c88*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c89*((i.num.9 + i.v.num.9)/num.9)
    
    # force of infection for 60+y contacts (infecting the 60+ year olds)
    # unvaccinated
    lambda.9 <- q*c91*((i.num.1 + i.v.num.1)/num.1) + 
      q*c92*((i.num.2 + i.v.num.2)/num.2) + 
      q*c93*((i.num.3 + i.v.num.3)/num.3) + 
      q*c94*((i.num.4 + i.v.num.4)/num.4) +
      q*c95*((i.num.5 + i.v.num.5)/num.5) + 
      q*c96*((i.num.6 + i.v.num.6)/num.6) +
      q*c97*((i.num.7 + i.v.num.7)/num.7) +
      q*c98*((i.num.8 + i.v.num.8)/num.8) +
      q*c99*((i.num.9 + i.v.num.9)/num.9)
    
    # vaccinated
    lambda.v.9 <- (1-psi)*q*c91*((i.num.1 + i.v.num.1)/num.1) + 
      (1-psi)*q*c92*((i.num.2 + i.v.num.2)/num.2) + 
      (1-psi)*q*c93*((i.num.3 + i.v.num.3)/num.3) + 
      (1-psi)*q*c94*((i.num.4 + i.v.num.4)/num.4) +
      (1-psi)*q*c95*((i.num.5 + i.v.num.5)/num.5) + 
      (1-psi)*q*c96*((i.num.6 + i.v.num.6)/num.6) +
      (1-psi)*q*c97*((i.num.7 + i.v.num.7)/num.7) +
      (1-psi)*q*c98*((i.num.8 + i.v.num.8)/num.8) +
      (1-psi)*q*c99*((i.num.9 + i.v.num.9)/num.9)
    
    # 3. differential equations 
    dS.1 <- -lambda.1*s.num.1
    dS.v.1 <- -lambda.v.1*s.v.num.1
    dI.1 <- lambda.1*s.num.1 - gamma*i.num.1
    dI.v.1 <- lambda.v.1*s.v.num.1 - gamma*i.v.num.1
    dR.1 <- gamma*i.num.1
    dR.v.1 <- gamma*i.v.num.1
    
    dS.2 <- -lambda.2*s.num.2
    dS.v.2 <- -lambda.v.2*s.v.num.2
    dI.2 <- lambda.2*s.num.2 - gamma*i.num.2
    dI.v.2 <- lambda.v.2*s.v.num.2 - gamma*i.v.num.2
    dR.2 <- gamma*i.num.2
    dR.v.2 <- gamma*i.v.num.2
    
    dS.3 <- -lambda.3*s.num.3
    dS.v.3 <- -lambda.v.3*s.v.num.3
    dI.3 <- lambda.3*s.num.3 - gamma*i.num.3
    dI.v.3 <- lambda.v.3*s.v.num.3 - gamma*i.v.num.3
    dR.3 <- gamma*i.num.3
    dR.v.3 <- gamma*i.v.num.3
    
    dS.4 <- -lambda.4*s.num.4
    dS.v.4 <- -lambda.v.4*s.v.num.4
    dI.4 <- lambda.4*s.num.4 - gamma*i.num.4
    dI.v.4 <- lambda.v.4*s.v.num.4 - gamma*i.v.num.4
    dR.4 <- gamma*i.num.4
    dR.v.4 <- gamma*i.v.num.4
    
    dS.5 <- -lambda.5*s.num.5
    dS.v.5 <- -lambda.v.5*s.v.num.5
    dI.5 <- lambda.5*s.num.5 - gamma*i.num.5
    dI.v.5 <- lambda.v.5*s.v.num.5 - gamma*i.v.num.5
    dR.5 <- gamma*i.num.5
    dR.v.5 <- gamma*i.v.num.5
    
    dS.6 <- -lambda.6*s.num.6
    dS.v.6 <- -lambda.v.6*s.v.num.6
    dI.6 <- lambda.6*s.num.6 - gamma*i.num.6
    dI.v.6 <- lambda.v.6*s.v.num.6 - gamma*i.v.num.6
    dR.6 <- gamma*i.num.6
    dR.v.6 <- gamma*i.v.num.6
    
    dS.7 <- -lambda.7*s.num.7
    dS.v.7 <- -lambda.v.7*s.v.num.7
    dI.7 <- lambda.7*s.num.7 - gamma*i.num.7
    dI.v.7 <- lambda.v.7*s.v.num.7 - gamma*i.v.num.7
    dR.7 <- gamma*i.num.7
    dR.v.7 <- gamma*i.v.num.7
    
    dS.8 <- -lambda.8*s.num.8
    dS.v.8 <- -lambda.v.8*s.v.num.8
    dI.8 <- lambda.8*s.num.8 - gamma*i.num.8
    dI.v.8 <- lambda.v.8*s.v.num.8 - gamma*i.v.num.8
    dR.8 <- gamma*i.num.8
    dR.v.8 <- gamma*i.v.num.8
    
    dS.9 <- -lambda.9*s.num.9
    dS.v.9 <- -lambda.v.9*s.v.num.9
    dI.9 <- lambda.9*s.num.9 - gamma*i.num.9
    dI.v.9 <- lambda.v.9*s.v.num.9 - gamma*i.v.num.9
    dR.9 <- gamma*i.num.9
    dR.v.9 <- gamma*i.v.num.9
    
    
    # 4. List outputs
    list(c(dS.1, dS.v.1, dI.1, dI.v.1, dR.1, dR.v.1,
           dS.2, dS.v.2, dI.2, dI.v.2, dR.2, dR.v.2,
           dS.3, dS.v.3, dI.3, dI.v.3, dR.3, dR.v.3,
           dS.4, dS.v.4, dI.4, dI.v.4, dR.4, dR.v.4,
           dS.5, dS.v.5, dI.5, dI.v.5, dR.5, dR.v.5,
           dS.6, dS.v.6, dI.6, dI.v.6, dR.6, dR.v.6,
           dS.7, dS.v.7, dI.7, dI.v.7, dR.7, dR.v.7,
           dS.8, dS.v.8, dI.8, dI.v.8, dR.8, dR.v.8,
           dS.9, dS.v.9, dI.9, dI.v.9, dR.9, dR.v.9,
           si.flow.1 = lambda.1*s.num.1 + lambda.v.1*s.v.num.1,
           si.flow.2 = lambda.2*s.num.2 + lambda.v.2*s.v.num.2,
           si.flow.3 = lambda.3*s.num.3 + lambda.v.3*s.v.num.3,
           si.flow.4 = lambda.4*s.num.4 + lambda.v.4*s.v.num.4,
           si.flow.5 = lambda.5*s.num.5 + lambda.v.5*s.v.num.5,
           si.flow.6 = lambda.6*s.num.6 + lambda.v.6*s.v.num.6,
           si.flow.7 = lambda.7*s.num.7 + lambda.v.7*s.v.num.7,
           si.flow.6 = lambda.8*s.num.8 + lambda.v.8*s.v.num.8,
           si.flow.6 = lambda.9*s.num.9 + lambda.v.9*s.v.num.9,
           svi.flow.1 = lambda.v.1*s.v.num.1, svi.flow.2 = lambda.v.2*s.v.num.2,
           svi.flow.3 = lambda.v.3*s.v.num.3, svi.flow.4 = lambda.v.4*s.v.num.4, 
           svi.flow.5 = lambda.v.5*s.v.num.5, svi.flow.6 = lambda.v.6*s.v.num.6,
           svi.flow.7 = lambda.v.7*s.v.num.7, svi.flow.8 = lambda.v.8*s.v.num.8,
           svi.flow.9 = lambda.v.9*s.v.num.9,
           sni.flow.1 = lambda.1*s.num.1, sni.flow.2 = lambda.2*s.num.2,
           sni.flow.3 = lambda.3*s.num.3, sni.flow.4 = lambda.4*s.num.4, 
           sni.flow.5 = lambda.5*s.num.5, sni.flow.6 = lambda.6*s.num.6,
           sni.flow.7 = lambda.7*s.num.7, sni.flow.8 = lambda.8*s.num.8,
           sni.flow.9 = lambda.9*s.num.9
    ))
  })
}



# --- Parameters
# - Urban
param.urban <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                         c11=0.15, c12=0.04, c13=1.07, c14=0.51, c15=1.02, c16=3.55, c17=1.93, c18=2.60, c19=0.82,
                         c21=0.07, c22=0.07, c23=1.09, c24=0.96, c25=0.96, c26=3.76, c27=2.42, c28=2.47, c29=0.80,
                         c31=0.22, c32=0.22, c33=2.38, c34=1.37, c35=1.14, c36=3.17, c37=2.42, c38=1.98, c39=0.82,
                         c41=0.03, c42=0.03, c43=0.92, c44=6.89, c45=2.89, c46=1.57, c47=3.65, c48=2.29, c49=0.86,
                         c51=0.06, c52=0.00, c53=0.30, c54=0.77, c55=9.66, c56=1.55, c57=2.86, c58=2.62, c59=0.71,
                         c61=0.06, c62=0.16, c63=1.03, c64=0.68, c65=1.54, c66=5.52, c67=2.11, c68=2.83, c69=0.78,
                         c71=0.10, c72=0.06, c73=0.92, c74=1.16, c75=2.25, c76=2.33, c77=4.37, c78=2.97, c79=0.87,
                         c81=0.03, c82=0.00, c83=0.80, c84=0.50, c85=1.73, c86=2.69, c87=3.16, c88=5.42, c89=1.00,
                         c91=0.03, c92=0.00, c93=0.47, c94=0.58, c95=1.16, c96=2.27, c97=2.24, c98=4.23, c99=2.08)

param.urban.lt1 <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02,
                             c11=0.15, c12=0.04, c13=1.07, c14=0.51, c15=1.02, c16=3.55, c17=1.93, c18=2.60, c19=0.82,
                             c21=0.07, c22=0.07, c23=1.09, c24=0.96, c25=0.96, c26=3.76, c27=2.42, c28=2.47, c29=0.80,
                             c31=0.22, c32=0.22, c33=2.38, c34=1.37, c35=1.14, c36=3.17, c37=2.42, c38=1.98, c39=0.82,
                             c41=0.03, c42=0.03, c43=0.92, c44=6.89, c45=2.89, c46=1.57, c47=3.65, c48=2.29, c49=0.86,
                             c51=0.06, c52=0.00, c53=0.30, c54=0.77, c55=9.66, c56=1.55, c57=2.86, c58=2.62, c59=0.71,
                             c61=0.06, c62=0.16, c63=1.03, c64=0.68, c65=1.54, c66=5.52, c67=2.11, c68=2.83, c69=0.78,
                             c71=0.10, c72=0.06, c73=0.92, c74=1.16, c75=2.25, c76=2.33, c77=4.37, c78=2.97, c79=0.87,
                             c81=0.03, c82=0.00, c83=0.80, c84=0.50, c85=1.73, c86=2.69, c87=3.16, c88=5.42, c89=1.00,
                             c91=0.03, c92=0.00, c93=0.47, c94=0.58, c95=1.16, c96=2.27, c97=2.24, c98=4.23, c99=2.08)

param.urban.aya <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
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
param.rural <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                         c11=0.31, c12=0.10, c13=0.79, c14=0.34, c15=1.10, c16=2.76, c17=2.07, c18=2.52, c19=1.09,
                         c21=0.07, c22=0.00, c23=0.35, c24=0.20, c25=0.98, c26=2.24, c27=1.35, c28=2.28, c29=1.06,
                         c31=0.26, c32=0.13, c33=2.25, c34=1.64, c35=1.08, c36=2.33, c37=3.15, c38=2.54, c39=1.82,
                         c41=0.08, c42=0.11, c43=1.56, c44=6.60, c45=2.98, c46=1.05, c47=3.51, c48=2.08, c49=1.51,
                         c51=0.03, c52=0.03, c53=0.50, c54=0.90, c55=10.06, c56=1.82, c57=2.49, c58=3.39, c59=1.56,
                         c61=0.06, c62=0.00, c63=0.65, c64=0.63, c65=1.53, c66=4.58, c67=2.39, c68=3.74, c69=1.42,
                         c71=0.03, c72=0.03, c73=1.11, c74=1.05, c75=2.05, c76=1.35, c77=3.70, c78=3.40, c79=1.98,
                         c81=0.06, c82=0.01, c83=0.51, c84=0.55, c85=1.38, c86=2.17, c87=2.32, c88=4.46, c89=1.89,
                         c91=0.00, c92=0.00, c93=0.24, c94=0.60, c95=1.13, c96=0.84, c97=1.89, c98=4.08, c99=2.37)

param.rural.lt1 <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                             c11=0.31, c12=0.10, c13=0.79, c14=0.34, c15=1.10, c16=2.76, c17=2.07, c18=2.52, c19=1.09,
                             c21=0.07, c22=0.00, c23=0.35, c24=0.20, c25=0.98, c26=2.24, c27=1.35, c28=2.28, c29=1.06,
                             c31=0.26, c32=0.13, c33=2.25, c34=1.64, c35=1.08, c36=2.33, c37=3.15, c38=2.54, c39=1.82,
                             c41=0.08, c42=0.11, c43=1.56, c44=6.60, c45=2.98, c46=1.05, c47=3.51, c48=2.08, c49=1.51,
                             c51=0.03, c52=0.03, c53=0.50, c54=0.90, c55=10.06, c56=1.82, c57=2.49, c58=3.39, c59=1.56,
                             c61=0.06, c62=0.00, c63=0.65, c64=0.63, c65=1.53, c66=4.58, c67=2.39, c68=3.74, c69=1.42,
                             c71=0.03, c72=0.03, c73=1.11, c74=1.05, c75=2.05, c76=1.35, c77=3.70, c78=3.40, c79=1.98,
                             c81=0.06, c82=0.01, c83=0.51, c84=0.55, c85=1.38, c86=2.17, c87=2.32, c88=4.46, c89=1.89,
                             c91=0.00, c92=0.00, c93=0.24, c94=0.60, c95=1.13, c96=0.84, c97=1.89, c98=4.08, c99=2.37)

param.rural.aya <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
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
init.urb <- init.dcm(s.num.1 = 10900000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                     s.num.2 = 10900000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                     s.num.3 = 44700000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                     s.num.4 = 59600000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                     s.num.5 = 109300000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                     s.num.6 = 89500000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                     s.num.7 = 64600000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                     s.num.8 = 69600000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                     s.num.9 = 39800000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.urb.lt1 <- init.dcm(s.num.1 = 10900000*(1-0.50), s.v.num.1 = 10900000*0.50, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 10900000*(1-0.50), s.v.num.2 = 10900000*0.50, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 44700000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 59600000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 109300000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 89500000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 64600000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 69600000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 39800000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.urb.aya <- init.dcm(s.num.1 = 10900000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 10900000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 44700000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 59600000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 109300000*(1-0.50), s.v.num.5 = 109300000*0.50, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 89500000*(1-0.50), s.v.num.6 = 89500000*0.50, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 64600000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 69600000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 39800000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)
# - Rural
init.rur <- init.dcm(s.num.1 = 20300000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                     s.num.2 = 20300000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                     s.num.3 = 83100000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                     s.num.4 = 110800000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                     s.num.5 = 203100000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                     s.num.6 = 166100000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                     s.num.7 = 120000000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                     s.num.8 = 129200000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                     s.num.9 = 73800000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.rur.lt1 <- init.dcm(s.num.1 = 20300000*(1-0.50), s.v.num.1 = 20300000*0.50, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 20300000*(1-0.50), s.v.num.2 = 20300000*0.50, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 83100000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 110800000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 203100000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 166100000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 120000000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 129200000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 73800000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.rur.aya <- init.dcm(s.num.1 = 20300000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 20300000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 83100000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 110800000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 203100000*(1-0.50), s.v.num.5 = 203100000*0.50, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 166100000*(1-0.50), s.v.num.6 = 166100000*0.50, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 120000000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 129200000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 73800000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)



#--- Controls 
control <- control.dcm(nstep=730, new.mod = sirmod)




# --- Compile model GlobalMix urban
sim.no.urb <- dcm(param.urban, init.urb, control)
df.no.urb <- as.data.frame(sim.no.urb)

sim.lt1.urb <- dcm(param.urban.lt1, init.urb.lt1, control)
df.lt1.urb <- as.data.frame(sim.lt1.urb)

sim.aya.urb <- dcm(param.urban.aya, init.urb.aya, control)
df.aya.urb <- as.data.frame(sim.aya.urb)

# --- Compile model GlobalMix rural
sim.no.rur <- dcm(param.rural, init.rur, control)
df.no.rur <- as.data.frame(sim.no.rur)

sim.lt1.rur <- dcm(param.rural.lt1, init.rur.lt1, control)
df.lt1.rur <- as.data.frame(sim.lt1.rur)

sim.aya.rur <- dcm(param.rural.aya, init.rur.aya, control)
df.aya.rur <- as.data.frame(sim.aya.rur)




# --- Make a dataframe with all India results
df.ind <- matrix(ncol=0, nrow=730)
df.ind <- as.data.frame(df.ind)

# -- Urban
# - No vaccine
df.ind$si.no.urb.ind.1 <- df.no.urb$si.flow.1
df.ind$si.no.urb.ind.2 <- df.no.urb$si.flow.2
df.ind$si.no.urb.ind.3 <- df.no.urb$si.flow.3
df.ind$si.no.urb.ind.4 <- df.no.urb$si.flow.4
df.ind$si.no.urb.ind.5 <- df.no.urb$si.flow.5
df.ind$si.no.urb.ind.6 <- df.no.urb$si.flow.6
df.ind$si.no.urb.ind.7 <- df.no.urb$si.flow.7
df.ind$si.no.urb.ind.8 <- df.no.urb$si.flow.8
df.ind$si.no.urb.ind.9 <- df.no.urb$si.flow.9

# Vaccine less than 1
df.ind$si.lt1.urb.ind.1 <- df.lt1.urb$si.flow.1
df.ind$si.lt1.urb.ind.2 <- df.lt1.urb$si.flow.2
df.ind$si.lt1.urb.ind.3 <- df.lt1.urb$si.flow.3
df.ind$si.lt1.urb.ind.4 <- df.lt1.urb$si.flow.4
df.ind$si.lt1.urb.ind.5 <- df.lt1.urb$si.flow.5
df.ind$si.lt1.urb.ind.6 <- df.lt1.urb$si.flow.6
df.ind$si.lt1.urb.ind.7 <- df.lt1.urb$si.flow.7
df.ind$si.lt1.urb.ind.8 <- df.lt1.urb$si.flow.8
df.ind$si.lt1.urb.ind.9 <- df.lt1.urb$si.flow.9
df.ind$svi.lt1.urb.ind.1 <- df.lt1.urb$svi.flow.1
df.ind$svi.lt1.urb.ind.2 <- df.lt1.urb$svi.flow.2
df.ind$svi.lt1.urb.ind.3 <- df.lt1.urb$svi.flow.3
df.ind$svi.lt1.urb.ind.4 <- df.lt1.urb$svi.flow.4
df.ind$svi.lt1.urb.ind.5 <- df.lt1.urb$svi.flow.5
df.ind$svi.lt1.urb.ind.6 <- df.lt1.urb$svi.flow.6
df.ind$svi.lt1.urb.ind.7 <- df.lt1.urb$svi.flow.7
df.ind$svi.lt1.urb.ind.8 <- df.lt1.urb$svi.flow.8
df.ind$svi.lt1.urb.ind.9 <- df.lt1.urb$svi.flow.9
df.ind$sni.lt1.urb.ind.1 <- df.lt1.urb$sni.flow.1
df.ind$sni.lt1.urb.ind.2 <- df.lt1.urb$sni.flow.2
df.ind$sni.lt1.urb.ind.3 <- df.lt1.urb$sni.flow.3
df.ind$sni.lt1.urb.ind.4 <- df.lt1.urb$sni.flow.4
df.ind$sni.lt1.urb.ind.5 <- df.lt1.urb$sni.flow.5
df.ind$sni.lt1.urb.ind.6 <- df.lt1.urb$sni.flow.6
df.ind$sni.lt1.urb.ind.7 <- df.lt1.urb$sni.flow.7
df.ind$sni.lt1.urb.ind.8 <- df.lt1.urb$sni.flow.8
df.ind$sni.lt1.urb.ind.9 <- df.lt1.urb$sni.flow.9

# Vaccine in 10-29 y/o (adolescents and young adults)
df.ind$si.aya.urb.ind.1 <- df.aya.urb$si.flow.1
df.ind$si.aya.urb.ind.2 <- df.aya.urb$si.flow.2
df.ind$si.aya.urb.ind.3 <- df.aya.urb$si.flow.3
df.ind$si.aya.urb.ind.4 <- df.aya.urb$si.flow.4
df.ind$si.aya.urb.ind.5 <- df.aya.urb$si.flow.5
df.ind$si.aya.urb.ind.6 <- df.aya.urb$si.flow.6
df.ind$si.aya.urb.ind.7 <- df.aya.urb$si.flow.7
df.ind$si.aya.urb.ind.8 <- df.aya.urb$si.flow.8
df.ind$si.aya.urb.ind.9 <- df.aya.urb$si.flow.9
df.ind$svi.aya.urb.ind.1 <- df.aya.urb$svi.flow.1
df.ind$svi.aya.urb.ind.2 <- df.aya.urb$svi.flow.2
df.ind$svi.aya.urb.ind.3 <- df.aya.urb$svi.flow.3
df.ind$svi.aya.urb.ind.4 <- df.aya.urb$svi.flow.4
df.ind$svi.aya.urb.ind.5 <- df.aya.urb$svi.flow.5
df.ind$svi.aya.urb.ind.6 <- df.aya.urb$svi.flow.6
df.ind$svi.aya.urb.ind.7 <- df.aya.urb$svi.flow.7
df.ind$svi.aya.urb.ind.8 <- df.aya.urb$svi.flow.8
df.ind$svi.aya.urb.ind.9 <- df.aya.urb$svi.flow.9
df.ind$sni.aya.urb.ind.1 <- df.aya.urb$sni.flow.1
df.ind$sni.aya.urb.ind.2 <- df.aya.urb$sni.flow.2
df.ind$sni.aya.urb.ind.3 <- df.aya.urb$sni.flow.3
df.ind$sni.aya.urb.ind.4 <- df.aya.urb$sni.flow.4
df.ind$sni.aya.urb.ind.5 <- df.aya.urb$sni.flow.5
df.ind$sni.aya.urb.ind.6 <- df.aya.urb$sni.flow.6
df.ind$sni.aya.urb.ind.7 <- df.aya.urb$sni.flow.7
df.ind$sni.aya.urb.ind.8 <- df.aya.urb$sni.flow.8
df.ind$sni.aya.urb.ind.9 <- df.aya.urb$sni.flow.9


# -- Rural
# - No vaccine
df.ind$si.no.rur.ind.1 <- df.no.rur$si.flow.1
df.ind$si.no.rur.ind.2 <- df.no.rur$si.flow.2
df.ind$si.no.rur.ind.3 <- df.no.rur$si.flow.3
df.ind$si.no.rur.ind.4 <- df.no.rur$si.flow.4
df.ind$si.no.rur.ind.5 <- df.no.rur$si.flow.5
df.ind$si.no.rur.ind.6 <- df.no.rur$si.flow.6
df.ind$si.no.rur.ind.7 <- df.no.rur$si.flow.7
df.ind$si.no.rur.ind.8 <- df.no.rur$si.flow.8
df.ind$si.no.rur.ind.9 <- df.no.rur$si.flow.9

# Vaccine less than 1
df.ind$si.lt1.rur.ind.1 <- df.lt1.rur$si.flow.1
df.ind$si.lt1.rur.ind.2 <- df.lt1.rur$si.flow.2
df.ind$si.lt1.rur.ind.3 <- df.lt1.rur$si.flow.3
df.ind$si.lt1.rur.ind.4 <- df.lt1.rur$si.flow.4
df.ind$si.lt1.rur.ind.5 <- df.lt1.rur$si.flow.5
df.ind$si.lt1.rur.ind.6 <- df.lt1.rur$si.flow.6
df.ind$si.lt1.rur.ind.7 <- df.lt1.rur$si.flow.7
df.ind$si.lt1.rur.ind.8 <- df.lt1.rur$si.flow.8
df.ind$si.lt1.rur.ind.9 <- df.lt1.rur$si.flow.9
df.ind$svi.lt1.rur.ind.1 <- df.lt1.rur$svi.flow.1
df.ind$svi.lt1.rur.ind.2 <- df.lt1.rur$svi.flow.2
df.ind$svi.lt1.rur.ind.3 <- df.lt1.rur$svi.flow.3
df.ind$svi.lt1.rur.ind.4 <- df.lt1.rur$svi.flow.4
df.ind$svi.lt1.rur.ind.5 <- df.lt1.rur$svi.flow.5
df.ind$svi.lt1.rur.ind.6 <- df.lt1.rur$svi.flow.6
df.ind$svi.lt1.rur.ind.7 <- df.lt1.rur$svi.flow.7
df.ind$svi.lt1.rur.ind.8 <- df.lt1.rur$svi.flow.8
df.ind$svi.lt1.rur.ind.9 <- df.lt1.rur$svi.flow.9
df.ind$sni.lt1.rur.ind.1 <- df.lt1.rur$sni.flow.1
df.ind$sni.lt1.rur.ind.2 <- df.lt1.rur$sni.flow.2
df.ind$sni.lt1.rur.ind.3 <- df.lt1.rur$sni.flow.3
df.ind$sni.lt1.rur.ind.4 <- df.lt1.rur$sni.flow.4
df.ind$sni.lt1.rur.ind.5 <- df.lt1.rur$sni.flow.5
df.ind$sni.lt1.rur.ind.6 <- df.lt1.rur$sni.flow.6
df.ind$sni.lt1.rur.ind.7 <- df.lt1.rur$sni.flow.7
df.ind$sni.lt1.rur.ind.8 <- df.lt1.rur$sni.flow.8
df.ind$sni.lt1.rur.ind.9 <- df.lt1.rur$sni.flow.9

# Vaccine in 10-29 y/o (adolescents and young adults)
df.ind$si.aya.rur.ind.1 <- df.aya.rur$si.flow.1
df.ind$si.aya.rur.ind.2 <- df.aya.rur$si.flow.2
df.ind$si.aya.rur.ind.3 <- df.aya.rur$si.flow.3
df.ind$si.aya.rur.ind.4 <- df.aya.rur$si.flow.4
df.ind$si.aya.rur.ind.5 <- df.aya.rur$si.flow.5
df.ind$si.aya.rur.ind.6 <- df.aya.rur$si.flow.6
df.ind$si.aya.rur.ind.7 <- df.aya.rur$si.flow.7
df.ind$si.aya.rur.ind.8 <- df.aya.rur$si.flow.8
df.ind$si.aya.rur.ind.9 <- df.aya.rur$si.flow.9
df.ind$svi.aya.rur.ind.1 <- df.aya.rur$svi.flow.1
df.ind$svi.aya.rur.ind.2 <- df.aya.rur$svi.flow.2
df.ind$svi.aya.rur.ind.3 <- df.aya.rur$svi.flow.3
df.ind$svi.aya.rur.ind.4 <- df.aya.rur$svi.flow.4
df.ind$svi.aya.rur.ind.5 <- df.aya.rur$svi.flow.5
df.ind$svi.aya.rur.ind.6 <- df.aya.rur$svi.flow.6
df.ind$svi.aya.rur.ind.7 <- df.aya.rur$svi.flow.7
df.ind$svi.aya.rur.ind.8 <- df.aya.rur$svi.flow.8
df.ind$svi.aya.rur.ind.9 <- df.aya.rur$svi.flow.9
df.ind$sni.aya.rur.ind.1 <- df.aya.rur$sni.flow.1
df.ind$sni.aya.rur.ind.2 <- df.aya.rur$sni.flow.2
df.ind$sni.aya.rur.ind.3 <- df.aya.rur$sni.flow.3
df.ind$sni.aya.rur.ind.4 <- df.aya.rur$sni.flow.4
df.ind$sni.aya.rur.ind.5 <- df.aya.rur$sni.flow.5
df.ind$sni.aya.rur.ind.6 <- df.aya.rur$sni.flow.6
df.ind$sni.aya.rur.ind.7 <- df.aya.rur$sni.flow.7
df.ind$sni.aya.rur.ind.8 <- df.aya.rur$sni.flow.8
df.ind$sni.aya.rur.ind.9 <- df.aya.rur$sni.flow.9


# -- Time
df.ind$time <- df.aya.rur$time




# ---------- LT1 VE Rural ---------- #
# ----- calculate overall ve
1-((sum(df.ind$si.lt1.rur.ind.1)/(init.rur.lt1$s.num.1+init.rur.lt1$s.v.num.1)) / (sum(df.ind$si.no.rur.ind.1)/(init.rur$s.num.1)))
1-((sum(df.ind$si.lt1.rur.ind.2)/(init.rur.lt1$s.num.2+init.rur.lt1$s.v.num.2)) / (sum(df.ind$si.no.rur.ind.2)/(init.rur$s.num.2)))
1-((sum(df.ind$si.lt1.rur.ind.3)/(init.rur.lt1$s.num.3+init.rur.lt1$s.v.num.3)) / (sum(df.ind$si.no.rur.ind.3)/(init.rur$s.num.3)))
1-((sum(df.ind$si.lt1.rur.ind.4)/(init.rur.lt1$s.num.4+init.rur.lt1$s.v.num.4)) / (sum(df.ind$si.no.rur.ind.4)/(init.rur$s.num.4)))
1-((sum(df.ind$si.lt1.rur.ind.5)/(init.rur.lt1$s.num.5+init.rur.lt1$s.v.num.5)) / (sum(df.ind$si.no.rur.ind.5)/(init.rur$s.num.5)))
1-((sum(df.ind$si.lt1.rur.ind.6)/(init.rur.lt1$s.num.6+init.rur.lt1$s.v.num.6)) / (sum(df.ind$si.no.rur.ind.6)/(init.rur$s.num.6)))
1-((sum(df.ind$si.lt1.rur.ind.7)/(init.rur.lt1$s.num.7+init.rur.lt1$s.v.num.7)) / (sum(df.ind$si.no.rur.ind.7)/(init.rur$s.num.7)))
1-((sum(df.ind$si.lt1.rur.ind.8)/(init.rur.lt1$s.num.8+init.rur.lt1$s.v.num.8)) / (sum(df.ind$si.no.rur.ind.8)/(init.rur$s.num.8)))
1-((sum(df.ind$si.lt1.rur.ind.9)/(init.rur.lt1$s.num.9+init.rur.lt1$s.v.num.9)) / (sum(df.ind$si.no.rur.ind.9)/(init.rur$s.num.9)))

# ----- calculate direct ve
1-((sum(df.ind$svi.lt1.rur.ind.1)/(init.rur.lt1$s.v.num.1)) / (sum(df.ind$sni.lt1.rur.ind.1)/(init.rur.lt1$s.num.1)))
1-((sum(df.ind$svi.lt1.rur.ind.2)/(init.rur.lt1$s.v.num.2)) / (sum(df.ind$sni.lt1.rur.ind.2)/(init.rur.lt1$s.num.2)))

# ----- calculate indirect ve
1-((sum(df.ind$sni.lt1.rur.ind.1)/(init.rur.lt1$s.num.1)) / (sum(df.ind$si.no.rur.ind.1)/(init.rur$s.num.1)))
1-((sum(df.ind$sni.lt1.rur.ind.2)/(init.rur.lt1$s.num.2)) / (sum(df.ind$si.no.rur.ind.2)/(init.rur$s.num.2)))
1-((sum(df.ind$sni.lt1.rur.ind.3)/(init.rur.lt1$s.num.3)) / (sum(df.ind$si.no.rur.ind.3)/(init.rur$s.num.3)))
1-((sum(df.ind$sni.lt1.rur.ind.4)/(init.rur.lt1$s.num.4)) / (sum(df.ind$si.no.rur.ind.4)/(init.rur$s.num.4)))
1-((sum(df.ind$sni.lt1.rur.ind.5)/(init.rur.lt1$s.num.5)) / (sum(df.ind$si.no.rur.ind.5)/(init.rur$s.num.5)))
1-((sum(df.ind$sni.lt1.rur.ind.6)/(init.rur.lt1$s.num.6)) / (sum(df.ind$si.no.rur.ind.6)/(init.rur$s.num.6)))
1-((sum(df.ind$sni.lt1.rur.ind.7)/(init.rur.lt1$s.num.7)) / (sum(df.ind$si.no.rur.ind.7)/(init.rur$s.num.7)))
1-((sum(df.ind$sni.lt1.rur.ind.8)/(init.rur.lt1$s.num.8)) / (sum(df.ind$si.no.rur.ind.8)/(init.rur$s.num.8)))
1-((sum(df.ind$sni.lt1.rur.ind.9)/(init.rur.lt1$s.num.9)) / (sum(df.ind$si.no.rur.ind.9)/(init.rur$s.num.9)))


# ---------- LT1 VE Urban ---------- #
# ----- calculate overall ve
1-((sum(df.ind$si.lt1.urb.ind.1)/(init.urb.lt1$s.num.1+init.urb.lt1$s.v.num.1)) / (sum(df.ind$si.no.urb.ind.1)/(init.urb$s.num.1)))
1-((sum(df.ind$si.lt1.urb.ind.2)/(init.urb.lt1$s.num.2+init.urb.lt1$s.v.num.2)) / (sum(df.ind$si.no.urb.ind.2)/(init.urb$s.num.2)))
1-((sum(df.ind$si.lt1.urb.ind.3)/(init.urb.lt1$s.num.3+init.urb.lt1$s.v.num.3)) / (sum(df.ind$si.no.urb.ind.3)/(init.urb$s.num.3)))
1-((sum(df.ind$si.lt1.urb.ind.4)/(init.urb.lt1$s.num.4+init.urb.lt1$s.v.num.4)) / (sum(df.ind$si.no.urb.ind.4)/(init.urb$s.num.4)))
1-((sum(df.ind$si.lt1.urb.ind.5)/(init.urb.lt1$s.num.5+init.urb.lt1$s.v.num.5)) / (sum(df.ind$si.no.urb.ind.5)/(init.urb$s.num.5)))
1-((sum(df.ind$si.lt1.urb.ind.6)/(init.urb.lt1$s.num.6+init.urb.lt1$s.v.num.6)) / (sum(df.ind$si.no.urb.ind.6)/(init.urb$s.num.6)))
1-((sum(df.ind$si.lt1.urb.ind.7)/(init.urb.lt1$s.num.7+init.urb.lt1$s.v.num.7)) / (sum(df.ind$si.no.urb.ind.7)/(init.urb$s.num.7)))
1-((sum(df.ind$si.lt1.urb.ind.8)/(init.urb.lt1$s.num.8+init.urb.lt1$s.v.num.8)) / (sum(df.ind$si.no.urb.ind.8)/(init.urb$s.num.8)))
1-((sum(df.ind$si.lt1.urb.ind.9)/(init.urb.lt1$s.num.9+init.urb.lt1$s.v.num.9)) / (sum(df.ind$si.no.urb.ind.9)/(init.urb$s.num.9)))

# ----- calculate direct ve
1-((sum(df.ind$svi.lt1.urb.ind.1)/(init.urb.lt1$s.v.num.1)) / (sum(df.ind$sni.lt1.urb.ind.1)/(init.urb.lt1$s.num.1)))
1-((sum(df.ind$svi.lt1.urb.ind.2)/(init.urb.lt1$s.v.num.2)) / (sum(df.ind$sni.lt1.urb.ind.2)/(init.urb.lt1$s.num.2)))

# ----- calculate indirect ve
1-((sum(df.ind$sni.lt1.urb.ind.1)/(init.urb.lt1$s.num.1)) / (sum(df.ind$si.no.urb.ind.1)/(init.urb$s.num.1)))
1-((sum(df.ind$sni.lt1.urb.ind.2)/(init.urb.lt1$s.num.2)) / (sum(df.ind$si.no.urb.ind.2)/(init.urb$s.num.2)))
1-((sum(df.ind$sni.lt1.urb.ind.3)/(init.urb.lt1$s.num.3)) / (sum(df.ind$si.no.urb.ind.3)/(init.urb$s.num.3)))
1-((sum(df.ind$sni.lt1.urb.ind.4)/(init.urb.lt1$s.num.4)) / (sum(df.ind$si.no.urb.ind.4)/(init.urb$s.num.4)))
1-((sum(df.ind$sni.lt1.urb.ind.5)/(init.urb.lt1$s.num.5)) / (sum(df.ind$si.no.urb.ind.5)/(init.urb$s.num.5)))
1-((sum(df.ind$sni.lt1.urb.ind.6)/(init.urb.lt1$s.num.6)) / (sum(df.ind$si.no.urb.ind.6)/(init.urb$s.num.6)))
1-((sum(df.ind$sni.lt1.urb.ind.7)/(init.urb.lt1$s.num.7)) / (sum(df.ind$si.no.urb.ind.7)/(init.urb$s.num.7)))
1-((sum(df.ind$sni.lt1.urb.ind.8)/(init.urb.lt1$s.num.8)) / (sum(df.ind$si.no.urb.ind.8)/(init.urb$s.num.8)))
1-((sum(df.ind$sni.lt1.urb.ind.9)/(init.urb.lt1$s.num.9)) / (sum(df.ind$si.no.urb.ind.9)/(init.urb$s.num.9)))

# ---------- AYA VE Rural ---------- #
# ----- calculate overall ve
1-((sum(df.ind$si.aya.rur.ind.1)/(init.rur.aya$s.num.1+init.rur.aya$s.v.num.1)) / (sum(df.ind$si.no.rur.ind.1)/(init.rur$s.num.1)))
1-((sum(df.ind$si.aya.rur.ind.2)/(init.rur.aya$s.num.2+init.rur.aya$s.v.num.2)) / (sum(df.ind$si.no.rur.ind.2)/(init.rur$s.num.2)))
1-((sum(df.ind$si.aya.rur.ind.3)/(init.rur.aya$s.num.3+init.rur.aya$s.v.num.3)) / (sum(df.ind$si.no.rur.ind.3)/(init.rur$s.num.3)))
1-((sum(df.ind$si.aya.rur.ind.4)/(init.rur.aya$s.num.4+init.rur.aya$s.v.num.4)) / (sum(df.ind$si.no.rur.ind.4)/(init.rur$s.num.4)))
1-((sum(df.ind$si.aya.rur.ind.5)/(init.rur.aya$s.num.5+init.rur.aya$s.v.num.5)) / (sum(df.ind$si.no.rur.ind.5)/(init.rur$s.num.5)))
1-((sum(df.ind$si.aya.rur.ind.6)/(init.rur.aya$s.num.6+init.rur.aya$s.v.num.6)) / (sum(df.ind$si.no.rur.ind.6)/(init.rur$s.num.6)))
1-((sum(df.ind$si.aya.rur.ind.7)/(init.rur.aya$s.num.7+init.rur.aya$s.v.num.7)) / (sum(df.ind$si.no.rur.ind.7)/(init.rur$s.num.7)))
1-((sum(df.ind$si.aya.rur.ind.8)/(init.rur.aya$s.num.8+init.rur.aya$s.v.num.8)) / (sum(df.ind$si.no.rur.ind.8)/(init.rur$s.num.8)))
1-((sum(df.ind$si.aya.rur.ind.9)/(init.rur.aya$s.num.9+init.rur.aya$s.v.num.9)) / (sum(df.ind$si.no.rur.ind.9)/(init.rur$s.num.9)))

# ----- calculate direct ve
1-((sum(df.ind$svi.aya.rur.ind.5)/(init.rur.aya$s.v.num.5)) / (sum(df.ind$sni.aya.rur.ind.5)/(init.rur.aya$s.num.5)))
1-((sum(df.ind$svi.aya.rur.ind.6)/(init.rur.aya$s.v.num.6)) / (sum(df.ind$sni.aya.rur.ind.6)/(init.rur.aya$s.num.6)))

# ----- calculate indirect ve
1-((sum(df.ind$sni.aya.rur.ind.1)/(init.rur.aya$s.num.1)) / (sum(df.ind$si.no.rur.ind.1)/(init.rur$s.num.1)))
1-((sum(df.ind$sni.aya.rur.ind.2)/(init.rur.aya$s.num.2)) / (sum(df.ind$si.no.rur.ind.2)/(init.rur$s.num.2)))
1-((sum(df.ind$sni.aya.rur.ind.3)/(init.rur.aya$s.num.3)) / (sum(df.ind$si.no.rur.ind.3)/(init.rur$s.num.3)))
1-((sum(df.ind$sni.aya.rur.ind.4)/(init.rur.aya$s.num.4)) / (sum(df.ind$si.no.rur.ind.4)/(init.rur$s.num.4)))
1-((sum(df.ind$sni.aya.rur.ind.5)/(init.rur.aya$s.num.5)) / (sum(df.ind$si.no.rur.ind.5)/(init.rur$s.num.5)))
1-((sum(df.ind$sni.aya.rur.ind.6)/(init.rur.aya$s.num.6)) / (sum(df.ind$si.no.rur.ind.6)/(init.rur$s.num.6)))
1-((sum(df.ind$sni.aya.rur.ind.7)/(init.rur.aya$s.num.7)) / (sum(df.ind$si.no.rur.ind.7)/(init.rur$s.num.7)))
1-((sum(df.ind$sni.aya.rur.ind.8)/(init.rur.aya$s.num.8)) / (sum(df.ind$si.no.rur.ind.8)/(init.rur$s.num.8)))
1-((sum(df.ind$sni.aya.rur.ind.9)/(init.rur.aya$s.num.9)) / (sum(df.ind$si.no.rur.ind.9)/(init.rur$s.num.9)))


# ---------- AYA VE Urban ---------- #
# ----- calculate overall ve
1-((sum(df.ind$si.aya.urb.ind.1)/(init.urb.aya$s.num.1+init.urb.aya$s.v.num.1)) / (sum(df.ind$si.no.urb.ind.1)/(init.urb$s.num.1)))
1-((sum(df.ind$si.aya.urb.ind.2)/(init.urb.aya$s.num.2+init.urb.aya$s.v.num.2)) / (sum(df.ind$si.no.urb.ind.2)/(init.urb$s.num.2)))
1-((sum(df.ind$si.aya.urb.ind.3)/(init.urb.aya$s.num.3+init.urb.aya$s.v.num.3)) / (sum(df.ind$si.no.urb.ind.3)/(init.urb$s.num.3)))
1-((sum(df.ind$si.aya.urb.ind.4)/(init.urb.aya$s.num.4+init.urb.aya$s.v.num.4)) / (sum(df.ind$si.no.urb.ind.4)/(init.urb$s.num.4)))
1-((sum(df.ind$si.aya.urb.ind.5)/(init.urb.aya$s.num.5+init.urb.aya$s.v.num.5)) / (sum(df.ind$si.no.urb.ind.5)/(init.urb$s.num.5)))
1-((sum(df.ind$si.aya.urb.ind.6)/(init.urb.aya$s.num.6+init.urb.aya$s.v.num.6)) / (sum(df.ind$si.no.urb.ind.6)/(init.urb$s.num.6)))
1-((sum(df.ind$si.aya.urb.ind.7)/(init.urb.aya$s.num.7+init.urb.aya$s.v.num.7)) / (sum(df.ind$si.no.urb.ind.7)/(init.urb$s.num.7)))
1-((sum(df.ind$si.aya.urb.ind.8)/(init.urb.aya$s.num.8+init.urb.aya$s.v.num.8)) / (sum(df.ind$si.no.urb.ind.8)/(init.urb$s.num.8)))
1-((sum(df.ind$si.aya.urb.ind.9)/(init.urb.aya$s.num.9+init.urb.aya$s.v.num.9)) / (sum(df.ind$si.no.urb.ind.9)/(init.urb$s.num.9)))

# ----- calculate direct ve
1-((sum(df.ind$svi.aya.urb.ind.5)/(init.urb.aya$s.v.num.5)) / (sum(df.ind$sni.aya.urb.ind.5)/(init.urb.aya$s.num.5)))
1-((sum(df.ind$svi.aya.urb.ind.6)/(init.urb.aya$s.v.num.6)) / (sum(df.ind$sni.aya.urb.ind.6)/(init.urb.aya$s.num.6)))

# ----- calculate indirect ve
1-((sum(df.ind$sni.aya.urb.ind.1)/(init.urb.aya$s.num.1)) / (sum(df.ind$si.no.urb.ind.1)/(init.urb$s.num.1)))
1-((sum(df.ind$sni.aya.urb.ind.2)/(init.urb.aya$s.num.2)) / (sum(df.ind$si.no.urb.ind.2)/(init.urb$s.num.2)))
1-((sum(df.ind$sni.aya.urb.ind.3)/(init.urb.aya$s.num.3)) / (sum(df.ind$si.no.urb.ind.3)/(init.urb$s.num.3)))
1-((sum(df.ind$sni.aya.urb.ind.4)/(init.urb.aya$s.num.4)) / (sum(df.ind$si.no.urb.ind.4)/(init.urb$s.num.4)))
1-((sum(df.ind$sni.aya.urb.ind.5)/(init.urb.aya$s.num.5)) / (sum(df.ind$si.no.urb.ind.5)/(init.urb$s.num.5)))
1-((sum(df.ind$sni.aya.urb.ind.6)/(init.urb.aya$s.num.6)) / (sum(df.ind$si.no.urb.ind.6)/(init.urb$s.num.6)))
1-((sum(df.ind$sni.aya.urb.ind.7)/(init.urb.aya$s.num.7)) / (sum(df.ind$si.no.urb.ind.7)/(init.urb$s.num.7)))
1-((sum(df.ind$sni.aya.urb.ind.8)/(init.urb.aya$s.num.8)) / (sum(df.ind$si.no.urb.ind.8)/(init.urb$s.num.8)))
1-((sum(df.ind$sni.aya.urb.ind.9)/(init.urb.aya$s.num.9)) / (sum(df.ind$si.no.urb.ind.9)/(init.urb$s.num.9)))



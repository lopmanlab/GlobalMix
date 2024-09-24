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
                         c11=0.02, c12=0.02, c13=0.89, c14=1.72, c15=1.98, c16=3.10, c17=2.30, c18=1.38, c19=0.39,
                         c21=0.04, c22=0.00, c23=1.11, c24=2.06, c25=2.68, c26=3.17, c27=2.66, c28=1.36, c29=0.30,
                         c31=0.00, c32=0.12, c33=1.86, c34=2.62, c35=2.32, c36=2.66, c37=2.30, c38=1.24, c39=0.24,
                         c41=0.14, c42=0.06, c43=1.52, c44=5.10, c45=3.79, c46=2.49, c47=3.30, c48=1.48, c49=0.32,
                         c51=0.05, c52=0.06, c53=0.67, c54=2.02, c55=7.30, c56=3.34, c57=3.95, c58=2.90, c59=0.44,
                         c61=0.09, c62=0.15, c63=0.97, c64=1.36, c65=2.66, c66=5.69, c67=4.87, c68=3.10, c69=0.46,
                         c71=0.03, c72=0.08, c73=1.54, c74=1.81, c75=4.05, c76=4.27, c77=8.34, c78=4.81, c79=0.65,
                         c81=0.15, c82=0.12, c83=0.82, c84=1.22, c85=2.84, c86=3.64, c87=4.48, c88=4.97, c89=0.51,
                         c91=0.00, c92=0.06, c93=0.43, c94=0.71, c95=3.63, c96=3.24, c97=4.14, c98=5.96, c99=2.20)

param.urban.lt1 <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                             c11=0.02, c12=0.02, c13=0.89, c14=1.72, c15=1.98, c16=3.10, c17=2.30, c18=1.38, c19=0.39,
                             c21=0.04, c22=0.00, c23=1.11, c24=2.06, c25=2.68, c26=3.17, c27=2.66, c28=1.36, c29=0.30,
                             c31=0.00, c32=0.12, c33=1.86, c34=2.62, c35=2.32, c36=2.66, c37=2.30, c38=1.24, c39=0.24,
                             c41=0.14, c42=0.06, c43=1.52, c44=5.10, c45=3.79, c46=2.49, c47=3.30, c48=1.48, c49=0.32,
                             c51=0.05, c52=0.06, c53=0.67, c54=2.02, c55=7.30, c56=3.34, c57=3.95, c58=2.90, c59=0.44,
                             c61=0.09, c62=0.15, c63=0.97, c64=1.36, c65=2.66, c66=5.69, c67=4.87, c68=3.10, c69=0.46,
                             c71=0.03, c72=0.08, c73=1.54, c74=1.81, c75=4.05, c76=4.27, c77=8.34, c78=4.81, c79=0.65,
                             c81=0.15, c82=0.12, c83=0.82, c84=1.22, c85=2.84, c86=3.64, c87=4.48, c88=4.97, c89=0.51,
                             c91=0.00, c92=0.06, c93=0.43, c94=0.71, c95=3.63, c96=3.24, c97=4.14, c98=5.96, c99=2.20)


param.urban.aya <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
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
param.rural <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                         c11=0.01, c12=0.04, c13=1.80, c14=2.90, c15=4.82, c16=4.04, c17=3.55, c18=2.80, c19=1.16,
                         c21=0.00, c22=0.00, c23=1.91, c24=3.38, c25=5.38, c26=5.06, c27=3.13, c28=3.09, c29=1.11,
                         c31=0.18, c32=0.31, c33=4.98, c34=6.67, c35=5.93, c36=5.18, c37=5.22, c38=3.36, c39=1.51,
                         c41=0.11, c42=0.45, c43=3.37, c44=8.58, c45=8.29, c46=4.95, c47=5.18, c48=4.37, c49=1.63,
                         c51=0.13, c52=0.26, c53=1.77, c54=3.55, c55=9.88, c56=5.76, c57=3.85, c58=4.85, c59=1.15,
                         c61=0.19, c62=0.19, c63=1.97, c64=2.65, c65=4.79, c66=6.21, c67=4.24, c68=4.65, c69=1.22,
                         c71=0.16, c72=0.08, c73=2.13, c74=3.51, c75=5.43, c76=5.16, c77=6.41, c78=5.08, c79=2.02,
                         c81=0.05, c82=0.20, c83=1.68, c84=3.04, c85=6.21, c86=4.97, c87=4.29, c88=5.22, c89=1.77,
                         c91=0.22, c92=0.17, c93=2.51, c94=2.73, c95=5.11, c96=4.81, c97=4.65, c98=5.59, c99=2.57)

param.rural.lt1 <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                             c11=0.01, c12=0.04, c13=1.80, c14=2.90, c15=4.82, c16=4.04, c17=3.55, c18=2.80, c19=1.16,
                             c21=0.00, c22=0.00, c23=1.91, c24=3.38, c25=5.38, c26=5.06, c27=3.13, c28=3.09, c29=1.11,
                             c31=0.18, c32=0.31, c33=4.98, c34=6.67, c35=5.93, c36=5.18, c37=5.22, c38=3.36, c39=1.51,
                             c41=0.11, c42=0.45, c43=3.37, c44=8.58, c45=8.29, c46=4.95, c47=5.18, c48=4.37, c49=1.63,
                             c51=0.13, c52=0.26, c53=1.77, c54=3.55, c55=9.88, c56=5.76, c57=3.85, c58=4.85, c59=1.15,
                             c61=0.19, c62=0.19, c63=1.97, c64=2.65, c65=4.79, c66=6.21, c67=4.24, c68=4.65, c69=1.22,
                             c71=0.16, c72=0.08, c73=2.13, c74=3.51, c75=5.43, c76=5.16, c77=6.41, c78=5.08, c79=2.02,
                             c81=0.05, c82=0.20, c83=1.68, c84=3.04, c85=6.21, c86=4.97, c87=4.29, c88=5.22, c89=1.77,
                             c91=0.22, c92=0.17, c93=2.51, c94=2.73, c95=5.11, c96=4.81, c97=4.65, c98=5.59, c99=2.57)


param.rural.aya <-param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
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
init.urb <- init.dcm(s.num.1 = 2160000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                     s.num.2 = 1990000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                     s.num.3 = 11230000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                     s.num.4 = 12960000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                     s.num.5 = 19010000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                     s.num.6 = 15550000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                     s.num.7 = 10370000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                     s.num.8 = 8640000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                     s.num.9 = 5620000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.urb.lt1 <- init.dcm(s.num.1 = 2160000*(1-0.50), s.v.num.1 = 2160000*0.50, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 1990000*(1-0.50), s.v.num.2 = 1990000*0.50, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 11230000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 12960000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 19010000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 15550000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 10370000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 8640000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 5620000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.urb.aya <- init.dcm(s.num.1 = 2160000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 1990000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 11230000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 12960000,, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 19010000*(1-0.50), s.v.num.5 = 19010000*0.50, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 15550000*(1-0.50), s.v.num.6 = 15550000*0.50, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 10370000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 8640000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 5620000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)
# - Rural
init.rur <- init.dcm(s.num.1 = 3840000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                     s.num.2 = 3530000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                     s.num.3 = 19970000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                     s.num.4 = 23040000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                     s.num.5 = 33790000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                     s.num.6 = 27650000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                     s.num.7 = 18430000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                     s.num.8 = 15360000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                     s.num.9 = 9980000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.rur.lt1 <- init.dcm(s.num.1 = 3840000*(1-0.50), s.v.num.1 = 3840000*0.50, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 3530000*(1-0.50), s.v.num.2 = 3530000*0.50, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 19970000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 23040000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 33790000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 27650000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 18430000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 15360000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 9980000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                         sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                         sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.rur.aya <- init.dcm(s.num.1 = 3840000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0, 
                         s.num.2 = 3530000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0, 
                         s.num.3 = 19970000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0, 
                         s.num.4 = 23040000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0, 
                         s.num.5 = 33790000*(1-0.50), s.v.num.5 = 33790000*0.50, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0, 
                         s.num.6 = 27650000*(1-0.50), s.v.num.6 = 27650000*0.50, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0, 
                         s.num.7 = 18430000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0, 
                         s.num.8 = 15360000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0, 
                         s.num.9 = 9980000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0, 
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




# --- Make a dataframe with all Pakistan results
df.pak <- matrix(ncol=0, nrow=730)
df.pak <- as.data.frame(df.pak)

# -- Urban
# - No vaccine
df.pak$si.no.urb.pak.1 <- df.no.urb$si.flow.1
df.pak$si.no.urb.pak.2 <- df.no.urb$si.flow.2
df.pak$si.no.urb.pak.3 <- df.no.urb$si.flow.3
df.pak$si.no.urb.pak.4 <- df.no.urb$si.flow.4
df.pak$si.no.urb.pak.5 <- df.no.urb$si.flow.5
df.pak$si.no.urb.pak.6 <- df.no.urb$si.flow.6
df.pak$si.no.urb.pak.7 <- df.no.urb$si.flow.7
df.pak$si.no.urb.pak.8 <- df.no.urb$si.flow.8
df.pak$si.no.urb.pak.9 <- df.no.urb$si.flow.9

# Vaccine less than 1
df.pak$si.lt1.urb.pak.1 <- df.lt1.urb$si.flow.1
df.pak$si.lt1.urb.pak.2 <- df.lt1.urb$si.flow.2
df.pak$si.lt1.urb.pak.3 <- df.lt1.urb$si.flow.3
df.pak$si.lt1.urb.pak.4 <- df.lt1.urb$si.flow.4
df.pak$si.lt1.urb.pak.5 <- df.lt1.urb$si.flow.5
df.pak$si.lt1.urb.pak.6 <- df.lt1.urb$si.flow.6
df.pak$si.lt1.urb.pak.7 <- df.lt1.urb$si.flow.7
df.pak$si.lt1.urb.pak.8 <- df.lt1.urb$si.flow.8
df.pak$si.lt1.urb.pak.9 <- df.lt1.urb$si.flow.9
df.pak$svi.lt1.urb.pak.1 <- df.lt1.urb$svi.flow.1
df.pak$svi.lt1.urb.pak.2 <- df.lt1.urb$svi.flow.2
df.pak$svi.lt1.urb.pak.3 <- df.lt1.urb$svi.flow.3
df.pak$svi.lt1.urb.pak.4 <- df.lt1.urb$svi.flow.4
df.pak$svi.lt1.urb.pak.5 <- df.lt1.urb$svi.flow.5
df.pak$svi.lt1.urb.pak.6 <- df.lt1.urb$svi.flow.6
df.pak$svi.lt1.urb.pak.7 <- df.lt1.urb$svi.flow.7
df.pak$svi.lt1.urb.pak.8 <- df.lt1.urb$svi.flow.8
df.pak$svi.lt1.urb.pak.9 <- df.lt1.urb$svi.flow.9
df.pak$sni.lt1.urb.pak.1 <- df.lt1.urb$sni.flow.1
df.pak$sni.lt1.urb.pak.2 <- df.lt1.urb$sni.flow.2
df.pak$sni.lt1.urb.pak.3 <- df.lt1.urb$sni.flow.3
df.pak$sni.lt1.urb.pak.4 <- df.lt1.urb$sni.flow.4
df.pak$sni.lt1.urb.pak.5 <- df.lt1.urb$sni.flow.5
df.pak$sni.lt1.urb.pak.6 <- df.lt1.urb$sni.flow.6
df.pak$sni.lt1.urb.pak.7 <- df.lt1.urb$sni.flow.7
df.pak$sni.lt1.urb.pak.8 <- df.lt1.urb$sni.flow.8
df.pak$sni.lt1.urb.pak.9 <- df.lt1.urb$sni.flow.9

# Vaccine in 10-29 y/o (adolescents and young adults)
df.pak$si.aya.urb.pak.1 <- df.aya.urb$si.flow.1
df.pak$si.aya.urb.pak.2 <- df.aya.urb$si.flow.2
df.pak$si.aya.urb.pak.3 <- df.aya.urb$si.flow.3
df.pak$si.aya.urb.pak.4 <- df.aya.urb$si.flow.4
df.pak$si.aya.urb.pak.5 <- df.aya.urb$si.flow.5
df.pak$si.aya.urb.pak.6 <- df.aya.urb$si.flow.6
df.pak$si.aya.urb.pak.7 <- df.aya.urb$si.flow.7
df.pak$si.aya.urb.pak.8 <- df.aya.urb$si.flow.8
df.pak$si.aya.urb.pak.9 <- df.aya.urb$si.flow.9
df.pak$svi.aya.urb.pak.1 <- df.aya.urb$svi.flow.1
df.pak$svi.aya.urb.pak.2 <- df.aya.urb$svi.flow.2
df.pak$svi.aya.urb.pak.3 <- df.aya.urb$svi.flow.3
df.pak$svi.aya.urb.pak.4 <- df.aya.urb$svi.flow.4
df.pak$svi.aya.urb.pak.5 <- df.aya.urb$svi.flow.5
df.pak$svi.aya.urb.pak.6 <- df.aya.urb$svi.flow.6
df.pak$svi.aya.urb.pak.7 <- df.aya.urb$svi.flow.7
df.pak$svi.aya.urb.pak.8 <- df.aya.urb$svi.flow.8
df.pak$svi.aya.urb.pak.9 <- df.aya.urb$svi.flow.9
df.pak$sni.aya.urb.pak.1 <- df.aya.urb$sni.flow.1
df.pak$sni.aya.urb.pak.2 <- df.aya.urb$sni.flow.2
df.pak$sni.aya.urb.pak.3 <- df.aya.urb$sni.flow.3
df.pak$sni.aya.urb.pak.4 <- df.aya.urb$sni.flow.4
df.pak$sni.aya.urb.pak.5 <- df.aya.urb$sni.flow.5
df.pak$sni.aya.urb.pak.6 <- df.aya.urb$sni.flow.6
df.pak$sni.aya.urb.pak.7 <- df.aya.urb$sni.flow.7
df.pak$sni.aya.urb.pak.8 <- df.aya.urb$sni.flow.8
df.pak$sni.aya.urb.pak.9 <- df.aya.urb$sni.flow.9


# -- Rural
# - No vaccine
df.pak$si.no.rur.pak.1 <- df.no.rur$si.flow.1
df.pak$si.no.rur.pak.2 <- df.no.rur$si.flow.2
df.pak$si.no.rur.pak.3 <- df.no.rur$si.flow.3
df.pak$si.no.rur.pak.4 <- df.no.rur$si.flow.4
df.pak$si.no.rur.pak.5 <- df.no.rur$si.flow.5
df.pak$si.no.rur.pak.6 <- df.no.rur$si.flow.6
df.pak$si.no.rur.pak.7 <- df.no.rur$si.flow.7
df.pak$si.no.rur.pak.8 <- df.no.rur$si.flow.8
df.pak$si.no.rur.pak.9 <- df.no.rur$si.flow.9

# Vaccine less than 1
df.pak$si.lt1.rur.pak.1 <- df.lt1.rur$si.flow.1
df.pak$si.lt1.rur.pak.2 <- df.lt1.rur$si.flow.2
df.pak$si.lt1.rur.pak.3 <- df.lt1.rur$si.flow.3
df.pak$si.lt1.rur.pak.4 <- df.lt1.rur$si.flow.4
df.pak$si.lt1.rur.pak.5 <- df.lt1.rur$si.flow.5
df.pak$si.lt1.rur.pak.6 <- df.lt1.rur$si.flow.6
df.pak$si.lt1.rur.pak.7 <- df.lt1.rur$si.flow.7
df.pak$si.lt1.rur.pak.8 <- df.lt1.rur$si.flow.8
df.pak$si.lt1.rur.pak.9 <- df.lt1.rur$si.flow.9
df.pak$svi.lt1.rur.pak.1 <- df.lt1.rur$svi.flow.1
df.pak$svi.lt1.rur.pak.2 <- df.lt1.rur$svi.flow.2
df.pak$svi.lt1.rur.pak.3 <- df.lt1.rur$svi.flow.3
df.pak$svi.lt1.rur.pak.4 <- df.lt1.rur$svi.flow.4
df.pak$svi.lt1.rur.pak.5 <- df.lt1.rur$svi.flow.5
df.pak$svi.lt1.rur.pak.6 <- df.lt1.rur$svi.flow.6
df.pak$svi.lt1.rur.pak.7 <- df.lt1.rur$svi.flow.7
df.pak$svi.lt1.rur.pak.8 <- df.lt1.rur$svi.flow.8
df.pak$svi.lt1.rur.pak.9 <- df.lt1.rur$svi.flow.9
df.pak$sni.lt1.rur.pak.1 <- df.lt1.rur$sni.flow.1
df.pak$sni.lt1.rur.pak.2 <- df.lt1.rur$sni.flow.2
df.pak$sni.lt1.rur.pak.3 <- df.lt1.rur$sni.flow.3
df.pak$sni.lt1.rur.pak.4 <- df.lt1.rur$sni.flow.4
df.pak$sni.lt1.rur.pak.5 <- df.lt1.rur$sni.flow.5
df.pak$sni.lt1.rur.pak.6 <- df.lt1.rur$sni.flow.6
df.pak$sni.lt1.rur.pak.7 <- df.lt1.rur$sni.flow.7
df.pak$sni.lt1.rur.pak.8 <- df.lt1.rur$sni.flow.8
df.pak$sni.lt1.rur.pak.9 <- df.lt1.rur$sni.flow.9

# Vaccine in 10-29 y/o (adolescents and young adults)
df.pak$si.aya.rur.pak.1 <- df.aya.rur$si.flow.1
df.pak$si.aya.rur.pak.2 <- df.aya.rur$si.flow.2
df.pak$si.aya.rur.pak.3 <- df.aya.rur$si.flow.3
df.pak$si.aya.rur.pak.4 <- df.aya.rur$si.flow.4
df.pak$si.aya.rur.pak.5 <- df.aya.rur$si.flow.5
df.pak$si.aya.rur.pak.6 <- df.aya.rur$si.flow.6
df.pak$si.aya.rur.pak.7 <- df.aya.rur$si.flow.7
df.pak$si.aya.rur.pak.8 <- df.aya.rur$si.flow.8
df.pak$si.aya.rur.pak.9 <- df.aya.rur$si.flow.9
df.pak$svi.aya.rur.pak.1 <- df.aya.rur$svi.flow.1
df.pak$svi.aya.rur.pak.2 <- df.aya.rur$svi.flow.2
df.pak$svi.aya.rur.pak.3 <- df.aya.rur$svi.flow.3
df.pak$svi.aya.rur.pak.4 <- df.aya.rur$svi.flow.4
df.pak$svi.aya.rur.pak.5 <- df.aya.rur$svi.flow.5
df.pak$svi.aya.rur.pak.6 <- df.aya.rur$svi.flow.6
df.pak$svi.aya.rur.pak.7 <- df.aya.rur$svi.flow.7
df.pak$svi.aya.rur.pak.8 <- df.aya.rur$svi.flow.8
df.pak$svi.aya.rur.pak.9 <- df.aya.rur$svi.flow.9
df.pak$sni.aya.rur.pak.1 <- df.aya.rur$sni.flow.1
df.pak$sni.aya.rur.pak.2 <- df.aya.rur$sni.flow.2
df.pak$sni.aya.rur.pak.3 <- df.aya.rur$sni.flow.3
df.pak$sni.aya.rur.pak.4 <- df.aya.rur$sni.flow.4
df.pak$sni.aya.rur.pak.5 <- df.aya.rur$sni.flow.5
df.pak$sni.aya.rur.pak.6 <- df.aya.rur$sni.flow.6
df.pak$sni.aya.rur.pak.7 <- df.aya.rur$sni.flow.7
df.pak$sni.aya.rur.pak.8 <- df.aya.rur$sni.flow.8
df.pak$sni.aya.rur.pak.9 <- df.aya.rur$sni.flow.9


# -- Time
df.pak$time <- df.aya.rur$time




# ---------- LT1 VE Rural ---------- #
# ----- calculate overall ve
1-((sum(df.pak$si.lt1.rur.pak.1)/(init.rur.lt1$s.num.1+init.rur.lt1$s.v.num.1)) / (sum(df.pak$si.no.rur.pak.1)/(init.rur$s.num.1)))
1-((sum(df.pak$si.lt1.rur.pak.2)/(init.rur.lt1$s.num.2+init.rur.lt1$s.v.num.2)) / (sum(df.pak$si.no.rur.pak.2)/(init.rur$s.num.2)))
1-((sum(df.pak$si.lt1.rur.pak.3)/(init.rur.lt1$s.num.3+init.rur.lt1$s.v.num.3)) / (sum(df.pak$si.no.rur.pak.3)/(init.rur$s.num.3)))
1-((sum(df.pak$si.lt1.rur.pak.4)/(init.rur.lt1$s.num.4+init.rur.lt1$s.v.num.4)) / (sum(df.pak$si.no.rur.pak.4)/(init.rur$s.num.4)))
1-((sum(df.pak$si.lt1.rur.pak.5)/(init.rur.lt1$s.num.5+init.rur.lt1$s.v.num.5)) / (sum(df.pak$si.no.rur.pak.5)/(init.rur$s.num.5)))
1-((sum(df.pak$si.lt1.rur.pak.6)/(init.rur.lt1$s.num.6+init.rur.lt1$s.v.num.6)) / (sum(df.pak$si.no.rur.pak.6)/(init.rur$s.num.6)))
1-((sum(df.pak$si.lt1.rur.pak.7)/(init.rur.lt1$s.num.7+init.rur.lt1$s.v.num.7)) / (sum(df.pak$si.no.rur.pak.7)/(init.rur$s.num.7)))
1-((sum(df.pak$si.lt1.rur.pak.8)/(init.rur.lt1$s.num.8+init.rur.lt1$s.v.num.8)) / (sum(df.pak$si.no.rur.pak.8)/(init.rur$s.num.8)))
1-((sum(df.pak$si.lt1.rur.pak.9)/(init.rur.lt1$s.num.9+init.rur.lt1$s.v.num.9)) / (sum(df.pak$si.no.rur.pak.9)/(init.rur$s.num.9)))

# ----- calculate direct ve
1-((sum(df.pak$svi.lt1.rur.pak.1)/(init.rur.lt1$s.v.num.1)) / (sum(df.pak$sni.lt1.rur.pak.1)/(init.rur.lt1$s.num.1)))
1-((sum(df.pak$svi.lt1.rur.pak.2)/(init.rur.lt1$s.v.num.2)) / (sum(df.pak$sni.lt1.rur.pak.2)/(init.rur.lt1$s.num.2)))

# ----- calculate pakirect ve
1-((sum(df.pak$sni.lt1.rur.pak.1)/(init.rur.lt1$s.num.1)) / (sum(df.pak$si.no.rur.pak.1)/(init.rur$s.num.1)))
1-((sum(df.pak$sni.lt1.rur.pak.2)/(init.rur.lt1$s.num.2)) / (sum(df.pak$si.no.rur.pak.2)/(init.rur$s.num.2)))
1-((sum(df.pak$sni.lt1.rur.pak.3)/(init.rur.lt1$s.num.3)) / (sum(df.pak$si.no.rur.pak.3)/(init.rur$s.num.3)))
1-((sum(df.pak$sni.lt1.rur.pak.4)/(init.rur.lt1$s.num.4)) / (sum(df.pak$si.no.rur.pak.4)/(init.rur$s.num.4)))
1-((sum(df.pak$sni.lt1.rur.pak.5)/(init.rur.lt1$s.num.5)) / (sum(df.pak$si.no.rur.pak.5)/(init.rur$s.num.5)))
1-((sum(df.pak$sni.lt1.rur.pak.6)/(init.rur.lt1$s.num.6)) / (sum(df.pak$si.no.rur.pak.6)/(init.rur$s.num.6)))
1-((sum(df.pak$sni.lt1.rur.pak.7)/(init.rur.lt1$s.num.7)) / (sum(df.pak$si.no.rur.pak.7)/(init.rur$s.num.7)))
1-((sum(df.pak$sni.lt1.rur.pak.8)/(init.rur.lt1$s.num.8)) / (sum(df.pak$si.no.rur.pak.8)/(init.rur$s.num.8)))
1-((sum(df.pak$sni.lt1.rur.pak.9)/(init.rur.lt1$s.num.9)) / (sum(df.pak$si.no.rur.pak.9)/(init.rur$s.num.9)))


# ---------- LT1 VE Urban ---------- #
# ----- calculate overall ve
1-((sum(df.pak$si.lt1.urb.pak.1)/(init.urb.lt1$s.num.1+init.urb.lt1$s.v.num.1)) / (sum(df.pak$si.no.urb.pak.1)/(init.urb$s.num.1)))
1-((sum(df.pak$si.lt1.urb.pak.2)/(init.urb.lt1$s.num.2+init.urb.lt1$s.v.num.2)) / (sum(df.pak$si.no.urb.pak.2)/(init.urb$s.num.2)))
1-((sum(df.pak$si.lt1.urb.pak.3)/(init.urb.lt1$s.num.3+init.urb.lt1$s.v.num.3)) / (sum(df.pak$si.no.urb.pak.3)/(init.urb$s.num.3)))
1-((sum(df.pak$si.lt1.urb.pak.4)/(init.urb.lt1$s.num.4+init.urb.lt1$s.v.num.4)) / (sum(df.pak$si.no.urb.pak.4)/(init.urb$s.num.4)))
1-((sum(df.pak$si.lt1.urb.pak.5)/(init.urb.lt1$s.num.5+init.urb.lt1$s.v.num.5)) / (sum(df.pak$si.no.urb.pak.5)/(init.urb$s.num.5)))
1-((sum(df.pak$si.lt1.urb.pak.6)/(init.urb.lt1$s.num.6+init.urb.lt1$s.v.num.6)) / (sum(df.pak$si.no.urb.pak.6)/(init.urb$s.num.6)))
1-((sum(df.pak$si.lt1.urb.pak.7)/(init.urb.lt1$s.num.7+init.urb.lt1$s.v.num.7)) / (sum(df.pak$si.no.urb.pak.7)/(init.urb$s.num.7)))
1-((sum(df.pak$si.lt1.urb.pak.8)/(init.urb.lt1$s.num.8+init.urb.lt1$s.v.num.8)) / (sum(df.pak$si.no.urb.pak.8)/(init.urb$s.num.8)))
1-((sum(df.pak$si.lt1.urb.pak.9)/(init.urb.lt1$s.num.9+init.urb.lt1$s.v.num.9)) / (sum(df.pak$si.no.urb.pak.9)/(init.urb$s.num.9)))

# ----- calculate direct ve
1-((sum(df.pak$svi.lt1.urb.pak.1)/(init.urb.lt1$s.v.num.1)) / (sum(df.pak$sni.lt1.urb.pak.1)/(init.urb.lt1$s.num.1)))
1-((sum(df.pak$svi.lt1.urb.pak.2)/(init.urb.lt1$s.v.num.2)) / (sum(df.pak$sni.lt1.urb.pak.2)/(init.urb.lt1$s.num.2)))

# ----- calculate pakirect ve
1-((sum(df.pak$sni.lt1.urb.pak.1)/(init.urb.lt1$s.num.1)) / (sum(df.pak$si.no.urb.pak.1)/(init.urb$s.num.1)))
1-((sum(df.pak$sni.lt1.urb.pak.2)/(init.urb.lt1$s.num.2)) / (sum(df.pak$si.no.urb.pak.2)/(init.urb$s.num.2)))
1-((sum(df.pak$sni.lt1.urb.pak.3)/(init.urb.lt1$s.num.3)) / (sum(df.pak$si.no.urb.pak.3)/(init.urb$s.num.3)))
1-((sum(df.pak$sni.lt1.urb.pak.4)/(init.urb.lt1$s.num.4)) / (sum(df.pak$si.no.urb.pak.4)/(init.urb$s.num.4)))
1-((sum(df.pak$sni.lt1.urb.pak.5)/(init.urb.lt1$s.num.5)) / (sum(df.pak$si.no.urb.pak.5)/(init.urb$s.num.5)))
1-((sum(df.pak$sni.lt1.urb.pak.6)/(init.urb.lt1$s.num.6)) / (sum(df.pak$si.no.urb.pak.6)/(init.urb$s.num.6)))
1-((sum(df.pak$sni.lt1.urb.pak.7)/(init.urb.lt1$s.num.7)) / (sum(df.pak$si.no.urb.pak.7)/(init.urb$s.num.7)))
1-((sum(df.pak$sni.lt1.urb.pak.8)/(init.urb.lt1$s.num.8)) / (sum(df.pak$si.no.urb.pak.8)/(init.urb$s.num.8)))
1-((sum(df.pak$sni.lt1.urb.pak.9)/(init.urb.lt1$s.num.9)) / (sum(df.pak$si.no.urb.pak.9)/(init.urb$s.num.9)))

# ---------- AYA VE Rural ---------- #
# ----- calculate overall ve
1-((sum(df.pak$si.aya.rur.pak.1)/(init.rur.aya$s.num.1+init.rur.aya$s.v.num.1)) / (sum(df.pak$si.no.rur.pak.1)/(init.rur$s.num.1)))
1-((sum(df.pak$si.aya.rur.pak.2)/(init.rur.aya$s.num.2+init.rur.aya$s.v.num.2)) / (sum(df.pak$si.no.rur.pak.2)/(init.rur$s.num.2)))
1-((sum(df.pak$si.aya.rur.pak.3)/(init.rur.aya$s.num.3+init.rur.aya$s.v.num.3)) / (sum(df.pak$si.no.rur.pak.3)/(init.rur$s.num.3)))
1-((sum(df.pak$si.aya.rur.pak.4)/(init.rur.aya$s.num.4+init.rur.aya$s.v.num.4)) / (sum(df.pak$si.no.rur.pak.4)/(init.rur$s.num.4)))
1-((sum(df.pak$si.aya.rur.pak.5)/(init.rur.aya$s.num.5+init.rur.aya$s.v.num.5)) / (sum(df.pak$si.no.rur.pak.5)/(init.rur$s.num.5)))
1-((sum(df.pak$si.aya.rur.pak.6)/(init.rur.aya$s.num.6+init.rur.aya$s.v.num.6)) / (sum(df.pak$si.no.rur.pak.6)/(init.rur$s.num.6)))
1-((sum(df.pak$si.aya.rur.pak.7)/(init.rur.aya$s.num.7+init.rur.aya$s.v.num.7)) / (sum(df.pak$si.no.rur.pak.7)/(init.rur$s.num.7)))
1-((sum(df.pak$si.aya.rur.pak.8)/(init.rur.aya$s.num.8+init.rur.aya$s.v.num.8)) / (sum(df.pak$si.no.rur.pak.8)/(init.rur$s.num.8)))
1-((sum(df.pak$si.aya.rur.pak.9)/(init.rur.aya$s.num.9+init.rur.aya$s.v.num.9)) / (sum(df.pak$si.no.rur.pak.9)/(init.rur$s.num.9)))

# ----- calculate direct ve
1-((sum(df.pak$svi.aya.rur.pak.5)/(init.rur.aya$s.v.num.5)) / (sum(df.pak$sni.aya.rur.pak.5)/(init.rur.aya$s.num.5)))
1-((sum(df.pak$svi.aya.rur.pak.6)/(init.rur.aya$s.v.num.6)) / (sum(df.pak$sni.aya.rur.pak.6)/(init.rur.aya$s.num.6)))

# ----- calculate pakirect ve
1-((sum(df.pak$sni.aya.rur.pak.1)/(init.rur.aya$s.num.1)) / (sum(df.pak$si.no.rur.pak.1)/(init.rur$s.num.1)))
1-((sum(df.pak$sni.aya.rur.pak.2)/(init.rur.aya$s.num.2)) / (sum(df.pak$si.no.rur.pak.2)/(init.rur$s.num.2)))
1-((sum(df.pak$sni.aya.rur.pak.3)/(init.rur.aya$s.num.3)) / (sum(df.pak$si.no.rur.pak.3)/(init.rur$s.num.3)))
1-((sum(df.pak$sni.aya.rur.pak.4)/(init.rur.aya$s.num.4)) / (sum(df.pak$si.no.rur.pak.4)/(init.rur$s.num.4)))
1-((sum(df.pak$sni.aya.rur.pak.5)/(init.rur.aya$s.num.5)) / (sum(df.pak$si.no.rur.pak.5)/(init.rur$s.num.5)))
1-((sum(df.pak$sni.aya.rur.pak.6)/(init.rur.aya$s.num.6)) / (sum(df.pak$si.no.rur.pak.6)/(init.rur$s.num.6)))
1-((sum(df.pak$sni.aya.rur.pak.7)/(init.rur.aya$s.num.7)) / (sum(df.pak$si.no.rur.pak.7)/(init.rur$s.num.7)))
1-((sum(df.pak$sni.aya.rur.pak.8)/(init.rur.aya$s.num.8)) / (sum(df.pak$si.no.rur.pak.8)/(init.rur$s.num.8)))
1-((sum(df.pak$sni.aya.rur.pak.9)/(init.rur.aya$s.num.9)) / (sum(df.pak$si.no.rur.pak.9)/(init.rur$s.num.9)))


# ---------- AYA VE Urban ---------- #
# ----- calculate overall ve
1-((sum(df.pak$si.aya.urb.pak.1)/(init.urb.aya$s.num.1+init.urb.aya$s.v.num.1)) / (sum(df.pak$si.no.urb.pak.1)/(init.urb$s.num.1)))
1-((sum(df.pak$si.aya.urb.pak.2)/(init.urb.aya$s.num.2+init.urb.aya$s.v.num.2)) / (sum(df.pak$si.no.urb.pak.2)/(init.urb$s.num.2)))
1-((sum(df.pak$si.aya.urb.pak.3)/(init.urb.aya$s.num.3+init.urb.aya$s.v.num.3)) / (sum(df.pak$si.no.urb.pak.3)/(init.urb$s.num.3)))
1-((sum(df.pak$si.aya.urb.pak.4)/(init.urb.aya$s.num.4+init.urb.aya$s.v.num.4)) / (sum(df.pak$si.no.urb.pak.4)/(init.urb$s.num.4)))
1-((sum(df.pak$si.aya.urb.pak.5)/(init.urb.aya$s.num.5+init.urb.aya$s.v.num.5)) / (sum(df.pak$si.no.urb.pak.5)/(init.urb$s.num.5)))
1-((sum(df.pak$si.aya.urb.pak.6)/(init.urb.aya$s.num.6+init.urb.aya$s.v.num.6)) / (sum(df.pak$si.no.urb.pak.6)/(init.urb$s.num.6)))
1-((sum(df.pak$si.aya.urb.pak.7)/(init.urb.aya$s.num.7+init.urb.aya$s.v.num.7)) / (sum(df.pak$si.no.urb.pak.7)/(init.urb$s.num.7)))
1-((sum(df.pak$si.aya.urb.pak.8)/(init.urb.aya$s.num.8+init.urb.aya$s.v.num.8)) / (sum(df.pak$si.no.urb.pak.8)/(init.urb$s.num.8)))
1-((sum(df.pak$si.aya.urb.pak.9)/(init.urb.aya$s.num.9+init.urb.aya$s.v.num.9)) / (sum(df.pak$si.no.urb.pak.9)/(init.urb$s.num.9)))

# ----- calculate direct ve
1-((sum(df.pak$svi.aya.urb.pak.5)/(init.urb.aya$s.v.num.5)) / (sum(df.pak$sni.aya.urb.pak.5)/(init.urb.aya$s.num.5)))
1-((sum(df.pak$svi.aya.urb.pak.6)/(init.urb.aya$s.v.num.6)) / (sum(df.pak$sni.aya.urb.pak.6)/(init.urb.aya$s.num.6)))

# ----- calculate pakirect ve
1-((sum(df.pak$sni.aya.urb.pak.1)/(init.urb.aya$s.num.1)) / (sum(df.pak$si.no.urb.pak.1)/(init.urb$s.num.1)))
1-((sum(df.pak$sni.aya.urb.pak.2)/(init.urb.aya$s.num.2)) / (sum(df.pak$si.no.urb.pak.2)/(init.urb$s.num.2)))
1-((sum(df.pak$sni.aya.urb.pak.3)/(init.urb.aya$s.num.3)) / (sum(df.pak$si.no.urb.pak.3)/(init.urb$s.num.3)))
1-((sum(df.pak$sni.aya.urb.pak.4)/(init.urb.aya$s.num.4)) / (sum(df.pak$si.no.urb.pak.4)/(init.urb$s.num.4)))
1-((sum(df.pak$sni.aya.urb.pak.5)/(init.urb.aya$s.num.5)) / (sum(df.pak$si.no.urb.pak.5)/(init.urb$s.num.5)))
1-((sum(df.pak$sni.aya.urb.pak.6)/(init.urb.aya$s.num.6)) / (sum(df.pak$si.no.urb.pak.6)/(init.urb$s.num.6)))
1-((sum(df.pak$sni.aya.urb.pak.7)/(init.urb.aya$s.num.7)) / (sum(df.pak$si.no.urb.pak.7)/(init.urb$s.num.7)))
1-((sum(df.pak$sni.aya.urb.pak.8)/(init.urb.aya$s.num.8)) / (sum(df.pak$si.no.urb.pak.8)/(init.urb$s.num.8)))
1-((sum(df.pak$sni.aya.urb.pak.9)/(init.urb.aya$s.num.9)) / (sum(df.pak$si.no.urb.pak.9)/(init.urb$s.num.9)))





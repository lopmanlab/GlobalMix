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
                         # gamma: duration of illness; psi: vaccine effectiveness; q: transmission probability; mu: demographics
                         c11=0.02, c12=0.05, c13=0.38, c14=0.80, c15=1.39, c16=1.30, c17=1.09, c18=0.48, c19=0.33,
                         c21=0.02, c22=0.08, c23=0.97, c24=1.22, c25=1.88, c26=1.95, c27=1.52, c28=0.80, c29=0.20,
                         c31=0.00, c32=0.01, c33=2.36, c34=2.38, c35=1.82, c36=1.99, c37=0.96, c38=0.76, c39=0.25,
                         c41=0.00, c42=0.03, c43=1.03, c44=4.09, c45=2.14, c46=1.55, c47=0.84, c48=0.34, c49=0.12,
                         c51=0.00, c52=0.03, c53=0.61, c54=1.43, c55=7.93, c56=2.16, c57=1.11, c58=0.78, c59=0.19,
                         c61=0.03, c62=0.03, c63=0.61, c64=0.93, c65=1.64, c66=3.66, c67=1.79, c68=1.05, c69=0.21,
                         c71=0.07, c72=0.13, c73=0.72, c74=0.72, c75=1.67, c76=2.25, c77=3.16, c78=1.52, c79=0.16,
                         c81=0.01, c82=0.07, c83=0.34, c84=0.68, c85=1.36, c86=1.99, c87=1.93, c88=2.60, c89=0.40,
                         c91=0.02, c92=0.07, c93=0.30, c94=0.72, c95=0.95, c96=1.46, c97=1.31, c98=1.85, c99=1.20)

param.urban.lt1 <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02,
                         # gamma: duration of illness; psi: vaccine effectiveness; q: transmission probability; mu: demographics
                         c11=0.02, c12=0.05, c13=0.38, c14=0.80, c15=1.39, c16=1.30, c17=1.09, c18=0.48, c19=0.33,
                         c21=0.02, c22=0.08, c23=0.97, c24=1.22, c25=1.88, c26=1.95, c27=1.52, c28=0.80, c29=0.20,
                         c31=0.00, c32=0.01, c33=2.36, c34=2.38, c35=1.82, c36=1.99, c37=0.96, c38=0.76, c39=0.25,
                         c41=0.00, c42=0.03, c43=1.03, c44=4.09, c45=2.14, c46=1.55, c47=0.84, c48=0.34, c49=0.12,
                         c51=0.00, c52=0.03, c53=0.61, c54=1.43, c55=7.93, c56=2.16, c57=1.11, c58=0.78, c59=0.19,
                         c61=0.03, c62=0.03, c63=0.61, c64=0.93, c65=1.64, c66=3.66, c67=1.79, c68=1.05, c69=0.21,
                         c71=0.07, c72=0.13, c73=0.72, c74=0.72, c75=1.67, c76=2.25, c77=3.16, c78=1.52, c79=0.16,
                         c81=0.01, c82=0.07, c83=0.34, c84=0.68, c85=1.36, c86=1.99, c87=1.93, c88=2.60, c89=0.40,
                         c91=0.02, c92=0.07, c93=0.30, c94=0.72, c95=0.95, c96=1.46, c97=1.31, c98=1.85, c99=1.20)

param.urban.aya <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                         # gamma: duration of illness; psi: vaccine effectiveness; q: transmission probability; mu: demographics
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
param.rural <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02,
                         # gamma: duration of illness; psi: vaccine effectiveness; q: transmission probability; mu: demographics
                         c11=0.00, c12=0.03, c13=1.23, c14=2.31, c15=3.55, c16=2.18, c17=1.29, c18=1.31, c19=0.52,
                         c21=0.01, c22=0.05, c23=1.82, c24=2.88, c25=4.10, c26=2.35, c27=1.34, c28=1.35, c29=0.38,
                         c31=0.02, c32=0.19, c33=2.46, c34=3.76, c35=3.87, c36=1.73, c37=1.13, c38=0.92, c39=0.27,
                         c41=0.12, c42=0.19, c43=2.25, c44=7.11, c45=4.47, c46=0.91, c47=0.66, c48=0.53, c49=0.36,
                         c51=0.04, c52=0.08, c53=0.83, c54=2.38, c55=10.98, c56=2.04, c57=1.34, c58=0.94, c59=0.42,
                         c61=0.06, c62=0.05, c63=0.83, c64=1.22, c65=4.81, c66=5.61, c67=2.28, c68=2.17, c69=0.47,
                         c71=0.05, c72=0.02, c73=0.95, c74=1.48, c75=2.33, c76=3.31, c77=3.45, c78=3.22, c79=0.83,
                         c81=0.02, c82=0.07, c83=0.65, c84=1.16, c85=2.99, c86=3.16, c87=3.00, c88=4.64, c89=1.15,
                         c91=0.05, c92=0.03, c93=0.60, c94=0.79, c95=1.60, c96=1.65, c97=1.59, c98=3.30, c99=1.59)

param.rural.lt1 <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02,
                         # gamma: duration of illness; psi: vaccine effectiveness; q: transmission probability; mu: demographics
                         c11=0.00, c12=0.03, c13=1.23, c14=2.31, c15=3.55, c16=2.18, c17=1.29, c18=1.31, c19=0.52,
                         c21=0.01, c22=0.05, c23=1.82, c24=2.88, c25=4.10, c26=2.35, c27=1.34, c28=1.35, c29=0.38,
                         c31=0.02, c32=0.19, c33=2.46, c34=3.76, c35=3.87, c36=1.73, c37=1.13, c38=0.92, c39=0.27,
                         c41=0.12, c42=0.19, c43=2.25, c44=7.11, c45=4.47, c46=0.91, c47=0.66, c48=0.53, c49=0.36,
                         c51=0.04, c52=0.08, c53=0.83, c54=2.38, c55=10.98, c56=2.04, c57=1.34, c58=0.94, c59=0.42,
                         c61=0.06, c62=0.05, c63=0.83, c64=1.22, c65=4.81, c66=5.61, c67=2.28, c68=2.17, c69=0.47,
                         c71=0.05, c72=0.02, c73=0.95, c74=1.48, c75=2.33, c76=3.31, c77=3.45, c78=3.22, c79=0.83,
                         c81=0.02, c82=0.07, c83=0.65, c84=1.16, c85=2.99, c86=3.16, c87=3.00, c88=4.64, c89=1.15,
                         c91=0.05, c92=0.03, c93=0.60, c94=0.79, c95=1.60, c96=1.65, c97=1.59, c98=3.30, c99=1.59)

param.rural.aya <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02, 
                         # gamma: duration of illness; psi: vaccine effectiveness; q: transmission probability; mu: demographics
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
init.urb <- init.dcm(s.num.1 = 300000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0,
                     s.num.2 = 240000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0,
                     s.num.3 = 1590000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0,
                     s.num.4 = 1830000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0,
                     s.num.5 = 3050000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0,
                     s.num.6 = 2200000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0,
                     s.num.7 = 1470000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0,
                     s.num.8 = 1220000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0,
                     s.num.9 = 610000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.urb.lt1 <- init.dcm(s.num.1 = 300000*(1-0.50), s.v.num.1 = 300000*0.50, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0,
                     s.num.2 = 240000*(1-0.50), s.v.num.2 = 240000*0.50, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0,
                     s.num.3 = 1590000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0,
                     s.num.4 = 1830000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0,
                     s.num.5 = 3050000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0,
                     s.num.6 = 2200000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0,
                     s.num.7 = 1470000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0,
                     s.num.8 = 1220000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0,
                     s.num.9 = 610000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.urb.aya <- init.dcm(s.num.1 = 300000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0,
                     s.num.2 = 240000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0,
                     s.num.3 = 1590000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0,
                     s.num.4 = 1830000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0,
                     s.num.5 = 3050000*(1-0.50), s.v.num.5 = 3050000*0.50, i.num.5 = 1, i.v.num.5 = 5, r.num.5 = 0, r.v.num.5 = 0,
                     s.num.6 = 2200000*(1-0.50), s.v.num.6 = 2200000*0.50, i.num.6 = 1, i.v.num.6 = 6, r.num.6 = 0, r.v.num.6 = 0,
                     s.num.7 = 1470000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0,
                     s.num.8 = 1220000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0,
                     s.num.9 = 610000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)
# - Rural
init.rur <- init.dcm(s.num.1 = 520000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0,
                     s.num.2 = 420000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0,
                     s.num.3 = 2700000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0,
                     s.num.4 = 3120000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0,
                     s.num.5 = 5200000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0,
                     s.num.6 = 3740000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0,
                     s.num.7 = 2500000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0,
                     s.num.8 = 2080000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0,
                     s.num.9 = 1040000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.rur.lt1 <- init.dcm(s.num.1 = 520000*(1-0.50), s.v.num.1 = 520000*0.50, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0,
                     s.num.2 = 420000*(1-0.50), s.v.num.2 = 420000*0.50, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0,
                     s.num.3 = 2700000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0,
                     s.num.4 = 3120000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0,
                     s.num.5 = 5200000, s.v.num.5 = 0, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0,
                     s.num.6 = 3740000, s.v.num.6 = 0, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0,
                     s.num.7 = 2500000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0,
                     s.num.8 = 2080000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0,
                     s.num.9 = 1040000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0,
                     si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                     si.flow.5 = 0, si.flow.6 = 0,  si.flow.7 = 0, si.flow.8 = 0, si.flow.9 = 0, 
                     svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                     svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0, svi.flow.8 = 0, svi.flow.9 = 0,
                     sni.flow.1 = 0, sni.flow.2 = 0, sni.flow.3 = 0, sni.flow.4 = 0,
                     sni.flow.5 = 0, sni.flow.6 = 0, sni.flow.7 = 0, sni.flow.8 = 0, sni.flow.9 = 0)

init.rur.aya <- init.dcm(s.num.1 = 520000, s.v.num.1 = 0, i.num.1 = 1, i.v.num.1 = 1, r.num.1 = 0, r.v.num.1 = 0,
                     s.num.2 = 420000, s.v.num.2 = 0, i.num.2 = 1, i.v.num.2 = 1, r.num.2 = 0, r.v.num.2 = 0,
                     s.num.3 = 2700000, s.v.num.3 = 0, i.num.3 = 1, i.v.num.3 = 1, r.num.3 = 0, r.v.num.3 = 0,
                     s.num.4 = 3120000, s.v.num.4 = 0, i.num.4 = 1, i.v.num.4 = 1, r.num.4 = 0, r.v.num.4 = 0,
                     s.num.5 = 5200000*(1-0.50), s.v.num.5 = 5200000*0.50, i.num.5 = 1, i.v.num.5 = 1, r.num.5 = 0, r.v.num.5 = 0,
                     s.num.6 = 3740000*(1-0.50), s.v.num.6 = 3740000*0.50, i.num.6 = 1, i.v.num.6 = 1, r.num.6 = 0, r.v.num.6 = 0,
                     s.num.7 = 2500000, s.v.num.7 = 0, i.num.7 = 1, i.v.num.7 = 1, r.num.7 = 0, r.v.num.7 = 0,
                     s.num.8 = 2080000, s.v.num.8 = 0, i.num.8 = 1, i.v.num.8 = 1, r.num.8 = 0, r.v.num.8 = 0,
                     s.num.9 = 1040000, s.v.num.9 = 0, i.num.9 = 1, i.v.num.9 = 1, r.num.9 = 0, r.v.num.9 = 0,
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




# --- Make a dataframe with all Mozambique results
df.moz <- matrix(ncol=0, nrow=730)
df.moz <- as.data.frame(df.moz)

# -- Urban
# - No vaccine
df.moz$si.no.urb.moz.1 <- df.no.urb$si.flow.1
df.moz$si.no.urb.moz.2 <- df.no.urb$si.flow.2
df.moz$si.no.urb.moz.3 <- df.no.urb$si.flow.3
df.moz$si.no.urb.moz.4 <- df.no.urb$si.flow.4
df.moz$si.no.urb.moz.5 <- df.no.urb$si.flow.5
df.moz$si.no.urb.moz.6 <- df.no.urb$si.flow.6
df.moz$si.no.urb.moz.7 <- df.no.urb$si.flow.7
df.moz$si.no.urb.moz.8 <- df.no.urb$si.flow.8
df.moz$si.no.urb.moz.9 <- df.no.urb$si.flow.9


# Vaccine less than 1
df.moz$si.lt1.urb.moz.1 <- df.lt1.urb$si.flow.1
df.moz$si.lt1.urb.moz.2 <- df.lt1.urb$si.flow.2
df.moz$si.lt1.urb.moz.3 <- df.lt1.urb$si.flow.3
df.moz$si.lt1.urb.moz.4 <- df.lt1.urb$si.flow.4
df.moz$si.lt1.urb.moz.5 <- df.lt1.urb$si.flow.5
df.moz$si.lt1.urb.moz.6 <- df.lt1.urb$si.flow.6
df.moz$si.lt1.urb.moz.7 <- df.lt1.urb$si.flow.7
df.moz$si.lt1.urb.moz.8 <- df.lt1.urb$si.flow.8
df.moz$si.lt1.urb.moz.9 <- df.lt1.urb$si.flow.9
df.moz$svi.lt1.urb.moz.1 <- df.lt1.urb$svi.flow.1
df.moz$svi.lt1.urb.moz.2 <- df.lt1.urb$svi.flow.2
df.moz$svi.lt1.urb.moz.3 <- df.lt1.urb$svi.flow.3
df.moz$svi.lt1.urb.moz.4 <- df.lt1.urb$svi.flow.4
df.moz$svi.lt1.urb.moz.5 <- df.lt1.urb$svi.flow.5
df.moz$svi.lt1.urb.moz.6 <- df.lt1.urb$svi.flow.6
df.moz$svi.lt1.urb.moz.7 <- df.lt1.urb$svi.flow.7
df.moz$svi.lt1.urb.moz.8 <- df.lt1.urb$svi.flow.8
df.moz$svi.lt1.urb.moz.9 <- df.lt1.urb$svi.flow.9
df.moz$sni.lt1.urb.moz.1 <- df.lt1.urb$sni.flow.1
df.moz$sni.lt1.urb.moz.2 <- df.lt1.urb$sni.flow.2
df.moz$sni.lt1.urb.moz.3 <- df.lt1.urb$sni.flow.3
df.moz$sni.lt1.urb.moz.4 <- df.lt1.urb$sni.flow.4
df.moz$sni.lt1.urb.moz.5 <- df.lt1.urb$sni.flow.5
df.moz$sni.lt1.urb.moz.6 <- df.lt1.urb$sni.flow.6
df.moz$sni.lt1.urb.moz.7 <- df.lt1.urb$sni.flow.7
df.moz$sni.lt1.urb.moz.8 <- df.lt1.urb$sni.flow.8
df.moz$sni.lt1.urb.moz.9 <- df.lt1.urb$sni.flow.9

# Vaccine in 10-29 y/o (adolescents and young adults)
df.moz$si.aya.urb.moz.1 <- df.aya.urb$si.flow.1
df.moz$si.aya.urb.moz.2 <- df.aya.urb$si.flow.2
df.moz$si.aya.urb.moz.3 <- df.aya.urb$si.flow.3
df.moz$si.aya.urb.moz.4 <- df.aya.urb$si.flow.4
df.moz$si.aya.urb.moz.5 <- df.aya.urb$si.flow.5
df.moz$si.aya.urb.moz.6 <- df.aya.urb$si.flow.6
df.moz$si.aya.urb.moz.7 <- df.aya.urb$si.flow.7
df.moz$si.aya.urb.moz.8 <- df.aya.urb$si.flow.8
df.moz$si.aya.urb.moz.9 <- df.aya.urb$si.flow.9
df.moz$svi.aya.urb.moz.1 <- df.aya.urb$svi.flow.1
df.moz$svi.aya.urb.moz.2 <- df.aya.urb$svi.flow.2
df.moz$svi.aya.urb.moz.3 <- df.aya.urb$svi.flow.3
df.moz$svi.aya.urb.moz.4 <- df.aya.urb$svi.flow.4
df.moz$svi.aya.urb.moz.5 <- df.aya.urb$svi.flow.5
df.moz$svi.aya.urb.moz.6 <- df.aya.urb$svi.flow.6
df.moz$svi.aya.urb.moz.7 <- df.aya.urb$svi.flow.7
df.moz$svi.aya.urb.moz.8 <- df.aya.urb$svi.flow.8
df.moz$svi.aya.urb.moz.9 <- df.aya.urb$svi.flow.9
df.moz$sni.aya.urb.moz.1 <- df.aya.urb$sni.flow.1
df.moz$sni.aya.urb.moz.2 <- df.aya.urb$sni.flow.2
df.moz$sni.aya.urb.moz.3 <- df.aya.urb$sni.flow.3
df.moz$sni.aya.urb.moz.4 <- df.aya.urb$sni.flow.4
df.moz$sni.aya.urb.moz.5 <- df.aya.urb$sni.flow.5
df.moz$sni.aya.urb.moz.6 <- df.aya.urb$sni.flow.6
df.moz$sni.aya.urb.moz.7 <- df.aya.urb$sni.flow.7
df.moz$sni.aya.urb.moz.8 <- df.aya.urb$sni.flow.8
df.moz$sni.aya.urb.moz.9 <- df.aya.urb$sni.flow.9


# -- Rural
# - No vaccine
df.moz$si.no.rur.moz.1 <- df.no.rur$si.flow.1
df.moz$si.no.rur.moz.2 <- df.no.rur$si.flow.2
df.moz$si.no.rur.moz.3 <- df.no.rur$si.flow.3
df.moz$si.no.rur.moz.4 <- df.no.rur$si.flow.4
df.moz$si.no.rur.moz.5 <- df.no.rur$si.flow.5
df.moz$si.no.rur.moz.6 <- df.no.rur$si.flow.6
df.moz$si.no.rur.moz.7 <- df.no.rur$si.flow.7
df.moz$si.no.rur.moz.8 <- df.no.rur$si.flow.8
df.moz$si.no.rur.moz.9 <- df.no.rur$si.flow.9

# Vaccine less than 1
df.moz$si.lt1.rur.moz.1 <- df.lt1.rur$si.flow.1
df.moz$si.lt1.rur.moz.2 <- df.lt1.rur$si.flow.2
df.moz$si.lt1.rur.moz.3 <- df.lt1.rur$si.flow.3
df.moz$si.lt1.rur.moz.4 <- df.lt1.rur$si.flow.4
df.moz$si.lt1.rur.moz.5 <- df.lt1.rur$si.flow.5
df.moz$si.lt1.rur.moz.6 <- df.lt1.rur$si.flow.6
df.moz$si.lt1.rur.moz.7 <- df.lt1.rur$si.flow.7
df.moz$si.lt1.rur.moz.8 <- df.lt1.rur$si.flow.8
df.moz$si.lt1.rur.moz.9 <- df.lt1.rur$si.flow.9
df.moz$svi.lt1.rur.moz.1 <- df.lt1.rur$svi.flow.1
df.moz$svi.lt1.rur.moz.2 <- df.lt1.rur$svi.flow.2
df.moz$svi.lt1.rur.moz.3 <- df.lt1.rur$svi.flow.3
df.moz$svi.lt1.rur.moz.4 <- df.lt1.rur$svi.flow.4
df.moz$svi.lt1.rur.moz.5 <- df.lt1.rur$svi.flow.5
df.moz$svi.lt1.rur.moz.6 <- df.lt1.rur$svi.flow.6
df.moz$svi.lt1.rur.moz.7 <- df.lt1.rur$svi.flow.7
df.moz$svi.lt1.rur.moz.8 <- df.lt1.rur$svi.flow.8
df.moz$svi.lt1.rur.moz.9 <- df.lt1.rur$svi.flow.9
df.moz$sni.lt1.rur.moz.1 <- df.lt1.rur$sni.flow.1
df.moz$sni.lt1.rur.moz.2 <- df.lt1.rur$sni.flow.2
df.moz$sni.lt1.rur.moz.3 <- df.lt1.rur$sni.flow.3
df.moz$sni.lt1.rur.moz.4 <- df.lt1.rur$sni.flow.4
df.moz$sni.lt1.rur.moz.5 <- df.lt1.rur$sni.flow.5
df.moz$sni.lt1.rur.moz.6 <- df.lt1.rur$sni.flow.6
df.moz$sni.lt1.rur.moz.7 <- df.lt1.rur$sni.flow.7
df.moz$sni.lt1.rur.moz.8 <- df.lt1.rur$sni.flow.8
df.moz$sni.lt1.rur.moz.9 <- df.lt1.rur$sni.flow.9

# Vaccine in 10-29 y/o (adolescents and young adults)
df.moz$si.aya.rur.moz.1 <- df.aya.rur$si.flow.1
df.moz$si.aya.rur.moz.2 <- df.aya.rur$si.flow.2
df.moz$si.aya.rur.moz.3 <- df.aya.rur$si.flow.3
df.moz$si.aya.rur.moz.4 <- df.aya.rur$si.flow.4
df.moz$si.aya.rur.moz.5 <- df.aya.rur$si.flow.5
df.moz$si.aya.rur.moz.6 <- df.aya.rur$si.flow.6
df.moz$si.aya.rur.moz.7 <- df.aya.rur$si.flow.7
df.moz$si.aya.rur.moz.8 <- df.aya.rur$si.flow.8
df.moz$si.aya.rur.moz.9 <- df.aya.rur$si.flow.9
df.moz$svi.aya.rur.moz.1 <- df.aya.rur$svi.flow.1
df.moz$svi.aya.rur.moz.2 <- df.aya.rur$svi.flow.2
df.moz$svi.aya.rur.moz.3 <- df.aya.rur$svi.flow.3
df.moz$svi.aya.rur.moz.4 <- df.aya.rur$svi.flow.4
df.moz$svi.aya.rur.moz.5 <- df.aya.rur$svi.flow.5
df.moz$svi.aya.rur.moz.6 <- df.aya.rur$svi.flow.6
df.moz$svi.aya.rur.moz.7 <- df.aya.rur$svi.flow.7
df.moz$svi.aya.rur.moz.8 <- df.aya.rur$svi.flow.8
df.moz$svi.aya.rur.moz.9 <- df.aya.rur$svi.flow.9
df.moz$sni.aya.rur.moz.1 <- df.aya.rur$sni.flow.1
df.moz$sni.aya.rur.moz.2 <- df.aya.rur$sni.flow.2
df.moz$sni.aya.rur.moz.3 <- df.aya.rur$sni.flow.3
df.moz$sni.aya.rur.moz.4 <- df.aya.rur$sni.flow.4
df.moz$sni.aya.rur.moz.5 <- df.aya.rur$sni.flow.5
df.moz$sni.aya.rur.moz.6 <- df.aya.rur$sni.flow.6
df.moz$sni.aya.rur.moz.7 <- df.aya.rur$sni.flow.7
df.moz$sni.aya.rur.moz.8 <- df.aya.rur$sni.flow.8
df.moz$sni.aya.rur.moz.9 <- df.aya.rur$sni.flow.9


# -- Time
df.moz$time <- df.aya.rur$time


# ---------- LT1 VE Rural ---------- #
# ----- calculate overall ve
1-((sum(df.moz$si.lt1.rur.moz.1)/(init.rur.lt1$s.num.1+init.rur.lt1$s.v.num.1)) / (sum(df.moz$si.no.rur.moz.1)/(init.rur$s.num.1)))
1-((sum(df.moz$si.lt1.rur.moz.2)/(init.rur.lt1$s.num.2+init.rur.lt1$s.v.num.2)) / (sum(df.moz$si.no.rur.moz.2)/(init.rur$s.num.2)))
1-((sum(df.moz$si.lt1.rur.moz.3)/(init.rur.lt1$s.num.3+init.rur.lt1$s.v.num.3)) / (sum(df.moz$si.no.rur.moz.3)/(init.rur$s.num.3)))
1-((sum(df.moz$si.lt1.rur.moz.4)/(init.rur.lt1$s.num.4+init.rur.lt1$s.v.num.4)) / (sum(df.moz$si.no.rur.moz.4)/(init.rur$s.num.4)))
1-((sum(df.moz$si.lt1.rur.moz.5)/(init.rur.lt1$s.num.5+init.rur.lt1$s.v.num.5)) / (sum(df.moz$si.no.rur.moz.5)/(init.rur$s.num.5)))
1-((sum(df.moz$si.lt1.rur.moz.6)/(init.rur.lt1$s.num.6+init.rur.lt1$s.v.num.6)) / (sum(df.moz$si.no.rur.moz.6)/(init.rur$s.num.6)))
1-((sum(df.moz$si.lt1.rur.moz.7)/(init.rur.lt1$s.num.7+init.rur.lt1$s.v.num.7)) / (sum(df.moz$si.no.rur.moz.7)/(init.rur$s.num.7)))
1-((sum(df.moz$si.lt1.rur.moz.8)/(init.rur.lt1$s.num.8+init.rur.lt1$s.v.num.8)) / (sum(df.moz$si.no.rur.moz.8)/(init.rur$s.num.8)))
1-((sum(df.moz$si.lt1.rur.moz.9)/(init.rur.lt1$s.num.9+init.rur.lt1$s.v.num.9)) / (sum(df.moz$si.no.rur.moz.9)/(init.rur$s.num.9)))

# ----- calculate direct ve
1-((sum(df.moz$svi.lt1.rur.moz.1)/(init.rur.lt1$s.v.num.1)) / (sum(df.moz$sni.lt1.rur.moz.1)/(init.rur.lt1$s.num.1)))
1-((sum(df.moz$svi.lt1.rur.moz.2)/(init.rur.lt1$s.v.num.2)) / (sum(df.moz$sni.lt1.rur.moz.2)/(init.rur.lt1$s.num.2)))

# ----- calculate indirect ve
1-((sum(df.moz$sni.lt1.rur.moz.1)/(init.rur.lt1$s.num.1)) / (sum(df.moz$si.no.rur.moz.1)/(init.rur$s.num.1)))
1-((sum(df.moz$sni.lt1.rur.moz.2)/(init.rur.lt1$s.num.2)) / (sum(df.moz$si.no.rur.moz.2)/(init.rur$s.num.2)))
1-((sum(df.moz$sni.lt1.rur.moz.3)/(init.rur.lt1$s.num.3)) / (sum(df.moz$si.no.rur.moz.3)/(init.rur$s.num.3)))
1-((sum(df.moz$sni.lt1.rur.moz.4)/(init.rur.lt1$s.num.4)) / (sum(df.moz$si.no.rur.moz.4)/(init.rur$s.num.4)))
1-((sum(df.moz$sni.lt1.rur.moz.5)/(init.rur.lt1$s.num.5)) / (sum(df.moz$si.no.rur.moz.5)/(init.rur$s.num.5)))
1-((sum(df.moz$sni.lt1.rur.moz.6)/(init.rur.lt1$s.num.6)) / (sum(df.moz$si.no.rur.moz.6)/(init.rur$s.num.6)))
1-((sum(df.moz$sni.lt1.rur.moz.7)/(init.rur.lt1$s.num.7)) / (sum(df.moz$si.no.rur.moz.7)/(init.rur$s.num.7)))
1-((sum(df.moz$sni.lt1.rur.moz.8)/(init.rur.lt1$s.num.8)) / (sum(df.moz$si.no.rur.moz.8)/(init.rur$s.num.8)))
1-((sum(df.moz$sni.lt1.rur.moz.9)/(init.rur.lt1$s.num.9)) / (sum(df.moz$si.no.rur.moz.9)/(init.rur$s.num.9)))


# ---------- LT1 VE Urban ---------- #
# ----- calculate overall ve
1-((sum(df.moz$si.lt1.urb.moz.1)/(init.urb.lt1$s.num.1+init.urb.lt1$s.v.num.1)) / (sum(df.moz$si.no.urb.moz.1)/(init.urb$s.num.1)))
1-((sum(df.moz$si.lt1.urb.moz.2)/(init.urb.lt1$s.num.2+init.urb.lt1$s.v.num.2)) / (sum(df.moz$si.no.urb.moz.2)/(init.urb$s.num.2)))
1-((sum(df.moz$si.lt1.urb.moz.3)/(init.urb.lt1$s.num.3+init.urb.lt1$s.v.num.3)) / (sum(df.moz$si.no.urb.moz.3)/(init.urb$s.num.3)))
1-((sum(df.moz$si.lt1.urb.moz.4)/(init.urb.lt1$s.num.4+init.urb.lt1$s.v.num.4)) / (sum(df.moz$si.no.urb.moz.4)/(init.urb$s.num.4)))
1-((sum(df.moz$si.lt1.urb.moz.5)/(init.urb.lt1$s.num.5+init.urb.lt1$s.v.num.5)) / (sum(df.moz$si.no.urb.moz.5)/(init.urb$s.num.5)))
1-((sum(df.moz$si.lt1.urb.moz.6)/(init.urb.lt1$s.num.6+init.urb.lt1$s.v.num.6)) / (sum(df.moz$si.no.urb.moz.6)/(init.urb$s.num.6)))
1-((sum(df.moz$si.lt1.urb.moz.7)/(init.urb.lt1$s.num.7+init.urb.lt1$s.v.num.7)) / (sum(df.moz$si.no.urb.moz.7)/(init.urb$s.num.7)))
1-((sum(df.moz$si.lt1.urb.moz.8)/(init.urb.lt1$s.num.8+init.urb.lt1$s.v.num.8)) / (sum(df.moz$si.no.urb.moz.8)/(init.urb$s.num.8)))
1-((sum(df.moz$si.lt1.urb.moz.9)/(init.urb.lt1$s.num.9+init.urb.lt1$s.v.num.9)) / (sum(df.moz$si.no.urb.moz.9)/(init.urb$s.num.9)))

# ----- calculate direct ve
1-((sum(df.moz$svi.lt1.urb.moz.1)/(init.urb.lt1$s.v.num.1)) / (sum(df.moz$sni.lt1.urb.moz.1)/(init.urb.lt1$s.num.1)))
1-((sum(df.moz$svi.lt1.urb.moz.2)/(init.urb.lt1$s.v.num.2)) / (sum(df.moz$sni.lt1.urb.moz.2)/(init.urb.lt1$s.num.2)))

# ----- calculate indirect ve
1-((sum(df.moz$sni.lt1.urb.moz.1)/(init.urb.lt1$s.num.1)) / (sum(df.moz$si.no.urb.moz.1)/(init.urb$s.num.1)))
1-((sum(df.moz$sni.lt1.urb.moz.2)/(init.urb.lt1$s.num.2)) / (sum(df.moz$si.no.urb.moz.2)/(init.urb$s.num.2)))
1-((sum(df.moz$sni.lt1.urb.moz.3)/(init.urb.lt1$s.num.3)) / (sum(df.moz$si.no.urb.moz.3)/(init.urb$s.num.3)))
1-((sum(df.moz$sni.lt1.urb.moz.4)/(init.urb.lt1$s.num.4)) / (sum(df.moz$si.no.urb.moz.4)/(init.urb$s.num.4)))
1-((sum(df.moz$sni.lt1.urb.moz.5)/(init.urb.lt1$s.num.5)) / (sum(df.moz$si.no.urb.moz.5)/(init.urb$s.num.5)))
1-((sum(df.moz$sni.lt1.urb.moz.6)/(init.urb.lt1$s.num.6)) / (sum(df.moz$si.no.urb.moz.6)/(init.urb$s.num.6)))
1-((sum(df.moz$sni.lt1.urb.moz.7)/(init.urb.lt1$s.num.7)) / (sum(df.moz$si.no.urb.moz.7)/(init.urb$s.num.7)))
1-((sum(df.moz$sni.lt1.urb.moz.8)/(init.urb.lt1$s.num.8)) / (sum(df.moz$si.no.urb.moz.8)/(init.urb$s.num.8)))
1-((sum(df.moz$sni.lt1.urb.moz.9)/(init.urb.lt1$s.num.9)) / (sum(df.moz$si.no.urb.moz.9)/(init.urb$s.num.9)))

# ---------- AYA VE Rural ---------- #
# ----- calculate overall ve
1-((sum(df.moz$si.aya.rur.moz.1)/(init.rur.aya$s.num.1+init.rur.aya$s.v.num.1)) / (sum(df.moz$si.no.rur.moz.1)/(init.rur$s.num.1)))
1-((sum(df.moz$si.aya.rur.moz.2)/(init.rur.aya$s.num.2+init.rur.aya$s.v.num.2)) / (sum(df.moz$si.no.rur.moz.2)/(init.rur$s.num.2)))
1-((sum(df.moz$si.aya.rur.moz.3)/(init.rur.aya$s.num.3+init.rur.aya$s.v.num.3)) / (sum(df.moz$si.no.rur.moz.3)/(init.rur$s.num.3)))
1-((sum(df.moz$si.aya.rur.moz.4)/(init.rur.aya$s.num.4+init.rur.aya$s.v.num.4)) / (sum(df.moz$si.no.rur.moz.4)/(init.rur$s.num.4)))
1-((sum(df.moz$si.aya.rur.moz.5)/(init.rur.aya$s.num.5+init.rur.aya$s.v.num.5)) / (sum(df.moz$si.no.rur.moz.5)/(init.rur$s.num.5)))
1-((sum(df.moz$si.aya.rur.moz.6)/(init.rur.aya$s.num.6+init.rur.aya$s.v.num.6)) / (sum(df.moz$si.no.rur.moz.6)/(init.rur$s.num.6)))
1-((sum(df.moz$si.aya.rur.moz.7)/(init.rur.aya$s.num.7+init.rur.aya$s.v.num.7)) / (sum(df.moz$si.no.rur.moz.7)/(init.rur$s.num.7)))
1-((sum(df.moz$si.aya.rur.moz.8)/(init.rur.aya$s.num.8+init.rur.aya$s.v.num.8)) / (sum(df.moz$si.no.rur.moz.8)/(init.rur$s.num.8)))
1-((sum(df.moz$si.aya.rur.moz.9)/(init.rur.aya$s.num.9+init.rur.aya$s.v.num.9)) / (sum(df.moz$si.no.rur.moz.9)/(init.rur$s.num.9)))

# ----- calculate direct ve
1-((sum(df.moz$svi.aya.rur.moz.5)/(init.rur.aya$s.v.num.5)) / (sum(df.moz$sni.aya.rur.moz.5)/(init.rur.aya$s.num.5)))
1-((sum(df.moz$svi.aya.rur.moz.6)/(init.rur.aya$s.v.num.6)) / (sum(df.moz$sni.aya.rur.moz.6)/(init.rur.aya$s.num.6)))

# ----- calculate indirect ve
1-((sum(df.moz$sni.aya.rur.moz.1)/(init.rur.aya$s.num.1)) / (sum(df.moz$si.no.rur.moz.1)/(init.rur$s.num.1)))
1-((sum(df.moz$sni.aya.rur.moz.2)/(init.rur.aya$s.num.2)) / (sum(df.moz$si.no.rur.moz.2)/(init.rur$s.num.2)))
1-((sum(df.moz$sni.aya.rur.moz.3)/(init.rur.aya$s.num.3)) / (sum(df.moz$si.no.rur.moz.3)/(init.rur$s.num.3)))
1-((sum(df.moz$sni.aya.rur.moz.4)/(init.rur.aya$s.num.4)) / (sum(df.moz$si.no.rur.moz.4)/(init.rur$s.num.4)))
1-((sum(df.moz$sni.aya.rur.moz.5)/(init.rur.aya$s.num.5)) / (sum(df.moz$si.no.rur.moz.5)/(init.rur$s.num.5)))
1-((sum(df.moz$sni.aya.rur.moz.6)/(init.rur.aya$s.num.6)) / (sum(df.moz$si.no.rur.moz.6)/(init.rur$s.num.6)))
1-((sum(df.moz$sni.aya.rur.moz.7)/(init.rur.aya$s.num.7)) / (sum(df.moz$si.no.rur.moz.7)/(init.rur$s.num.7)))
1-((sum(df.moz$sni.aya.rur.moz.8)/(init.rur.aya$s.num.8)) / (sum(df.moz$si.no.rur.moz.8)/(init.rur$s.num.8)))
1-((sum(df.moz$sni.aya.rur.moz.9)/(init.rur.aya$s.num.9)) / (sum(df.moz$si.no.rur.moz.9)/(init.rur$s.num.9)))


# ---------- AYA VE Urban ---------- #
# ----- calculate overall ve
1-((sum(df.moz$si.aya.urb.moz.1)/(init.urb.aya$s.num.1+init.urb.aya$s.v.num.1)) / (sum(df.moz$si.no.urb.moz.1)/(init.urb$s.num.1)))
1-((sum(df.moz$si.aya.urb.moz.2)/(init.urb.aya$s.num.2+init.urb.aya$s.v.num.2)) / (sum(df.moz$si.no.urb.moz.2)/(init.urb$s.num.2)))
1-((sum(df.moz$si.aya.urb.moz.3)/(init.urb.aya$s.num.3+init.urb.aya$s.v.num.3)) / (sum(df.moz$si.no.urb.moz.3)/(init.urb$s.num.3)))
1-((sum(df.moz$si.aya.urb.moz.4)/(init.urb.aya$s.num.4+init.urb.aya$s.v.num.4)) / (sum(df.moz$si.no.urb.moz.4)/(init.urb$s.num.4)))
1-((sum(df.moz$si.aya.urb.moz.5)/(init.urb.aya$s.num.5+init.urb.aya$s.v.num.5)) / (sum(df.moz$si.no.urb.moz.5)/(init.urb$s.num.5)))
1-((sum(df.moz$si.aya.urb.moz.6)/(init.urb.aya$s.num.6+init.urb.aya$s.v.num.6)) / (sum(df.moz$si.no.urb.moz.6)/(init.urb$s.num.6)))
1-((sum(df.moz$si.aya.urb.moz.7)/(init.urb.aya$s.num.7+init.urb.aya$s.v.num.7)) / (sum(df.moz$si.no.urb.moz.7)/(init.urb$s.num.7)))
1-((sum(df.moz$si.aya.urb.moz.8)/(init.urb.aya$s.num.8+init.urb.aya$s.v.num.8)) / (sum(df.moz$si.no.urb.moz.8)/(init.urb$s.num.8)))
1-((sum(df.moz$si.aya.urb.moz.9)/(init.urb.aya$s.num.9+init.urb.aya$s.v.num.9)) / (sum(df.moz$si.no.urb.moz.9)/(init.urb$s.num.9)))

# ----- calculate direct ve
1-((sum(df.moz$svi.aya.urb.moz.5)/(init.urb.aya$s.v.num.5)) / (sum(df.moz$sni.aya.urb.moz.5)/(init.urb.aya$s.num.5)))
1-((sum(df.moz$svi.aya.urb.moz.6)/(init.urb.aya$s.v.num.6)) / (sum(df.moz$sni.aya.urb.moz.6)/(init.urb.aya$s.num.6)))

# ----- calculate indirect ve
1-((sum(df.moz$sni.aya.urb.moz.1)/(init.urb.aya$s.num.1)) / (sum(df.moz$si.no.urb.moz.1)/(init.urb$s.num.1)))
1-((sum(df.moz$sni.aya.urb.moz.2)/(init.urb.aya$s.num.2)) / (sum(df.moz$si.no.urb.moz.2)/(init.urb$s.num.2)))
1-((sum(df.moz$sni.aya.urb.moz.3)/(init.urb.aya$s.num.3)) / (sum(df.moz$si.no.urb.moz.3)/(init.urb$s.num.3)))
1-((sum(df.moz$sni.aya.urb.moz.4)/(init.urb.aya$s.num.4)) / (sum(df.moz$si.no.urb.moz.4)/(init.urb$s.num.4)))
1-((sum(df.moz$sni.aya.urb.moz.5)/(init.urb.aya$s.num.5)) / (sum(df.moz$si.no.urb.moz.5)/(init.urb$s.num.5)))
1-((sum(df.moz$sni.aya.urb.moz.6)/(init.urb.aya$s.num.6)) / (sum(df.moz$si.no.urb.moz.6)/(init.urb$s.num.6)))
1-((sum(df.moz$sni.aya.urb.moz.7)/(init.urb.aya$s.num.7)) / (sum(df.moz$si.no.urb.moz.7)/(init.urb$s.num.7)))
1-((sum(df.moz$sni.aya.urb.moz.8)/(init.urb.aya$s.num.8)) / (sum(df.moz$si.no.urb.moz.8)/(init.urb$s.num.8)))
1-((sum(df.moz$sni.aya.urb.moz.9)/(init.urb.aya$s.num.9)) / (sum(df.moz$si.no.urb.moz.9)/(init.urb$s.num.9)))


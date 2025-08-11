#Parameters and the model is taken from Taylor et al (2014) 
#A Computational Study of Stimulus Driven Epileptic Seizure Abatement


#libraries

setwd("/Users/ntellioglu/OneDrive - The University of Melbourne/Epilepsy")
source("Code/params.R")

#activation functions
f <- function(epsilon, u) (1/(1 + epsilon^(-u))) 
s <- function(a,b,u) a*u + b

#deterministic simulator
det.simulator <-function(p){
  #simulates epilepsy model of Taylor et al. 2014
  #params is a list of params
  #order of init parameter matters: [PY, IN, TC, RE]
  # nsteps: time steps (unit is not defined yet)
  # init: initla values of stock variables [PY, IN, TC, RE]
  #for ex:
  # params <- list(nsteps = 10, init = c(0.1724,0.1787,-0.0818,0.2775) ,
  #             tau1 = 26, tau2 = 32.5, tau3 = 2.6, tau4 = 2.6, 
  #             h.py = -0.35, h.in = -3.4, h.tc = -2.05, h.re = -5,
  #             C = c(1.8,4,1.5,0.2,10.5,0.6,3,3,1), a = 2.8, b = 1.5, 
  #             epsilon = 250000, alpha =0.022)
  
  nsteps <- p$nsteps
  #creat vectors for stock values 
  PPY <- rep(c(p$init[1]), nsteps)
  IIN <- rep(c(p$init[2]), nsteps)
  TTC <- rep(c(p$init[3]), nsteps)
  RRE <- rep(c(p$init[4]), nsteps)
  
  tau1 <- p$tau1
  tau2 <- p$tau2
  tau3 <- p$tau3
  tau4 <- p$tau4
  h.py <- p$h.py 
  h.in <- p$h.in 
  h.tc <- p$h.tc  
  h.re <- p$h.re
  C1 <- p$C[1]
  C2 <- p$C[2]
  C3 <- p$C[3]
  C4 <- p$C[4]
  C5 <- p$C[5]
  C6 <- p$C[6]
  C7 <- p$C[7]
  C8 <- p$C[8]
  C9 <- p$C[9]
  eps <- p$epsilon
  a <- p$a
  b <- p$b
  alpha <- p$alpha  
  dt1 <- p$dt1
  for (t in 1:nsteps){
    #extract current stock values
    tPY <- PPY[t]
    tIN <- IIN[t]
    tTC <- TTC[t]
    tRE <- RRE[t]
    
    #calculate flows
    dPY <- dt1 * tau1 * (h.py - tPY + C1* f(eps, tPY) 
                   - C3 * f(eps, tIN)+ C9 * f(eps, tTC))
    dIN <- dt1 * tau2 * (h.in - tIN + C2 * f(eps, tPY))
    dTC <- dt1 *(tau3 * (h.tc - tTC + C7 * f(eps, tPY)) - C6 * s(a,b,tRE))    #Check here: alpha is 0 anyway
    dRE <- dt1 *(tau4 * (h.re - tRE + C8 * f(eps, tPY)) - C4 * s(a,b, tRE) + C5 * s(a,b,tTC))
    
    
    # Update next timestep
    PPY[t+1] = tPY + dPY
    IIN[t+1] = tIN + dIN
    TTC[t+1] = tTC + dTC
    RRE[t+1] = tRE + dRE
    
    }
    
    sim = data.frame(
        time = c(0:(nsteps )),
        PY = PPY,
        IN = IIN,
        TC = TTC,
        RE = RRE)
    return(sim)
}

#deterministic simulator
det.simulator2 <-function(p){
  #simulates epilepsy model of Taylor et al. 2014
  #params is a list of params
  #order of init parameter matters: [PY, IN, TC, RE]
  # nsteps: time steps (unit is not defined yet)
  # init: initla values of stock variables [PY, IN, TC, RE]
  #for ex:
  # params <- list(nsteps = 10, init = c(0.1724,0.1787,-0.0818,0.2775) ,
  #             tau1 = 26, tau2 = 32.5, tau3 = 2.6, tau4 = 2.6, 
  #             h.py = -0.35, h.in = -3.4, h.tc = -2.05, h.re = -5,
  #             C = c(1.8,4,1.5,0.2,10.5,0.6,3,3,1), a = 2.8, b = 1.5, 
  #             epsilon = 250000, alpha =0.022)
  
  nsteps <- p$nsteps
  #creat vectors for stock values 
  PPY <- rep(c(p$init[1]), nsteps)
  IIN <- rep(c(p$init[2]), nsteps)
  TTC <- rep(c(p$init[3]), nsteps)
  RRE <- rep(c(p$init[4]), nsteps)
  
  tau1 <- p$tau1
  tau2 <- p$tau2
  tau3 <- p$tau3
  tau4 <- p$tau4
  h.py <- p$h.py 
  h.in <- p$h.in 
  h.tc <- p$h.tc  
  h.re <- p$h.re
  C1 <- p$C[1]
  C2 <- p$C[2]
  C3 <- p$C[3]
  C4 <- p$C[4]
  C5 <- p$C[5]
  C6 <- p$C[6]
  C7 <- p$C[7]
  C8 <- p$C[8]
  C9 <- p$C[9]
  eps <- p$epsilon
  a <- p$a
  b <- p$b
  alpha <- p$alpha  
  dt1 <- p$dt1
  for (t in 1:nsteps){
    #extract current stock values
    tPY <- PPY[t]
    tIN <- IIN[t]
    tTC <- TTC[t]
    tRE <- RRE[t]
    
    #calculate flows
    dPY <- dt1 * tau1 * (h.py - tPY + C1* f(eps, tPY) 
                        - C3 * f(eps, tIN)+ C9 * f(eps, tTC))
    dIN <- dt1 * tau2 * (h.in - tIN + C2 * f(eps, tPY))
    dTC <- dt1 *(tau3 * (h.tc - tTC + C7 * f(eps, tPY) - C6 * s(a,b,tRE))) 
    dRE <- dt1 *(tau4 * (h.re - tRE + C8 * f(eps, tPY) - C4 * s(a,b, tRE) + C5 * s(a,b,tTC)))
    
    
    # Update next timestep
    PPY[t+1] = tPY + dPY
    IIN[t+1] = tIN + dIN
    TTC[t+1] = tTC + dTC
    RRE[t+1] = tRE + dRE
    
  }
  
  sim = data.frame(
    time = c(0:(nsteps)),
    PY = PPY,
    IN = IIN,
    TC = TTC,
    RE = RRE)
  return(sim)
}

#deterministic and stochastic simulator
simulator <-function(p){
  #TODO: make sure that Euler Maruyama is applied correctly!
  
  #simulates epilepsy model of Taylor et al. 2014
  #params is a list of params
  #order of init parameter matters: [PY, IN, TC, RE]
  # nsteps: time steps (unit is not defined yet)
  # init: initla values of stock variables [PY, IN, TC, RE]
  #for ex:
  # params <- list(nsteps = 10, init = c(0.1724,0.1787,-0.0818,0.2775) ,
  #             tau1 = 26, tau2 = 32.5, tau3 = 2.6, tau4 = 2.6, 
  #             h.py = -0.35, h.in = -3.4, h.tc = -2.05, h.re = -5,
  #             C = c(1.8,4,1.5,0.2,10.5,0.6,3,3,1), a = 2.8, b = 1.5, 
  #             epsilon = 250000, alpha =0.022)
  
  nsteps <- p$nsteps
  #creat vectors for stock values 
  PPY <- rep(c(p$init[1]), nsteps)
  IIN <- rep(c(p$init[2]), nsteps)
  TTC <- rep(c(p$init[3]), nsteps)
  RRE <- rep(c(p$init[4]), nsteps)
  
  tau1 <- p$tau1
  tau2 <- p$tau2
  tau3 <- p$tau3
  tau4 <- p$tau4
  h.py <- p$h.py 
  h.in <- p$h.in 
  h.tc <- p$h.tc  
  h.re <- p$h.re
  C1 <- p$C[1]
  C2 <- p$C[2]
  C3 <- p$C[3]
  C4 <- p$C[4]
  C5 <- p$C[5]
  C6 <- p$C[6]
  C7 <- p$C[7]
  C8 <- p$C[8]
  C9 <- p$C[9]
  eps <- p$epsilon
  a <- p$a
  b <- p$b
  alpha <- p$alpha  
  dt1 <- p$dt1
  for (t in 1:nsteps){
    #extract current stock values
    tPY <- PPY[t]
    tIN <- IIN[t]
    tTC <- TTC[t]
    tRE <- RRE[t]
    
    #calculate flows
    dPY <- dt1 * tau1 * (h.py - tPY + C1* f(eps, tPY) 
                        - C3 * f(eps, tIN)+ C9 * f(eps, tTC))
    dIN <- dt1 * tau2 * (h.in - tIN + C2 * f(eps, tPY))
    dTC <- dt1 * tau3 * (h.tc - tTC + C7 * f(eps, tPY) - C6 * s(a,b,tRE)) + alpha * rnorm(1,0,sqrt(dt1))  #Check here: alpha?
    dRE <- dt1 *tau4 * (h.re - tRE + C8 * f(eps, tPY) - C4 * s(a,b, tRE) + C5 * s(a,b,tTC))
    
    
    # Update next timestep
    PPY[t+1] = tPY + dPY
    IIN[t+1] = tIN + dIN
    TTC[t+1] = tTC + dTC
    RRE[t+1] = tRE + dRE
    
  }
  
  sim = data.frame(
    time = c(0:(nsteps)),
    PY = PPY,
    IN = IIN,
    TC = TTC,
    RE = RRE)
  return(sim)
}

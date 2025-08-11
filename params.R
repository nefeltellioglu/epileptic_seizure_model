#Parameters and the model is taken from Taylor et al (2014) 
#A Computational Study of Stimulus Driven Epileptic Seizure Abatement

setwd("/Users/ntellioglu/OneDrive - The University of Melbourne/Epilepsy")

det.params <- list(nsteps = 10*15000, dt1 = 1/15000, init = c(0.1724,0.1787,-0.0818,0.2775) ,
                  tau1 = 26, tau2 = 32.5, tau3 = 2.6, tau4 = 2.6, 
                  h.py = -0.35, h.in = -3.4, h.tc = -2.0, h.re = -5,
                  C = c(1.8,4,1.5,0.2,10.5,0.6,3,3,1), a = 2.8, b = 0.5, 
                  epsilon = 250000, alpha =0)

params <- list(nsteps = 10*15000, dt1 = 1/15000, init = c(0.1724,0.1787,-0.0818,0.2775) ,
                   tau1 = 26, tau2 = 32.5, tau3 = 2.6, tau4 = 2.6, 
                   h.py = -0.35, h.in = -3.4, h.tc = -2.05, h.re = -5,
                   C = c(1.8,4,1.5,0.2,10.5,0.6,3,3,1), a = 2.8, b = 0.5, 
                   epsilon = 250000, alpha =0.022)



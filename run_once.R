.rs.restartR()
rm(list = ls())

#libraries

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("params.R")
source("simulator.R")

#background state
params$nsteps <- 10*15000
params$alpha <- 0
params$h.tc <- -2
#stoc.params$init <- c(0.02294131,  0.06490129, -0.14913010,  0.50022214)
result <- simulator(params)

#seizure state
init2 <- unname(unlist(result[nrow(result),c(2:5)])) - c(.3, .3, 0, 0)
params$nsteps <- 5*15000
params$init <- init2
result2 <- simulator(params)

#background state
init3 <- unname(unlist(result2[nrow(result2),c(2:5)])) - c(.2, .2, 0, 0)
params$nsteps <- 15*15000
params$init <- init3
result3 <- simulator(params)


plot(rowMeans(result[,c(2,3)])[0:30000])
plot(rowMeans(result2[,c(2,3)])[0:60000])
plot(rowMeans(result3[,c(2,3)])[0:30000])

plot(c(rowMeans(result[,c(2,3)])[25000:30000] , 
       rowMeans(result2[,c(2,3)])[39280:60000] , 
       rowMeans(result3[,c(2,3)])[0:30000]))






#det.params$nsteps <- 10*15000
#result <- det.simulator(det.params)
#result <- det.simulator2(det.params)

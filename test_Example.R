library(devtools)
library(testthat)

library(BBmisc)
library(deSolve)

load_all("skel", reset = TRUE)

#########################
# 1d Beispiel
#########################
masses <- 1
dampers <- as.matrix(1.5)
springs <- as.matrix(4)

odenet <- ODEnetwork(masses, dampers, springs)
odenet <- setState(odenet, 3, 0)
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet)

odenet <- setState(odenet, 0, 0)
eventdat <- data.frame(  var = c("x.1", "x.1", "v.1")
                         , time = c(1, 2, 5)
                         , value = c(2, 3, 4)
)
odenet <- setEvents(odenet, eventdat, type = "constant")
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet)

plot(odenet, select = "state1")
plot(odenet, select = "state2")
plot(odenet, select = "state1vs2")

# Testweise
createState(odenet)
createOscillators(odenet)
createParamVec(odenet)

#########################
# 2d Beispiel
#########################
masses <- c(1, 2)
dampers <- diag(c(1.1, 1.5))
dampers[1, 2] <- 0.5
springs <- diag(c(4, 0))
springs[1, 2] <- 6

odenet <- ODEnetwork(masses, dampers, springs)
odenet <- setState(odenet, c(1, 3), c(0, 0))
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.05))

plot(odenet)
plot(odenet, select = "state1")
plot(odenet, select = "state2")
plot(odenet, select = "state1vs2")

createParamVec(odenet)
createState(odenet)
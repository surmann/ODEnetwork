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
odenet <- setState(odenet, 0, 3)
odenet <- setState(odenet, cbind(time=c(0, 0.5, 1), x=c(0, 1, 0)), 3)
odenet$state
createState(odenet, 0.5)
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))

createOscillators(odenet)

mRes <- getResult(odenet)
plot(diff(mRes[, 3]), type = "l")

plot(odenet)
plot(odenet, select = "state1")
plot(odenet, select = "state2")
plot(odenet, select = "state1vs2")

# Testweise
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
odenet <- setState(odenet, cbind(c(0, 1, 2), c(1, 2, 1), c(3, NA, NA)), c(0, 0))
odenet <- simuNetwork(odenet, seq(0, 5, by = 0.1))

plot(odenet)
plot(odenet, select = "state1")
plot(odenet, select = "state2")
plot(odenet, select = "state1vs2")

odenet$state
createOscillators(odenet)
createParamVec(odenet)
createState(odenet, -1)

mRes <- getResult(odenet)
tail(mRes)


a <- 1
b <- 3
bquote(a + b)
bquote(a + .(b))
bquote(.(a) + .(b))
bquote(.(a + b))

library(devtools)
library(testthat)

library(BBmisc)
library(deSolve)

load_all("skel", reset = TRUE)

#########################
# 1d Beispiel
#########################
masses <- 1
dampers <- as.matrix(0.5)
springs <- as.matrix(4)

odenet <- ODEnetwork(masses, dampers, springs)

# only state
odenet <- setState(odenet, 3, 0)
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet)
plot(odenet, state = "1vs2")

# events
eventdata <- data.frame(  var = c("x.1", "x.1", "v.1")
                         , time = c(1, 2, 5)
                         , value = c(2, 3, 4)
)
odenet <- setEvents(odenet, eventdata, type = "constant")
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet)

odenet <- setState(odenet, 0, 0)
eventdata <- data.frame(  var = c("x.1", "x.1", "v.1", "v.1")
                          , time = c(1, 2, 3, 4)
                          , value = c(0, 3, 4, 2)
)
eventdata <- data.frame(  var = c("x.1", "x.1", "x.1")
                          , time = c(1, 4, 10)
                          , value = c(0, 3, 3)
)
odenet <- setEvents(odenet, eventdata, type = "linear")
odenet <- simuNetwork(odenet, seq(0, 11, by = 0.1))
plot(odenet)

#########################
# 1d Beispiel: Polar
#########################
masses <- 1
dampers <- as.matrix(0.5)
springs <- as.matrix(4)
odenet <- ODEnetwork(masses, dampers, springs, FALSE)
eventdata <- data.frame(  var = "m.1"
                          , time = seq(0, 2, length.out = 100)
                          , value = seq(2, 2, length.out = 100)
)
eventdata <- rbind(eventdata, 
                   data.frame(  var = "a.1"
                          , time = seq(0, 2, length.out = 100)
                          , value = seq(0, -9/5*pi, length.out = 100)
                   )
)
odenet <- setEvents(odenet, eventdata, type = "linear")
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet, state = "1vs2")
plot(odenet)

#########################
# 2d Beispiel
#########################
masses <- c(1, 2)
dampers <- diag(c(0.02, 0.1))
dampers[1, 2] <- 0.1
springs <- diag(c(4, 1))
springs[1, 2] <- 2

odenet <- ODEnetwork(masses, dampers, springs)

# state only
odenet <- setState(odenet, c(1, 1), c(0, 0))
odenet <- simuNetwork(odenet, seq(0, 40, by = 1))
calcResonances(odenet)
odenet$state
plot(odenet)
plot(odenet, state = "1")
plot(odenet, state = "2")
plot(odenet, state = "1vs2")

# events
eventdata <- data.frame(var = c("v.1")
                        , time = c(0)
                        , value = c(0)
)

eventdata <- data.frame(var = c("x.1", "x.1", "x.1")
                        , time = c(1, 4, 5)
                        , value = c(0, 5, 5)
)
eventdata <- data.frame(var = c("m.2", "m.1", "m.1", "a.1", "m.1", "m.2")
                        , time = c(1, 4, 5, 1, 1, 3)
                        , value = c(0, 5, 5, 2, 2, 9)
)

odenet <- setEvents(odenet, eventdata, type = "linear")
odenet <- simuNetwork(odenet, seq(0, 40, by = 1))
plot(odenet)
plot(odenet, state = "1")
plot(odenet, state = "2")
plot(odenet, state = "1vs2")

#########################
# 2d Beispiel: Polar
#########################
masses <- c(1, 2)
dampers <- diag(c(1.1, 1.5))
dampers[1, 2] <- 0.5
springs <- diag(c(4, 0))
springs[1, 2] <- 6

eventdata <- data.frame(  var = "m.1"
                          , time = seq(0, 5, length.out = 100)
                          , value = seq(2, 2, length.out = 100)
)
eventdata <- rbind(eventdata, 
                   data.frame(  var = "a.1"
                                , time = seq(0, 5, length.out = 100)
                                , value = seq(0, -9/5*pi, length.out = 100)
                   )
)
odenet <- ODEnetwork(masses, dampers, springs, FALSE)
odenet <- setEvents(odenet, eventdata, type = "linear")
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet, state = "1vs2", var = 1)
plot(odenet, var = 1)

#########################
# 5d Beispiel
#########################
masses <- 1:5
dampers <- diag(rep(1, 5))
for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 2+i}
dampers[2, 4] <- 1.5
springs <- diag(11:15)
for (i in 1:(length(masses)-1)) {springs[i, i+1] <- 15+i}
springs[3, 5] <- 13.5
odenet <- ODEnetwork(masses, dampers, springs)
odenet <- setState(odenet, c(rep(0, 4), 5), c(rep(0, 4), 5))
odenet <- setState(odenet, seq(3, 5, length.out=5), seq(1, 3, length.out=5))
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet)

#########################
# 10d Beispiel
#########################
masses <- rep(1, 10)
dampers <- diag(masses)
for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 1}
springs <- dampers

# State
odenet <- ODEnetwork(masses, dampers, springs, FALSE)
odenet <- setState(odenet, c(1, rep(0, 9)), rep(0, 10))
odenet <- simuNetwork(odenet, seq(0, 20, by = 0.1))
plot(odenet, state = "1vs2")

# Events
eventdata <- data.frame(var = c("m.1", "m.1"), time = c(1, 2), value = c(0, 2))
odenet <- ODEnetwork(masses, dampers, springs, FALSE)
odenet <- setEvents(odenet, eventdata, type = "dirac")
odenet <- simuNetwork(odenet, seq(0, 20, by = 0.1))
plot(odenet, state = "1vs2")
plot(odenet, state = "1vs2", var = c(5, -7, 2))

#########################
# Tests
#########################
createState(odenet)
createOscillators(odenet)
createParamVec(odenet)

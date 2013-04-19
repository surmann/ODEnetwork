#########################
# 1d Beispiel
#########################
masses <- 1
dampers <- as.matrix(0.1)
springs <- as.matrix(4)

odenet <- ODEnetwork(masses, dampers, springs)
odenet <- setState(odenet, 3, 0)
odenet <- simuNetwork(odenet, seq(0, 50, by = 0.1))
plot(odenet)

createParamVec(odenet)

#########################
# 2d Beispiel
#########################
masses <- c(1, 2)
dampers <- diag(c(0.1, 0.5))
dampers[1, 2] <- 0.05
springs <- diag(c(4, 10))
springs[1, 2] <- 6

odenet <- ODEnetwork(masses, dampers, springs)
odenet <- setState(odenet, c(3, 3), c(0, 0))
odenet <- simuNetwork(odenet, seq(0, 50, by = 0.1))

createParamVec(odenet)
createState(odenet)
mass <- 1
damper <- as.matrix(0.1)
spring <- as.matrix(4)

odenet <- ODEnetwork(mass, damper, spring)

odenet <- setState(odenet, 3, 0)

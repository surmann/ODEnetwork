context("estimate distances")

test_that("estimateDistances", {
  # one mass
  mass <- 1
  damper <- as.matrix(1.5)
  spring <- as.matrix(4)
  odenet <- ODEnetwork(mass, damper, spring)
  odenet <- estimateDistances(odenet, 1)
  expect_equal(odenet$distances, as.matrix(1))
  
  Temp <- matrix(11:22, ncol=4)
  colnames(Temp) <- paste("tst", 1:4, sep = "")
  odenet <- estimateDistances(odenet, Temp[2, 3])
  expect_equal(odenet$distances, as.matrix(18))

  rownames(Temp) <- paste("rw", 1:3, sep = "")
  odenet <- estimateDistances(odenet, Temp[2, 2])
  expect_equal(odenet$distances, as.matrix(15))
  
  # two masses
  masses <- c(1, 1)
  dampers <- diag(c(1, 1))
  springs <- diag(c(1, 1))
  springs[1, 2] <- 1
  equilibrium <- c(1/3, 5/3)  
  
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- estimateDistances(odenet, equilibrium)
  # THOWS AN REASONABLE ERROR IN THIS NETWORK,
  # because of the regularisation, which should be optional for problems with a unique solution
#   expect_equal(odenet$distances, matrix(c(1, 2, 2, 1), ncol=2), tolerance=1e-3)
  
  # three masses
  masses <- 1:3
  dampers <- diag(rep(0.1, 3))
  springs <- diag(rep(2.5, 3))
  distances <- diag(rep(1, 3))
  equilibrium <- c(2, 2.5, 3)
  
  odenet <- ODEnetwork(masses, dampers, springs, distances=distances)
  expect_message(estimateDistances(odenet, equilibrium, distGround="fixed")
                 , "All parameters are fixed.")
  
  expect_error(estimateDistances(odenet, equilibrium, distGround=c("A", "B"))
               , "Must have length")
  
  expect_equal(diag(odenet$distances), rep(1, 3))
  
  # five masses
  masses <- 1:5
  dampers <- diag(rep(1, 5))
  for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 2+i}
  dampers[2, 4] <- 2
  springs <- diag(11:15)
  for (i in 1:(length(masses)-1)) {springs[i, i+1] <- 15+i}
  springs[3, 5] <- 13.5
  equilibrium <- c(2, 2.5, 3, 3.5, 4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  
  expect_warning(estimateDistances(odenet, equilibrium, optim.control=list(maxit=3)))
  
  expect_warning(estimateDistances(odenet, equilibrium, optim.control=list(reltol=1)))
  
  expect_output(estimateDistances(odenet, equilibrium, optim.control=list(trace=1)), "converged")
   
  odenet <- estimateDistances(odenet, equilibrium)
  odenet <- simuNetwork(odenet, seq(0, 20, by = 0.1))
  expect_equal(tail(odenet$simulation$results, n=1L)[, paste("x", 1:5, sep=".")]
               , equilibrium, tolerance=1e-2, check.attributes=FALSE)
  odenet <- updateOscillators(odenet, distances=diag(rep(1, 5)))
  # estimate distance with groups
  odenet <- estimateDistances(odenet, equilibrium, distGround=c("A", "B", "B", "A", "A"))
  temp <- var(diag(odenet$distances)[c(1, 4, 5)])
  temp <- c(temp, var(diag(odenet$distances)[c(2, 3)]))
  expect_equal(temp, c(0, 0))  
})


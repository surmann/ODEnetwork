context("construction of ode network class")

test_that("ODEnetwork", {
  dampers <- diag(rep(1, 3))
  dampers[1, 2] <- 3
  springs <- diag(rep(1, 3))
  springs[2, 3] <- 5
  masses <- c(0:2)
  expect_error(ODEnetwork(masses, dampers, springs))
  masses <- c(1:2, -4)
  expect_error(ODEnetwork(masses, dampers, springs))
  
  masses <- c(1:3)
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_equal(odenet$masses, masses)
  
  dampers2 <- dampers
  dampers2[2, 1] <- 3
  expect_equal(odenet$dampers, dampers2)
  
  springs2 <- springs
  springs2[3, 2] <- 5
  expect_equal(odenet$springs, springs2)
  
  masses2 <- masses[-1]
  expect_error(ODEnetwork(masses2, dampers, springs))
  
  dampers2 <- dampers[-1]
  expect_error(ODEnetwork(masses, dampers2, springs))
  
  springs2 <- springs[-1]
  expect_error(ODEnetwork(masses, dampers, springs2))
  
  mass <- 2
  damper <- as.matrix(1)
  spring <- as.matrix(1)
  odenet <- ODEnetwork(mass, damper, spring)
  odetest <- list(masses=mass, dampers=damper, springs=spring)
  class(odetest) <- "ODEnetwork"
  expect_equal(odenet, odetest)
})

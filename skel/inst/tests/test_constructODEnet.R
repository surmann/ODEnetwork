context("construction of ode network class")

test_that("ODEnetwork", {
  # masses must be positive, dampers are real, springs non-negative
  masses <- c(1:3)
  dampers <- diag(rep(1, 3))
  springs <- diag(rep(1, 3))
  distances <- diag(2, 3)
  dampers[1, 2] <- -3
  odenet <- ODEnetwork(masses, dampers, springs)
  dampers[1, 2] <- 3
  springs[2, 3] <- -5
  expect_error(ODEnetwork(masses, dampers, springs))
  springs[2, 3] <- 5
  masses <- c(0:2)
  expect_error(ODEnetwork(masses, dampers, springs))
  masses <- c(1:2, -4)
  expect_error(ODEnetwork(masses, dampers, springs))
  distances[1, 3] <- "ja"
  expect_error(ODEnetwork(masses, dampers, springs, distances=distances))
  distances <- diag(2, 3)
  distances[1, 3] <- 5
  
  masses <- c(1:3)
  odenet <- ODEnetwork(masses, dampers, springs, distances=distances)
  expect_equal(odenet$coordtype, "cartesian")
  expect_equal(odenet$masses, masses)
  
  dampers2 <- dampers
  dampers2[2, 1] <- 3
  expect_equal(odenet$dampers, dampers2)
  
  springs2 <- springs
  springs2[3, 2] <- 5
  expect_equal(odenet$springs, springs2)
  
  distances2 <- distances
  distances2[3, 1] <- -5
  expect_equal(odenet$distances, distances2)
  
  masses2 <- masses[-1]
  expect_error(ODEnetwork(masses2, dampers, springs))
  
  dampers2 <- dampers[-1]
  expect_error(ODEnetwork(masses, dampers2, springs))
  
  springs2 <- springs[-1]
  expect_error(ODEnetwork(masses, dampers, springs2))
  
  mass <- 2
  damper <- as.matrix(1)
  spring <- as.matrix(1)
  distance <- as.matrix(0)
  odenet <- ODEnetwork(mass, damper, spring, FALSE)
  expect_equal(odenet$coordtype, "polar")
  odetest <- list(masses=mass, dampers=damper, springs=spring, distances=distance
                  , coordtype="polar", state=cbind(state1=0, state2=0))
  class(odetest) <- "ODEnetwork"
  expect_equal(odenet, odetest)
  
  # Bigger network: test copy of triangle
  masses <- 1:6
  dampers <- diag(11:16)
  for (i in 1:(length(masses)-1)) {
    dampers[i, i+1] <- 15+i
    dampers[i+1, i] <- 15+i
  }
  springs <- dampers + 10
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_true(isSymmetric(odenet$dampers))
  expect_equal(odenet$dampers, dampers)
  expect_true(isSymmetric(odenet$springs))
  expect_equal(odenet$springs, springs)
})

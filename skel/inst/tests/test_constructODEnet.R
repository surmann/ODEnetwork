context("Test construction of ode network class")

test_that("ODEnetwork", {
  masses <- c(1:3)
  dampers <- diag(rep(1, 3))
  dampers[1, 2] <- 3
  springs <- diag(rep(1, 3))
  springs[2, 3] <- 5
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
})

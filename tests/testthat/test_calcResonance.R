context("calculation of resonance frequencies")

test_that("calcResonances", {
  masses <- rep(5, 3)
  dampers <- diag(rep(1, 3))
  dampers[1, 2] <- 3
  springs <- diag((c(1:3)*2*pi)^2*5)
  springs[2, 3] <- 1
  odenet <- ODEnetwork(masses, dampers, springs)
  
  expect_equal(length(calcResonances(odenet)), 1)
  expect_equal(calcResonances(odenet)$cResonances, 1:3)
})

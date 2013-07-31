context("update oscillator parameters")

test_that("updateOscillators", {
  masses <- c(1:5)
  dampers <- diag(-15:-11)
  springs <- diag(21:25)
  odenet <- ODEnetwork(masses, dampers, springs)
  
  # errors in matrix update because of different lenght and sizes
  expect_error(updateOscillators(odenet, masses = c(1:4)))
  expect_error(updateOscillators(odenet, masses = c(1:6)))
  expect_error(updateOscillators(odenet, masses = c(0:4)))

  expect_error(updateOscillators(odenet, dampers = diag(10:15)))
  expect_error(updateOscillators(odenet, dampers = diag(12:15)))
  
  expect_error(updateOscillators(odenet, springs = diag(10:15)))
  expect_error(updateOscillators(odenet, springs = diag(12:15)))
  expect_error(updateOscillators(odenet, springs = diag(-15:-11)))
  
  odenet1 <- updateOscillators(odenet, c(m.2 = 32))
  expect_equal(odenet1$masses[2], 32)
  
  # vector matrix update
  odenet1 <- updateOscillators(odenet, masses = c(3:7))
  expect_equal(odenet1$masses, c(3:7))
  dampers[2, 5] <- 14
  odenet1 <- updateOscillators(odenet, dampers = dampers)
  dampers[5, 2] <- 14
  expect_equal(odenet1$dampers, dampers)
  springs[1, 4] <- 24
  odenet1 <- updateOscillators(odenet, springs = springs)
  springs[4, 1] <- 24
  expect_equal(odenet1$springs, springs)
  
  # Bigger network: test copy of triangle
  odenet <- updateOscillators(odenet, c(d.1.2 = 101, d.3.4 = 102))
  expect_true(isSymmetric(odenet$dampers))
  expect_equal(odenet$dampers[3, 4], 102)
  expect_equal(odenet$dampers[4, 3], 102)
  odenet <- updateOscillators(odenet, c(k.1.2 = 201, k.3.5 = 202))
  expect_true(isSymmetric(odenet$springs))
  expect_equal(odenet$springs[3, 5], 202)
  expect_equal(odenet$springs[5, 3], 202)
})

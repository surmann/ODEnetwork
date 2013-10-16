context("update oscillator parameters")

test_that("updateOscillators", {
  masses <- c(1:5)
  dampers <- diag(-15:-11)
  springs <- diag(21:25)
  distances <- diag(31:35)
  odenet <- ODEnetwork(masses, dampers, springs, distances=distances)
  
  # errors in matrix update because of different lenght and sizes
  expect_error(updateOscillators(odenet, masses = c(1:4)))
  expect_error(updateOscillators(odenet, masses = c(1:6)))
  expect_error(updateOscillators(odenet, masses = c(0:4)))

  expect_error(updateOscillators(odenet, dampers = diag(10:15)))
  expect_error(updateOscillators(odenet, dampers = diag(12:15)))
  
  expect_error(updateOscillators(odenet, springs = diag(10:15)))
  expect_error(updateOscillators(odenet, springs = diag(12:15)))
  expect_error(updateOscillators(odenet, springs = diag(-15:-11)))

  expect_error(updateOscillators(odenet, distances = diag(10:15)))
  expect_error(updateOscillators(odenet, distances = diag(12:15)))
  
  expect_error(updateOscillators(odenet, state1 = rep(10, 4)))
  expect_error(updateOscillators(odenet, state2 = rep(7, 6)))

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
  distances[1, 4] <- 34
  odenet1 <- updateOscillators(odenet, distances = distances)
  distances[4, 1] <- -34
  diag(distances) <- -diag(distances)
  expect_equal(odenet1$distances, distances)
  
  # Bigger network: test copy of triangle
  odenet <- updateOscillators(odenet, c(d.1.2 = 101, d.3.4 = 102))
  expect_true(isSymmetric(odenet$dampers))
  expect_equal(odenet$dampers[3, 4], 102)
  expect_equal(odenet$dampers[4, 3], 102)
  
  odenet <- updateOscillators(odenet, c(k.1.2 = 201, k.3.5 = 202))
  expect_true(isSymmetric(odenet$springs))
  expect_equal(odenet$springs[3, 5], 202)
  expect_equal(odenet$springs[5, 3], 202)
  
  odenet <- updateOscillators(odenet, c(r.1.2 = 301, r.3.5 = 302))
  expect_equal(odenet$distances[3, 5], 302)
  expect_equal(odenet$distances[5, 3], -302)
  
  # update states
  odenet <- updateOscillators(odenet, c(st1.2 = 11, st2.4 = 8))
  expect_equal(as.numeric(odenet$state[2, "state1"]), 11)
  expect_equal(as.numeric(odenet$state[4, "state2"]), 8)
})

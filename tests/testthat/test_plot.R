context("plotting")

test_that("plot", {
  masses <- 1
  dampers <- as.matrix(1.5)
  springs <- as.matrix(4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_error(plot(odenet))
  
  odenet <- setState(odenet, 0, -3)
  expect_error(plot(odenet))

  masses <- rep(1, 10)
  dampers <- diag(masses)
  for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 1}
  springs <- dampers
  
  # define network
  odenet <- ODEnetwork(masses, dampers, springs, FALSE)
  odenet <- setState(odenet, c(1, rep(0, 9)), rep(0, 10))
  odenet <- simuNetwork(odenet, seq(0, 20, by = 0.1))
  # check integerish
  expect_error(plot(odenet, var = 1.1))
  # Argument var must be greater than or equal 1!
  expect_error(plot(odenet, var = 0))
  # Argument var must be less than or equal 10!
  expect_error(plot(odenet, var = 11))
  expect_error(plot(odenet, var = c(3, -5, 7)))
  expect_error(plot(odenet, state = "1vs3"))
  # positive with polar coordinates
  expect_true(plot(odenet, state = "1vs2", var = 2:4, ask = FALSE))
  
  # define network
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, c(1, rep(0, 9)), rep(0, 10))
  odenet <- simuNetwork(odenet, seq(0, 20, by = 0.1))
  
  # positive tests
  expect_true(plot(odenet, ask = FALSE))
  expect_true(plot(odenet, state = "1", var = 1L, ask = FALSE))
  expect_true(plot(odenet, state = "2", var = 2L, ask = FALSE))
  expect_true(plot(odenet, state = "1vs2", var = 3L, ask = FALSE))
  expect_true(plot(odenet, state = "1vs2", var = 2:4, ask = FALSE))
  expect_true(plot(odenet, state = "1vs2", var = 5:10, ask = FALSE))
})

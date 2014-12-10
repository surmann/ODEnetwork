context("plotting")

test_that("plot", {
  masses <- 1
  dampers <- as.matrix(1.5)
  springs <- as.matrix(4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_error(plot(odenet))
  
  odenet <- setState(odenet, 0, -3)
  expect_error(plot(odenet))

  # test not applicable, because a graph is plotted
#   odenet <- ODEnetwork(masses, dampers, springs, FALSE)
#   odenet <- setState(odenet, -3, 0.5*pi)
#   odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
#   plot(odenet, state = "1vs2")

  masses <- rep(1, 10)
  dampers <- diag(masses)
  for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 1}
  springs <- dampers
  
  # State
  odenet <- ODEnetwork(masses, dampers, springs, FALSE)
  odenet <- setState(odenet, c(1, rep(0, 9)), rep(0, 10))
  odenet <- simuNetwork(odenet, seq(0, 20, by = 0.1))
  expect_error(plot(odenet, state = "1vs2", var = 0)
               , "Argument var must be greater than or equal 1!")
  expect_error(plot(odenet, state = "1vs2", var = 11)
               , "Argument var must be less than or equal 10!")
#   expect_error(plot(odenet, state = "1vs2", var = c(3, -5, 7))
#                , "Argument var must be greater than or equal 1!")
#   expect_error(plot(odenet, state = "1vs2", var = c(3, 15, 7))
#                , "Argument var must be less than or equal 10!")
})

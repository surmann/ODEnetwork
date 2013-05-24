context("plotting")

test_that("plot", {
  masses <- 1
  dampers <- as.matrix(1.5)
  springs <- as.matrix(4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_error(plot(odenet))
  
  odenet <- setState(odenet, 0, -3)
  expect_error(plot(odenet))

  odenet <- ODEnetwork(masses, dampers, springs, FALSE)
  odenet <- setState(odenet, -3, 0.5*pi)
  odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
  # test not applicable, because a graph is plotted
#   plot(odenet, select = "state1vs2")
})

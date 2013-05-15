context("plotting")

test_that("plot", {
  masses <- 1
  dampers <- as.matrix(1.5)
  springs <- as.matrix(4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_error(plot(odenet))
  
  odenet <- setState(odenet, 0, -3)
  expect_error(plot(odenet))
})

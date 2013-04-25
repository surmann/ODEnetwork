context("get result")

test_that("getResult", {
  masses <- 1
  dampers <- as.matrix(1.5)
  springs <- as.matrix(4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, 0, -3)
  odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
  
  expect_equal(odenet$simulation$results, getResult(odenet))
})

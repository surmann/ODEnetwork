context("simulate ode network")

test_that("simuNetwork", {
  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, 0, 0)
  odenet <- simuNetwork(odenet, seq(0, 4, by = 1))
  
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=seq(0, 4, by = 1), x.1=rep(0, 5), v.1=rep(0, 5))
  attr(mResManual, "istate") <- c(0, 4, 17, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, 4, NA, NA, NA)
  attr(mResManual, "lengthvar") <- 2
  attr(mResManual, "class") <- c("deSolve", "matrix")
  attr(mResManual, "type") <- "rk"
  expect_equal(mRes, mResManual)
})

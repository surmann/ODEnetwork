context("simulate ode network")

test_that("simuNetwork", {
  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)

  odenet <- simuNetwork(odenet, seq(0, 4, by = 1))
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=0:4, x.1=rep(0, 5), v.1=rep(0, 5))
  attr(mResManual, "istate") <- c(2, 6, 7, NA, 2, 2, 0, 52, 22, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA)
  attr(mResManual, "rstate") <- c(1, 1, 4.002, 0, 0)
  attr(mResManual, "lengthvar") <- 2
  attr(mResManual, "class") <- c("deSolve", "matrix")
  attr(mResManual, "type") <- "lsoda"
  expect_equal(mRes, mResManual)

  odenet <- simuNetwork(odenet, 0:4, method = "rk4")
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=seq(0, 4, by = 1), x.1=rep(0, 5), v.1=rep(0, 5))
  attr(mResManual, "istate") <- c(0, 4, 17, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, 4, NA, NA, NA)
  attr(mResManual, "lengthvar") <- 2
  attr(mResManual, "class") <- c("deSolve", "matrix")
  attr(mResManual, "type") <- "rk"
  expect_equal(mRes, mResManual)
  
  odenet <- ODEnetwork(masses, dampers, springs, FALSE)
  odenet <- simuNetwork(odenet, seq(0, 4, by = 1))
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=0:4, m.1=rep(0, 5), a.1=rep(0, 5))
  attr(mResManual, "istate") <- c(2, 6, 7, NA, 2, 2, 0, 52, 22, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA)
  attr(mResManual, "rstate") <- c(1, 1, 4.002, 0, 0)
  attr(mResManual, "lengthvar") <- 2
  attr(mResManual, "class") <- c("deSolve", "matrix")
  attr(mResManual, "type") <- "lsoda"
  expect_equal(mRes, mResManual)
})

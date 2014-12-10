context("create jacobian matrix")

test_that("createJacobian", {
  masses <- 1:5
  dampers <- diag(rep(1, 5))
  for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 2+i}
  dampers[2, 4] <- 1.5
  springs <- diag(11:15)
  for (i in 1:(length(masses)-1)) {springs[i, i+1] <- 15+i}
  springs[3, 5] <- 13.5
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, seq(3, 5, length.out=5), seq(1, 3, length.out=5))
  jac <- createJacobian(odenet)
  # check dimension
  expect_equal(1, prod(dim(jac) == c(2*5, 4*5)))
})

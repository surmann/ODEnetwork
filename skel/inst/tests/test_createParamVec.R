context("creating parameter vector")

test_that("createParamVec", {
  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)
  cPar <- createParamVec(odenet)
  
  expect_is(cPar, "numeric")
  expect_equal(cPar, c(m.1=1, d.1=0.1, k.1=4))
  
  masses <- c(1, 2)
  dampers <- diag(c(0.1, 0.5))
  dampers[1, 2] <- 0.05
  springs <- diag(c(4, 10))
  springs[1, 2] <- 6
  odenet <- ODEnetwork(masses, dampers, springs)
  cPar <- createParamVec(odenet)
  
  expect_equal(cPar, c(m.1=1, d.1=0.1, k.1=4, d.1.2=0.05, k.1.2=6,
                       m.2=2, d.2=0.5, k.2=10, d.2.1=0.05, k.2.1=6))
})

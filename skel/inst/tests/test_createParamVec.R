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
  
  masses <- 1:5
  dampers <- diag(11:15)
  for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 15+i}
  dampers[2, 4] <- 16.2
  springs <- diag(21:25)
  for (i in 1:(length(masses)-1)) {springs[i, i+1] <- 25+i}
  springs[3, 5] <- 23.5
  odenet <- ODEnetwork(masses, dampers, springs)
  expect_true(is.vector(createParamVec(odenet)))
})

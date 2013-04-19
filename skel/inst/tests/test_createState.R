context("creating state vector")

test_that("createState", {
  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)

  odenet <- setState(odenet, 3, 0)
  cState <- createState(odenet)
  expect_is(cState, "numeric")
  expect_equal(cState, c(x.1=3, v.1=0))
  
  masses <- c(1, 2)
  dampers <- diag(c(0.1, 0.5))
  dampers[1, 2] <- 0.05
  springs <- diag(c(4, 10))
  springs[1, 2] <- 6
  odenet <- ODEnetwork(masses, dampers, springs)
  
  expect_error(setState(odenet, 2, 1))
  
  odenet <- setState(odenet, c(3, 5), c(2, 0))
  cState <- createState(odenet)
  
  expect_equal(cState, c(x.1=3, v.1=2, x.2=5, v.2=0))

  odenet <- setState(odenet, c(3, 5), c(2, 0), FALSE)
  expect_error(createState(odenet), "Missing convert to euclidian coordinates")
})

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
  
  # Tests with timepoints
  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)

  odenet <- setState(odenet, cbind(time=c(0, 1, 2), x=c(0, 1, 0)), 3)
  expect_equal(createState(odenet, 1), c(x.1=1, v.1=NA))
  expect_equal(createState(odenet, 0.5), c(x.1=0, v.1=NA))
  expect_equal(createState(odenet, 1.5), c(x.1=1, v.1=NA))
  expect_equal(createState(odenet, 2.5), c(x.1=NA, v.1=NA))

  odenet <- setState(odenet, 5, cbind(c(0, 1, 2), c(1, 2, -1)))
  expect_equal(createState(odenet, 1), c(x.1=NA, v.1=2))
  expect_equal(createState(odenet, 1.5), c(x.1=NA, v.1=2))
  expect_equal(createState(odenet, 2.5), c(x.1=NA, v.1=NA))

  masses <- c(1, 2)
  dampers <- diag(c(0.1, 0.5))
  springs <- diag(c(4, 10))
  odenet <- ODEnetwork(masses, dampers, springs)

  odenet <- setState(odenet, cbind(time=c(0, 1, 2, 3), x=c(0, 1, 0, 2), c(5, 2, 1, NA)), c(3, 1))
  expect_equal(createState(odenet, 1), c(x.1=1, v.1=NA, x.2=2, v.2=NA))
  expect_equal(createState(odenet, 0.5), c(x.1=0, v.1=NA, x.2=5, v.2=NA))
  expect_equal(createState(odenet, 2.5), c(x.1=0, v.1=NA, x.2=NA, v.2=NA))
  expect_equal(createState(odenet, 3), c(x.1=2, v.1=NA, x.2=NA, v.2=NA))
  expect_equal(createState(odenet, 3.5), c(x.1=NA, v.1=NA, x.2=NA, v.2=NA))
  
  odenet <- setState(odenet, c(5, 1), cbind(c(0, 1, 2), c(1, 2, -1), c(8, 9, 12)))
  expect_equal(createState(odenet, 0.5), c(x.1=NA, v.1=1, x.2=NA, v.2=8))
  expect_equal(createState(odenet, 1), c(x.1=NA, v.1=2, x.2=NA, v.2=9))
  expect_equal(createState(odenet, 2), c(x.1=NA, v.1=-1, x.2=NA, v.2=12))
  expect_equal(createState(odenet, 2.5), c(x.1=NA, v.1=NA, x.2=NA, v.2=NA))
  
  odenet <- setState(odenet, c(5, 1), cbind(c(0, 1, 2, 3), c(1, 2, -1, NA), c(8, 9, 12, 14)))
  expect_equal(createState(odenet, 1.5), c(x.1=NA, v.1=2, x.2=NA, v.2=9))
  expect_equal(createState(odenet, 2.5), c(x.1=NA, v.1=NA, x.2=NA, v.2=12))
  expect_equal(createState(odenet, 3), c(x.1=NA, v.1=NA, x.2=NA, v.2=14))
  expect_equal(createState(odenet, 3.5), c(x.1=NA, v.1=NA, x.2=NA, v.2=NA))
})

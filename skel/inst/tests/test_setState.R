context("setting start state")

test_that("setState", {
  mass <- rep(1, 2)
  damper <- diag(2, 2)
  spring <- diag(3, 2)
  odenet <- ODEnetwork(mass, damper, spring)
  
  expect_error(setState(odenet, 10, 20:21))
  expect_error(setState(odenet, 20:21, 10))
  expect_error(setState(odenet, 10:11, 20:21, 5))
  
  odenet <- setState(odenet, 10:11, 20:21)
  expect_equal(odenet$coordtype, "euclidian")
  
  odenet <- setState(odenet, 10:11, 20:21, FALSE)
  expect_equal(odenet$coordtype, "polar")
  
  expect_equal(odenet$state$one, 10:11)
  expect_equal(odenet$state$two, 20:21)
})

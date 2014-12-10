context("convert coordinates")

test_that("convertCoordinates", {
  expect_error(convertCoordinates(matrix(1:9, ncol = 3)))
  expect_error(convertCoordinates(matrix(1:9, ncol = 1)))
  expect_error(convertCoordinates(1:9))
  
  coordsK <- rbind(c(3, 0), c(1, 3), c(0, 2), c(-3, 1), c(-1, 0), c(-1, -3), c(0, -2), c(2, -3))
  colnames(coordsK) <- c("x", "y")

  expect_error(convertCoordinates(coordsK, "3d"))
  
  coordsP <- convertCoordinates(coordsK, "polar")
  expect_equal(coordsP[c(1, 3, 5, 7), ], cbind(r = c(3, 2, 1, 2), phi = c(0, 0.5, 1, 1.5)*pi))
  
  coordsK1 <- convertCoordinates(coordsP)
  expect_equal(coordsK, coordsK1)
})

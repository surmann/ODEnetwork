context("setting events")

test_that("setState", {
  mass <- 1
  damper <- as.matrix(1.5)
  spring <- as.matrix(4)
  odenet <- ODEnetwork(mass, damper, spring)
    
  expect_error(setEvents(odenet, c(1:5)))
  
  eventdat <- data.frame(  var = c("x.1")
                         , time = c(0.5)
                         , value = c(3)
                        )
  expect_error(setEvents(odenet, eventdat, "foo"))
  
  odenet <- setEvents(odenet, eventdat)
  expect_equal(odenet$events$type, "dirac")
  expect_equal(odenet$events$data, cbind(eventdat, method = rep("rep", 1)))
})

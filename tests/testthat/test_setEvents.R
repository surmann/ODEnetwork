context("setting events")

test_that("setEvents", {
  mass <- 1
  damper <- as.matrix(1.5)
  spring <- as.matrix(4)
  odenet <- ODEnetwork(mass, damper, spring)
    
  expect_error(setEvents(odenet, c(1:5)))
  
  eventdata <- data.frame(  var = c("x.1")
                          , time = c(0.5)
                          , value = c(3)
                          , stringsAsFactors = TRUE
  )
  expect_error(setEvents(odenet, eventdata, "foo"))
  
  odenet <- setEvents(odenet, eventdata)
  expect_false(is.null(odenet$events$data))
  expect_true(is.null(odenet$events$zeroderiv))
  expect_true(is.null(odenet$events$linear$complete))
  expect_equal(odenet$events$type, "dirac")
  expect_equal(odenet$events$data, cbind(eventdata, method = rep("rep", 1)))
  
  eventdata <- data.frame(  var = c("x.1", "x.1", "v.1")
                            , time = c(1, 2, 5)
                            , value = c(2, 3, 4)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "constant")
  expect_equal(odenet$events$type, "constant")
  expect_true(is.null(odenet$events$zeroderiv))
  expect_true(is.null(odenet$events$linear$complete))
  
  eventdata <- data.frame(  var = c("x.1", "x.1", "x.1")
                            , time = c(1, 2, 10)
                            , value = c(0, 3, 3)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "linear")
  expect_equal(odenet$events$type, "linear")
  expect_false(is.null(odenet$events$data))
  expect_true(is.null(odenet$events$zeroderiv))
  expect_true(is.null(odenet$events$linear$complete))
  
  eventdata <- data.frame(  var = c("x.1", "x.1", "x.2")
                            , time = c(1, 2, 10)
                            , value = c(0, 3, 3)
                            , stringsAsFactors = TRUE
  )
  expect_error(setEvents(odenet, eventdata))
})

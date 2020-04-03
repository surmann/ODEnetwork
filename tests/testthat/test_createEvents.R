context("creating events functions")

test_that("createEvents", {
  mass <- 1
  damper <- as.matrix(1.5)
  spring <- as.matrix(4)
  odenet <- ODEnetwork(mass, damper, spring)
  eventdata <- data.frame(  var = c("x.1")
                            , time = c(0.5)
                            , value = c(3)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata)
  
  odenet <- createEvents(odenet)
  expect_false(is.null(odenet$events$data))
  expect_true(is.null(odenet$events$zeroderiv))
  expect_equal(odenet$events$type, "dirac")
  expect_equal(odenet$events$data, cbind(eventdata, method = rep("rep", 1)))
  
  eventdata <- data.frame(  var = c("x.1", "x.1", "v.1")
                            , time = c(1, 2, 5)
                            , value = c(2, 3, 4)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "constant")
  odenet <- createEvents(odenet)
  expect_false(is.null(odenet$events$zeroderiv))
  
  eventdata <- data.frame(  var = c("x.1", "x.1", "x.1")
                            , time = c(1, 2, 10)
                            , value = c(0, 3, 3)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "linear")
  odenet <- createEvents(odenet)
  expect_false(is.null(odenet$events$data))
  expect_true(is.null(odenet$events$zeroderiv))
  expect_false(is.null(odenet$events$linear$complete))
  expect_false(is.null(odenet$events$linear$x.1))
  
  eventdata <- data.frame(  var = c("x.1", "x.1", "v.1", "v.1")
                            , time = c(1, 2, 3, 4)
                            , value = c(0, 3, 4, 2)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "linear")
  odenet <- createEvents(odenet)
  expect_false(is.null(odenet$events$data))
  expect_true(is.null(odenet$events$zeroderiv))
  expect_false(is.null(odenet$events$linear$complete))
  expect_false(is.null(odenet$events$linear$x.1))
  expect_false(is.null(odenet$events$linear$v.1))

  odenet <- ODEnetwork(mass, damper, spring, FALSE)
  eventdata <- data.frame(  var = c("m.1", "m.1", "a.1", "a.1")
                            , time = c(1, 2, 3, 4)
                            , value = c(0, 3, 4, 2)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "linear")
  expect_error(createEvents(odenet))

  eventdata <- data.frame(  var = c("m.1", "m.1", "a.1", "a.1")
                            , time = c(1, 2, 1, 2)
                            , value = c(0, 3, 4, 2)
                            , stringsAsFactors = TRUE
  )
  odenet <- setEvents(odenet, eventdata, type = "linear")
  odenet <- createEvents(odenet)
  expect_false(is.null(odenet$events$linear$x.1))
  expect_false(is.null(odenet$events$linear$v.1))
})

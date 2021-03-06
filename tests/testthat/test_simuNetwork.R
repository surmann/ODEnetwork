context("simulate ode network")

test_that("simuNetwork", {
  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)

  eventdata <- data.frame(var = c("v.1"), time = c(0), value = c(0))
  odenet <- setEvents(odenet, eventdata)
  # test numeric results
  odenet <- simuNetwork(odenet, seq(0, 4, by = 1))
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=0:4, x.1=rep(0, 5), v.1=rep(0, 5))
  mostattributes(mResManual) <- attributes(mRes)
  expect_equal(mRes, mResManual, check.attributes = FALSE)
  expect_equal(odenet$simulation$method, "lsoda")

  odenet <- simuNetwork(odenet, 0:4, method = "rk4")
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=seq(0, 4, by = 1), x.1=rep(0, 5), v.1=rep(0, 5))
  mostattributes(mResManual) <- attributes(mRes)
  expect_equal(mRes, mResManual, check.attributes = FALSE)
  expect_equal(odenet$simulation$method, "rk")
  
  odenet <- ODEnetwork(masses, dampers, springs, FALSE)
  eventdata <- data.frame(var = c("m.1", "a.1"), time = c(0, 0), value = c(0, 0))
  odenet <- setEvents(odenet, eventdata, type = "linear")
  odenet <- simuNetwork(odenet, seq(0, 4, by = 1))
  mRes <- odenet$simulation$results
  mResManual <- cbind(time=0:4, m.1=rep(0, 5), a.1=rep(0, 5))
  mostattributes(mResManual) <- attributes(mRes)
  expect_equal(mRes, mResManual, check.attributes = FALSE)
  expect_equal(odenet$simulation$method, "lsoda")

  odenet <- ODEnetwork(masses, dampers, springs, FALSE)
  eventdata <- data.frame(var = c("m.1", "m.1"), time = c(1, 2), value = c(5, 2))
  odenet <- setEvents(odenet, eventdata, type = "dirac")
  expect_warning(odenet <- simuNetwork(odenet, seq(0, 4, by = 1)))
})

test_that("simuNetwork calc vs. simu", {
  # compare analytic to numeric results
  tol = .Machine$double.eps ^ 0.5 * 10^2
  # 1 node
  masses <- 0.1
  dampers <- as.matrix(0)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, 1, 0)
  odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
  
  # 2 nodes
  masses <- c(1, 2)
  dampers <- diag(c(0.02, 0.1))
  dampers[1, 2] <- 0.1
  springs <- diag(c(4, 1))
  springs[1, 2] <- 2
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, c(1, 1), c(0, 0))

  odenetAna <- simuNetwork(odenet, seq(0, 20, by = 0.01))
  expect_equal(odenetAna$simulation$method, "analytic")
  
  eventdata <- data.frame(var = c("v.1"), time = c(0), value = c(0))
  odenetNum <- setEvents(odenet, eventdata)
  odenetNum <- simuNetwork(odenetNum, seq(0, 20, by = 0.01))
  expect_equal(odenetNum$simulation$method, "lsoda")
  
  expect_equal(odenetAna$simulation$results, odenetNum$simulation$results
               , tolerance = tol, check.attributes = FALSE)
  
  # 2 nodes with distances
  masses <- c(1, 1)
  dampers <- diag(c(0.5, 0.5))
  springs <- diag(c(1, 1))
  springs[1, 2] <- 5
  distances <- diag(c(0, 2))
  distances[1, 2] <- 1
  odenet <- ODEnetwork(masses, dampers, springs, distances=distances)
  odenet <- setState(odenet, c(0.5, 1), c(0, 0))
  
  # analytisch
  odenetAna <- simuNetwork(odenet, seq(0, 20, by = 0.01))
  expect_equal(odenetAna$simulation$method, "analytic")
  
  # nummerisch
  eventdata <- data.frame(var = c("v.1"), time = c(0), value = c(0))
  odenetNum <- setEvents(odenet, eventdata, type = "dirac")
  odenetNum <- simuNetwork(odenetNum, seq(0, 20, by = 0.01))
  expect_equal(odenetNum$simulation$method, "lsoda")
  
  expect_equal(odenetAna$simulation$results, odenetNum$simulation$results
               , tolerance = tol, check.attributes = FALSE)
  
  # 5 nodes
  masses <- 1:5
  dampers <- diag(rep(1, 5))
  for (i in 1:(length(masses)-1)) {dampers[i, i+1] <- 2+i}
  dampers[2, 4] <- 1.5
  springs <- diag(11:15)
  for (i in 1:(length(masses)-1)) {springs[i, i+1] <- 15+i}
  springs[3, 5] <- 13.5
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, c(rep(0, 4), 5), c(rep(0, 4), 5))
  
  odenetAna <- simuNetwork(odenet, seq(0, 10, by = 0.01))

  eventdata <- data.frame(var = c("v.1"), time = c(0), value = c(0))
  odenetNum <- setEvents(odenet, eventdata)
  odenetNum <- simuNetwork(odenetNum, seq(0, 10, by = 0.01))
  
  expect_equal(odenetAna$simulation$results, odenetNum$simulation$results
               , tolerance = tol, check.attributes = FALSE)
})

test_that("simuNetwork time origins", {
  # compare analytic to numeric results
  tol = .Machine$double.eps ^ 0.5 * 10^2

  masses <- 1
  dampers <- as.matrix(0.1)
  springs <- as.matrix(4)
  odenet <- ODEnetwork(masses, dampers, springs)
  odenet <- setState(odenet, 1, 0)
  
  odenet.zero <- simuNetwork(odenet, seq(0, 10, by = 0.1))
  odenet.min <- simuNetwork(odenet, seq(10, 20, by = 0.1), origin.min.time = TRUE)
  
  expect_equal(odenet.zero$simulation$results[, "time"], seq(0, 10, by = 0.1))
  expect_equal(odenet.min$simulation$results[, "time"], seq(10, 20, by = 0.1))
  expect_equal(odenet.zero$simulation$results[, "x.1"], odenet.min$simulation$results[, "x.1"]
               , tolerance = tol
               )
  
  # update to higher accuracy
  odenet.min <- simuNetwork(odenet, seq(10, 20, by = 0.01), origin.min.time = TRUE)
  eventdata <- data.frame(var = c("v.1"), time = c(0), value = c(0))
  odenet <- setEvents(odenet, eventdata)
  
  odenet.min.num <- simuNetwork(odenet, seq(10, 20, by = 0.01), origin.min.time = TRUE)

  expect_equal(odenet.min$simulation$results, odenet.min.num$simulation$results
               , tolerance = tol, check.attributes = FALSE
               )
})

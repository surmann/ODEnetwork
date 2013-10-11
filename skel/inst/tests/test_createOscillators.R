context("creating oscillators function")

test_that("createOscillators", {
  mass <- 1
  damper <- as.matrix(1)
  spring <- as.matrix(1)
  odenet <- ODEnetwork(mass, damper, spring)
  
  funOscillators <- createOscillators(odenet)
  funOsci <- function (cTime, cState, cParameters) {
    with(as.list(c(cState, cParameters)), {
      dx.1 <- v.1
      dv.1 <- (-d.1 * v.1 - k.1 * x.1)/m.1
      list(c(dx.1, dv.1))
    })
  }
  expect_equal(funOscillators, funOsci)

  distances <- as.matrix(2)
  odenet <- ODEnetwork(mass, damper, spring, distances=distances)

  funOscillators <- createOscillators(odenet)
  funOsci <- function (cTime, cState, cParameters) {
    with(as.list(c(cState, cParameters)), {
      dx.1 <- v.1
      dv.1 <- (-d.1 * v.1 - k.1 * (x.1 - r.1))/m.1
      list(c(dx.1, dv.1))
    })
  }
  expect_equal(funOscillators, funOsci)

  mass <- c(1, 1)
  damper <- diag(c(2, 2))
  damper[1, 2] <- 2
  spring <- diag(c(3, 3))
  spring[1, 2] <- 3
  odenet <- ODEnetwork(mass, damper, spring)
  
  funOscillators <- createOscillators(odenet)
  funOsci <- function (cTime, cState, cParameters) {
    with(as.list(c(cState, cParameters)), {
      dx.1 <- v.1
      dv.1 <- (-d.1 * v.1 - k.1 * x.1 - d.1.2 * (v.1 - v.2) - k.1.2 * (x.1 - x.2))/m.1
      dx.2 <- v.2
      dv.2 <- (-d.2 * v.2 - k.2 * x.2 - d.2.1 * (v.2 - v.1) - k.2.1 * (x.2 - x.1))/m.2
      list(c(dx.1, dv.1, dx.2, dv.2))
    })
  }
  expect_equal(funOscillators, funOsci)
  
  mass <- c(1, 1)
  damper <- diag(c(2, 2))
  damper[1, 2] <- 2
  spring <- diag(c(3, 3))
  spring[1, 2] <- 3
  distances <- diag(c(4, 4))
  distances[1, 2] <- 4
  odenet <- ODEnetwork(mass, damper, spring, distances=distances)
  
  funOscillators <- createOscillators(odenet)
  funOsci <- function (cTime, cState, cParameters) {
    with(as.list(c(cState, cParameters)), {
      dx.1 <- v.1
      dv.1 <- (-d.1 * v.1 - k.1 * (x.1 - r.1) - d.1.2 * (v.1 - v.2) - k.1.2 * (x.1 - x.2 + r.1.2))/m.1
      dx.2 <- v.2
      dv.2 <- (-d.2 * v.2 - k.2 * (x.2 - r.2) - d.2.1 * (v.2 - v.1) - k.2.1 * (x.2 - x.1 + r.2.1))/m.2
      list(c(dx.1, dv.1, dx.2, dv.2))
    })
  }
  expect_equal(funOscillators, funOsci)
})

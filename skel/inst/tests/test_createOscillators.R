context("creating oscillators function")

test_that("createOscillators", {
  mass <- 1
  damper <- as.matrix(1)
  spring <- as.matrix(1)
  odenet <- ODEnetwork(mass, damper, spring)
  odenet <- createOscillators(odenet)
  funOsci <- function (cTime, cState, cParameters) {
    with(as.list(c(cState, cParameters)), {
      dx.1 <- v.1
      dv.1 <- (-d.1 * v.1 - k.1 * x.1)/m.1
      list(c(dx.1, dv.1))
    })
  }
  expect_equal(odenet$funOscillators, funOsci)
})

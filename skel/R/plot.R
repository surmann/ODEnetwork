#' Plots Results of ODEnetwork
#' 
#' Plots the results of \code{simuNetwork} of the given \code{\link{ODEnetwork}}
#' in different ways.
#'
#' @param x [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param select [\code{character}]\cr
#'    The type of result, that is plotted.
#'    If \code{state1}, only state1 (position or angle) is plotted over time.
#'    If \code{state2}, only state2 (velocity or magnitude) is plotted over time.
#'    If \code{state12}, state1 and state2 are plotted over time.
#'    If \code{state1vs2}, state2 is plotted over state1.
#'    Default is \code{state12}
#' @param ... Additional arguments.
#' @export
#' @examples
#' masses <- c(1, 2)
#' dampers <- diag(c(0.1, 0.5))
#' dampers[1, 2] <- 0.05
#' springs <- diag(c(4, 10))
#' springs[1, 2] <- 6
#' odenet <- ODEnetwork(masses, dampers, springs)
#' odenet <- setState(odenet, c(1, 3), c(0, 0))
#' odenet <- simuNetwork(odenet, seq(0, 10, by = 0.05))
#' plot(odenet)
#' plot(odenet, select = "state1")
#' plot(odenet, select = "state2")
#' plot(odenet, select = "state1vs2")
plot.ODEnetwork <- function(x, ..., select = "state12") {
  checkArg(select, "character", len=1, na.ok=FALSE)
  # Check ode result
  if (is.null(x$simulation$results))
    stop("Simulation results missing!")
  # Read ode result
  mRes <- x$simulation$results
  switch(select
         , "state12" = {
           op <- par(no.readonly = TRUE)
           plot(mRes, ...)
           par(op)
         }
         , "state1" = {
           classes <- class(mRes)
           mRes <- mRes[, c(1, seq(2, ncol(mRes), by=2))]
           attr(mRes, "class") <- classes
           op <- par(no.readonly = TRUE)
           plot(mRes, ...)
           par(op)
         }
         , "state2" = {
           classes <- class(mRes)
           mRes <- mRes[, c(1, seq(3, ncol(mRes), by=2))]
           attr(mRes, "class") <- classes
           op <- par(no.readonly = TRUE)
           plot(mRes, ...)
           par(op)
         }
         , "state1vs2" = {
           # calculate plot size
           intVars <- ncol(mRes)-1
           intRows <- intCols <- floor(sqrt(intVars/2))
           if (intCols*intRows < intVars/2) {
             intCols <- intCols + 1
             if (intCols*intRows < intVars/2)
               intRows <- intRows + 1
           }

           op <- par(mfrow = c(intRows, intCols))
           if (odenet$coordtype == "cartesian") {
             for(intVar in seq(2, intVars, by=2)) {
               # plot x vs. v
               plot(  mRes[, intVar], mRes[, intVar+1], type = "l"
                    , xlab = colnames(mRes)[intVar], ylab = colnames(mRes)[intVar+1], ...)
               # plot starting point
               points(mRes[1, intVar], mRes[1, intVar+1], col = "red", pch = 13)
             }
           } else {
             for(intVar in seq(2, intVars, by=2)) {
               # plot m(agnitude) vs. a(ngle)
               radial.plot(  mRes[, intVar], mRes[, intVar+1]
                           , rp.type = "p", xlab = paste("Oscillator", intVar/2)
#                            , labels = expression(0, frac(1,2)*pi, pi, frac(3,2)*pi)
                           , labels = c("0", "1/2*pi", "pi", "3/2*pi")
                           , label.pos = c(0, pi/2, pi, 3/2*pi)
                           , show.grid.labels = 3
                           , radial.lim = c(0, max(mRes[, intVar]))
                           , grid.col = "lightgray"
                           , point.symbols = 13, point.col = "red", poly.col = "NA"
                           , ...)
               radial.plot(  mRes[1, intVar], mRes[1, intVar+1]
                           , rp.type = "s"
                           , radial.lim = c(0, max(mRes[, intVar]))
                           , point.symbols = 13, point.col = "red", poly.col = "NA"
                           , add = TRUE)
             }
           }
           par(op)
         }
         , stop("Wrong option of argument 'select'!")
  )
}

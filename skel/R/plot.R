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
  checkArg(select, subset = c("state12", "state1", "state2", "state1vs2"))
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
            crclSteps <- seq(0, 2*pi, length.out = 400)
            for(intVar in seq(2, intVars, by=2)) {
              circles <- pretty(c(0, max(mRes[, intVar])))
              plotrange <- c(-max(circles), max(circles))
              mCartesian <- convertCoordinates(mRes[, c(intVar, intVar+1)])
              # plot area
              plot(1, type = "n"
                   , xlim = plotrange, ylim = plotrange
                   , xlab = paste("Oscillator", intVar/2), ylab = ""
                   , axes = FALSE, frame.plot = FALSE, asp = 1
              )
              # add axis and label
              axis(2
                   , at = sort(union(-circles, circles))
                   , labels = FALSE
                   , pos = -1.2 * max(circles)
                   )
              text(-1.22 * max(circles), sort(union(-circles, circles))
                   , labels = sort(union(-circles, circles))
                   , xpd = TRUE, pos = 2)
              text(1.22 * max(circles), 0, labels = "Magnitude", xpd = TRUE, srt = 270)
              # draw circle lines
              for (i in circles) {
                lines(i*cbind(cos(crclSteps), sin(crclSteps)), col = gray(0.9))
              }
              # add grid
              axpos <- c(0, pi/2, pi, 3/2*pi)
              axlab <- expression(0
                                  , textstyle(frac(1,2))*pi
                                  , pi
                                  , textstyle(frac(3,2))*pi)
              for (i in 1:length(axpos)) {
                coordpair <- matrix(c(max(circles), axpos[i]), ncol = 2)
                coordpair <- as.vector(convertCoordinates(coordpair))
                segments(0, 0, coordpair[1], coordpair[2], col = gray(0.9))
                text(coordpair[1]*1.1, coordpair[2]*1.1, axlab[i], xpd = TRUE)
              }
              # plot data
              lines(mCartesian, ...)
              # plot starting point
              points(mCartesian[1, 1], mCartesian[1, 2], col = "red", pch = 13)
            }
          }
          par(op)
        }
  )
}

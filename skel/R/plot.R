#' Plots Results of ODEnetwork
#' 
#' Plots the results of \code{simuNetwork} of the given \code{\link{ODEnetwork}}
#' in different ways.
#'
#' @param odenetwork [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param select [\code{character}]\cr
#'    The type of result, that is plotted.
#'    If \code{state1}, only state1 (position or angle) is plotted over time.
#'    If \code{state2}, only state2 (velocity or magnitude) is plotted over time.
#'    If \code{state12}, state1 and state2 are plotted over time.
#'    If \code{state1vs2}, state2 is plotted over state1.
#'    Default is \code{state12}
#' @export
#' @rd
#' @examples
#' plot(odenetwork)
plot.ODEnetwork <- function(odenet, select = "state12") {
  # check arguments
  checkArg(select, "character", len=1, na.ok=FALSE)
  # Read ode result
  mRes <- odenet$simulation$results
  switch(select
         , "state12" = {
           plot(mRes)
         }
         , "state1" = {
           classes <- class(mRes)
           mRes <- mRes[, c(1, seq(2, ncol(mRes), by=2))]
           attr(mRes, "class") <- classes
           plot(mRes)
         }
         , "state2" = {
           classes <- class(mRes)
           mRes <- mRes[, c(1, seq(3, ncol(mRes), by=2))]
           attr(mRes, "class") <- classes
           plot(mRes)
         }
         , "state1vs2" = {
           # calculate plot size
           intVars <- ncol(mRes)-1
           intRows <- intCols <- floor(sqrt(intVars/2))
           intCols <- intCols + 1
           if (intCols*intRows < intVars/2)
             intRows <- intRows + 1

           op <- par(mfrow = c(intRows, intCols))
           for(intVar in seq(2, intVars, by=2)) {
             # plot x vs. v
             plot(  mRes[, intVar], mRes[, intVar+1], type = "l"
                  , xlab = colnames(mRes)[intVar], ylab = colnames(mRes)[intVar+1])
             # plot starting point
             points(mRes[1, intVar], mRes[1, intVar+1], col = "red", pch = 13)
           }
           par(op)
         }
         , stop("Wrong option of argument 'select'!")
  )
}

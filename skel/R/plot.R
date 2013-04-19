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
#' @exportMethod
#' @examples
#' plot(odenetwork)
plot <- function(odenet, select) {
  UseMethod("plot")
}
plot.ODEnetwork <- function(odenet, select = "state12") {
  # check arguments
  checkArg(odenet, "ODEnetwork", na.ok=FALSE)
  checkArg(select, "character", len=1, na.ok=FALSE)
  
  if (select == "state12") {
    mRes <- odenet$simulation$results
    plot(mRes)
  } else if (select == "state1") {
    
  } else if (select == "state2") {
    
  } else if (select == "state1vs2") {
    
  } else {
    stop("Wrong option of argument 'select'!")
  } 
}

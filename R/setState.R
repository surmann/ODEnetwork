#' Set starting State
#' 
#' Sets the starting State for the given \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param state1 [\code{numeric(n)}]\cr
#'    Numeric vector of length n (same as in \code{\link{ODEnetwork}}) with position or angle.
#' @param state2 [\code{numeric(n)}]\cr
#'    Numeric vector of length n (same as in \code{\link{ODEnetwork}}) with velocity or magnitude.
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @export
#' @examples
#' masses <- 4:6
#' dampers <- diag(1:3)
#' springs <- diag(7:9)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' position <- rep(10, 3)
#' velocity <- rep(0, 3)
#' odenet <- setState(odenet, position, velocity)
setState <- function(odenet, state1, state2) {
  UseMethod("setState")
}

#' @method setState ODEnetwork
#' @export
setState.ODEnetwork <- function(odenet, state1, state2) {
  checkArg(state1, "numeric", len=length(odenet$masses), na.ok=FALSE)
  checkArg(state1, "vector", len=length(odenet$masses), na.ok=FALSE)
  checkArg(state2, "numeric", len=length(odenet$masses), na.ok=FALSE)
  checkArg(state2, "vector", len=length(odenet$masses), na.ok=FALSE)
  
  # set state1 and state2
  odenet$state <- cbind(state1, state2)
  
  return(odenet)
}

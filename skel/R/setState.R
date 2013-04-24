#' Set starting State
#' 
#' Sets the starting State for the given \code{\link{ODEnetwork}}.
#' One can set the state in euclidian coordinates (position and
#' velocitiy) or in polar coordinates (angle and magnitude).
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param state1 [\code{numeric(n)}]\cr
#'    Numeric vector of length n (same as in \code{\link{ODEnetwork}}) with position or angle.
#' @param state2 [\code{numeric(n)}]\cr
#'    Numeric vector of length n (same as in \code{\link{ODEnetwork}}) with velocity or magnitude.
#' @param euclidian [\code{boolean(1)}]\cr
#'    If \code{TRUE}, \code{state1} and \code{state2} are position and velocity,
#'    otherwise angle and magnitude.
#'    Default is \code{TRUE}.
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
setState <- function(odenet, state1, state2, euclidian) {
  UseMethod("setState")
}

#' @S3method setState ODEnetwork
setState.ODEnetwork <- function(odenet, state1, state2, euclidian=TRUE) {
  checkArg(state1, "numeric", len=length(odenet$masses), na.ok=FALSE)
  checkArg(state1, "vector", len=length(odenet$masses), na.ok=FALSE)
  checkArg(state2, "numeric", len=length(odenet$masses), na.ok=FALSE)
  checkArg(state2, "vector", len=length(odenet$masses), na.ok=FALSE)
  checkArg(euclidian, "logical", len=1, na.ok=FALSE)
  
  # set state type
  if (euclidian)
    odenet$statetype <- "euclidian"
  else
    odenet$statetype <- "polar"
  
  # set state1 and state2
  odenet$state <- cbind(state1, state2)
  
  return(odenet)
}

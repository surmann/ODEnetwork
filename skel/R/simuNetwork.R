# FIXME: Converting polar to cartesian coordinates missing

#' Simulation of the Differential Equations
#' 
#' Simulates the given \code{\link{ODEnetwork}} over a time range.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param times [\code{numeric}]\cr
#'    time sequence for which output is wanted,
#'    the first value of \code{times} is the initial time..
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
simuNetwork <- function(odenet, times) {
  UseMethod("simuNetwork")
}

#' @S3method simuNetwork ODEnetwork
simuNetwork.ODEnetwork <- function(odenet, times) {
  checkArg(times, "numeric", na.ok=FALSE)
  checkArg(times, "vector", na.ok=FALSE)
  
  # DGLs nummerisch l?sen
  mResOde <- ode(  y = createState(odenet)		# starting state
                 , times = times     # time vector
                 , func = createOscillators(odenet) # function of all differential equations
                 , parms = createParamVec(odenet)  # create parmeter vector from masses, springs and dampers
                 , method = "rk4"
  )
  # convert to polar coordinates
  if (odenet$coordtype == "polar") {
    strNames <- c("a", "m")
    n <- length(odenet$masses)
    colnames(mResOde) <- c(colnames(mResOde)[1], paste(rep(strNames, n), rep(1:n, each=2), sep="."))
  }
  
  # extend the ODEnetwork object
  odenet$simulation$method <- attr(mResOde, "type")
  odenet$simulation$results <- mResOde
  odenet
}

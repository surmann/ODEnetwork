# FIXME: Converting polar to cartesian coordinates missing

#' Simulation of the Differential Equations
#' 
#' Simulates the given \code{\link{ODEnetwork}} over a time range.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param times [\code{numeric}]\cr
#'    time sequence for which output is wanted,
#'    the first value of \code{times} is the initial time.
#' @param ... Additional arguments.
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
simuNetwork <- function(odenet, times, ...) {
  UseMethod("simuNetwork")
}

#' @S3method simuNetwork ODEnetwork
simuNetwork.ODEnetwork <- function(odenet, times, ...) {
  checkArg(times, "numeric", na.ok=FALSE)
  checkArg(times, "vector", na.ok=FALSE)
  
  if (is.null(odenet$events)) {
    eventdat <- NULL
  } else if (odenet$events$type == "dirac" || odenet$events$type == "constant") {
    eventdat <- list(data = odenet$events$data)
  } else if (odenet$events$type == "linear") {
    # get maximum from eventdata, to generate an event that sets the state to the correct value
    # when forcing ends
    eventdat <- odenet$events$data
    # we need only label, and with Martin Morgan trick sorting over label is not needed
    cLab <- eventdat$var[cOrder <- order(eventdat$time)]
    blnLast <- !duplicated(cLab, fromLast = TRUE) # it finds last (max) occurrence of label
    eventdat <- eventdat[seq_len(nrow(eventdat))[cOrder][blnLast], ]
    # switch to format for events in ode
    eventdat <- list(data = eventdat)
  }
  # convert to cartesian
  if (odenet$coordtype == "polar") {
    stop("Nope")
  }
  # create events structure from events data
  odenet <- createEvents(odenet)
  # DGLs nummerisch lösen
  mResOde <- ode(  y = createState(odenet)		# starting state
                 , times = times     # time vector
                 , func = createOscillators(odenet) # function of all differential equations
                 , parms = createParamVec(odenet)  # create parmeter vector from masses, springs and dampers
                 , events = eventdat
                 , ...
  )
  # extend the ODEnetwork object
  odenet$simulation$method <- attr(mResOde, "type")
  # write forcings over calculations
  if (!is.null(odenet$events) && odenet$events$type == "linear") {
    # add return of forcings
    for (i in 1:length(odenet$masses)) {
      for (strVar in c("x.", "v.")) {
        if (!is.null(odenet$events$linear[[paste(strVar, i, sep = "")]])) {
          state <- paste(strVar, i, sep = "")
          stateF <- paste(strVar, i, "Force", sep = "")
          # replace non-forcing with forcing, where forcings are not NA
          mResOde[!is.na(mResOde[, stateF]), state] <- mResOde[!is.na(mResOde[, stateF]), stateF]
          mResOde <- mResOde[, colnames(mResOde) %nin% stateF]
        }
      }
    }
    # add necessary deSolve class attributes
    attr(mResOde, "class") <- c("deSolve", "matrix")
  }
  # convert to polar coordinates
  if (odenet$coordtype == "polar") {
    strNames <- c("a", "m")
    n <- length(odenet$masses)
    colnames(mResOde) <- c(colnames(mResOde)[1], paste(rep(strNames, n), rep(1:n, each=2), sep="."))
  }
  
  # extend the ODEnetwork object
  odenet$simulation$results <- mResOde
  return(odenet)
}

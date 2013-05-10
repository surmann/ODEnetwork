#' Setting Events
#' 
#' Sets diffent types of events for the given \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param events [\code{data.frame}]\cr
#'    Data frame with a time base and named column per variable.
#'    See \code{\link{deSolve::events}} for detailed definition of \code{events}.
#' @param type [\code{character}]\cr
#'    The type of the events to use.
#'    Possible values are \code{dirac}, \code{constant} or \code{linear}.\cr
#'    Type \code{dirac} sets the current state at a given timepoint to a new value.
#'    \code{Constant} sets the state to the given value und the state does not change until
#'    setting new value or the end of \code{events}.
#'    \code{Linear} interpolates linear between \code{events} and sets the state variables
#'    to this value.\cr
#'    Default is \code{dirac}.
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @examples
#' masses <- 1
#' dampers <- as.matrix(1.5)
#' springs <- as.matrix(4)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' odenet <- setEvents(odenet, )
setEvents <- function(odenet, events, type="dirac") {
  UseMethod("setEvents")
}

#' @S3method setEvents ODEnetwork
setEvents.ODEnetwork <- function(odenet, events, type="dirac") {
  checkArg(events, "data.frame", na.ok=FALSE)
  checkArg(type, "character", len=1, na.ok=FALSE)
  checkArg(type, "character", choices=c("dirac", "constant", "linear"))
  
  if (type == "linear")
    stop ("Not implemented yet.")
  # test data frame
  if (ncol(events) < 3 || ncol(events) > 4)
    stop ("The events data.frame must have 3 or 4 columns.")
  
  if (ncol(events) == 3)
    events <- cbind(events, method = rep("rep", nrow(events)))
  
  # set event type
  odenet$events$type <- type
  # set data.frame
  odenet$events$data <- events
  
  return(odenet)
}

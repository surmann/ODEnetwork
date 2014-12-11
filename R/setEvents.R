#' Setting Events
#' 
#' Sets diffent types of events for the given \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param events [\code{data.frame}]\cr
#'    Data frame with a time base and named column per variable.
#'    See \code{\link{events}} for detailed definition of \code{events}.
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
#' @export
#' @examples
#' masses <- 1
#' dampers <- as.matrix(1.5)
#' springs <- as.matrix(4)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' eventdat <- data.frame(  var = c("x.1", "x.1")
#'                        , time = c(1, 3)
#'                        , value = c(1, 3)
#'                        )
#' odenet <- setState(odenet, 0, 0)
#' odenet <- setEvents(odenet, eventdat)
#' odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
#' plot(odenet)
setEvents <- function(odenet, events, type="dirac") {
  UseMethod("setEvents")
}

#' @method setEvents ODEnetwork
#' @export
setEvents.ODEnetwork <- function(odenet, events, type="dirac") {
  checkArg(events, "data.frame", na.ok=FALSE)
  checkArg(type, "character", len=1, na.ok=FALSE)
  checkArg(type, "character", choices=c("dirac", "constant", "linear"))
  
  # clear events
  odenet$events <- NULL
  
  # test data frame
  if (ncol(events) < 3 || ncol(events) > 4)
    stop ("The events data.frame must have 3 or 4 columns.")
  
  # convert variable-name to factor
  if (!is.factor(events$var))
    events$var <- as.factor(events$var)
  # get correct variable prefix
  if (odenet$coordtype == "cartesian") {
    cNames <- c("x", "v")
  } else {
    cNames <- c("m", "a")
  }
  # check available events
  if (!prod(levels(events$var) %in% paste(cNames, rep(1:length(odenet$masses), each=2), sep = ".")))
    stop("Wrong event names or more events than oscillators defined.")
  
  if (ncol(events) == 3)
    events <- cbind(events, method = rep("rep", nrow(events)))
  # set event type
  odenet$events$type <- type
  # set data.frame
  odenet$events$data <- events

  return(odenet)
}

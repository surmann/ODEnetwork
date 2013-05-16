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

#' @S3method setEvents ODEnetwork
setEvents.ODEnetwork <- function(odenet, events, type="dirac") {
  checkArg(events, "data.frame", na.ok=FALSE)
  checkArg(type, "character", len=1, na.ok=FALSE)
  checkArg(type, "character", choices=c("dirac", "constant", "linear"))
  
  # clear events
  odenet$events <- NULL
  
  # test data frame
  if (ncol(events) < 3 || ncol(events) > 4)
    stop ("The events data.frame must have 3 or 4 columns.")
  
  if (ncol(events) == 3)
    events <- cbind(events, method = rep("rep", nrow(events)))
  # set event type
  odenet$events$type <- type
  # set data.frame
  odenet$events$data <- events
  
  if (type == "constant") {
    # add function to know when the oscillator can move free
    strFun <- ""
    for (strVar in levels(events$var)) {
      strFun <- paste(strFun, "if (cState == \"", strVar, "\") {", sep = "")
      cTemp <- range(subset(events, var == strVar)$time)
      strFun <- paste(strFun, "ifelse (cTime < ", cTemp[1], " || cTime > ", cTemp[2], ", FALSE, TRUE)" , sep = "")
      strFun <- paste(strFun, "} else ", sep = "")
    }
    strFun <- paste(strFun, "{ NA }", sep = "")
    # leere Funktion erstellen
    fktDerivZero <- function() {}
    # Eingabeparameter einstellen
    formals(fktDerivZero) <- alist(cState=, cTime=0)
    # Funktionstext in Funktion verpacken
    expstrFunktion <- parse(text = strFun)
    # Funktion in den K?rper der leeren Funktion packen
    body(fktDerivZero) <- as.call(c(as.name("{"), expstrFunktion))
    # save in odenet
    odenet$events$zeroderiv <- fktDerivZero
  } else {
    # true for dirac and linear
    odenet$events$zeroderiv <- NULL
  }
  
  if (type == "linear") {
    # create empty function
    fktLinInter <- function() {}
    # initialise this part to make it accessable for odenet$events$linear[[strVar]]
    odenet$events$linear$complete <- fktLinInter
    # add linear interpolated function (forcings)
    strFun <- "switch(cState"
    for (strVar in levels(events$var)) {
      # no interpolation with one point
      if (table(events$var)[strVar] == 1)
        next
      # create function
      odenet$events$linear[[strVar]] <- approxfun(subset(events, var == strVar)[, c("time", "value")])
      # add link to switch statement
      strFun <- paste(strFun, ", ", strVar, " = odenet$events$linear$", strVar, "(cTime)", sep = "")
    }
    strFun <- paste(strFun, ")", sep = "")
    # add parameters
    formals(fktLinInter) <- alist(cState=, cTime=0)
    # parse function text to build a function
    expstrFunktion <- parse(text = strFun)
    # add function text to body
    body(fktLinInter) <- as.call(c(as.name("{"), expstrFunktion))
    # save in odenet
    odenet$events$linear$complete <- fktLinInter
  } else {
    odenet$events$linear <- NULL
  }
  
  return(odenet)
}

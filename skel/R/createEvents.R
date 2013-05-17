#' Creates Events
#' 
#' Creates functions for constant and linear events of the given \code{\link{ODEnetwork}}
#' to know when events have to be replaced or forced.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @examples
#' if (interactive()) {
#'   masses <- 1
#'   dampers <- as.matrix(1.5)
#'   springs <- as.matrix(4)
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   eventdat <- data.frame(  var = c("x.1", "x.1")
#'                          , time = c(1, 3)
#'                          , value = c(1, 3)
#'                          )
#'   odenet <- setState(odenet, 0, 0)
#'   odenet <- setEvents(odenet, eventdat)
#'   createEvents(odenet)
#' }
createEvents <- function(odenet) {
  UseMethod("createEvents")
}

#' @S3method createEvents ODEnetwork
createEvents.ODEnetwork <- function(odenet) {
  if (is.null(odenet$events))
    return(odenet)
  # read events data
  events <- odenet$events$data
  if (odenet$coordtype == "polar") {
    stop("bitte einbauen")
  }
  # calc events or forcings
  if (odenet$events$type == "constant") {
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
    # true for dirac and linear (redundant, but playing safe)
    odenet$events$zeroderiv <- NULL
  }
  
  if (odenet$events$type == "linear") {
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
    # (redundant, but playing safe)
    odenet$events$linear <- NULL
  }
  
  return(odenet)
}

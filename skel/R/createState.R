
#' Creates starting State Vector
#' 
#' Creates a vector with the starting state of the given \code{\link{ODEnetwork}}.
#'
#' @param odenetwork [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a named vector with assigned starting state
#' @examples
#' createState(odenetwork)
createState <- function(odenet) {
  UseMethod("createState")
}

#' @S3method createState ODEnetwork
createState.ODEnetwork <- function(odenet) {
  # convert from polar to euclidian
  if (odenet$statetype == "polar") {
    stop("Missing convert to euclidian coordinates")
  } else {
    cPos <- odenet$state[, "state1"]
    cVel <- odenet$state[, "state2"]
  }
  # create vector for state
  strState <- "c("
  for (i in 1:length(odenet$masses)) {
    # Startauslenkung und -geschwindigkeit der Massen
    strState <- paste(strState, "x.", i, " = ", cPos[i], ", ", sep = "")
    strState <- paste(strState, "v.", i, " = ", cVel[i], sep = "")
    # Komma oder Abschluss
    if (i < length(odenet$masses)) {
      strState <- paste(strState, ",")
    } else {
      strState <- paste(strState, ")", sep = "")
    }	
  }	
  # Rueckgabe
  return(eval(parse(text = strState)))
}

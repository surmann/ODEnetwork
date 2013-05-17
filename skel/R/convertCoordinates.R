stop("TODO: not ready")
#' Converts coordinates between cartesian and polar
#' 
#' Creates a vector with the starting state of the given \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a named vector with assigned starting state
#' @examples
#' if (interactive()) {
#'   masses <- c(1, 2)
#'   dampers <- diag(c(0.1, 0.5))
#'   dampers[1, 2] <- 0.05
#'   springs <- diag(c(4, 10))
#'   springs[1, 2] <- 6
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   createState(odenet)
#' }
convertCoordinates <- function(odenet) {
  UseMethod("convertCoordinates")
}

#' @S3method convertCoordinates ODEnetwork
convertCoordinates.ODEnetwork <- function(odenet) {
  # convert from polar to euclidian
  if (odenet$coordtype == "polar") {
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

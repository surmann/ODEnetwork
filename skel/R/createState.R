#' Creates starting State Vector
#' 
#' Creates a vector with the starting state of the given \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @param timepoint [\code{numeric(1)}]
#'   Timepoint at which returning the state
#' @return A named vector with assigned starting state for given timepoint.
#'   Returns the init state only with default \code{timepoint=NULL}.
#' @examples
#' if (interactive()) {
#'   masses <- c(1, 2)
#'   dampers <- diag(c(0.1, 0.5))
#'   dampers[1, 2] <- 0.05
#'   springs <- diag(c(4, 10))
#'   springs[1, 2] <- 6
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   createState(odenet)
#'   createState(odenet, 1)
#' }
createState <- function(odenet, timepoint) {
  UseMethod("createState")
}

#' @S3method createState ODEnetwork
createState.ODEnetwork <- function(odenet, timepoint = NULL) {
  print(paste("tp:", timepoint))
  if (!is.null(timepoint))
    checkArg(timepoint, "numeric", len=1, na.ok=FALSE)
  cState1 <- odenet$state$one
  cState2 <- odenet$state$two
  # timepoint only makes sense, if state1 and/or state2 is a matrix with timepoints
  if (!is.null(timepoint) && !is.matrix(cState1) && !is.matrix(cState2))
    stop("With given timepoint, at least one state must be a matrix with timepoints.")
  if (!is.null(timepoint)) {
    rowState1 <- rowState2 <- NA
    # search row for state1 or set state1 to NA
    if (is.matrix(cState1) && timepoint <= max(cState1[, "time"]))
      rowState1 <- max(which(cState1[, "time"] <= timepoint))
    else
      cState1 <- rep(NA, length(odenet$masses))
    # search row for state2 or set state2 to NA
    if (is.matrix(cState2) && timepoint <= max(cState2[, "time"]))
      rowState2 <- max(which(cState2[, "time"] <= timepoint))
    else
      cState2 <- rep(NA, length(odenet$masses))
  } else
    rowState1 <- rowState2 <- 1   # starting state
  # get values from matrix and replace entries with future NAs
  if (is.matrix(cState1)) {
    cState1 <- cState1[rowState1, -1]
    if (rowState1 < nrow(odenet$state$one) 
        && !is.null(timepoint)
        && timepoint != odenet$state$one[rowState1, "time"]) {
      cTemp <- odenet$state$one[rowState1+1, -1]
      cState1[is.na(cTemp)] <- cTemp[is.na(cTemp)]
    }
  }
  if (is.matrix(cState2)) {
    cState2 <- cState2[rowState2, -1]
    if (rowState2 < nrow(odenet$state$two)
        && !is.null(timepoint)
        && timepoint != odenet$state$two[rowState2, "time"]) {
      cTemp <- odenet$state$two[rowState2+1, -1]
      cState2[is.na(cTemp)] <- cTemp[is.na(cTemp)]
    }
  }
  # convert from polar to euclidian
  if (odenet$coordtype == "polar") {
    stop("Missing convert to euclidian coordinates")
  } else {
  }
  # create vector for state
  strState <- "c("
  for (i in 1:length(odenet$masses)) {
    # Startauslenkung und -geschwindigkeit der Massen
    strState <- paste(strState, "x.", i, " = ", cState1[i], ", ", sep = "")
    strState <- paste(strState, "v.", i, " = ", cState2[i], sep = "")
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

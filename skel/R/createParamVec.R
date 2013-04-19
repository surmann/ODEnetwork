#' Creates Parameter Vector
#' 
#' Creates a vector with assigned parameters of the given \code{\link{ODEnetwork}}.
#' ACHTUNG: Soll nicht exportiert werden
#'
#' @param odenetwork [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a named vector with assigned values
#' @examples
#' createParamVec(odenetwork)
createParamVec <- function(odenet) {
  UseMethod("createParamVec")
}
createParamVec.ODEnetwork <- function(odenet) {
  # check arguments
  checkArg(odenet, "ODEnetwork", na.ok=FALSE)
  
  # Parametervektor starten
  strParams <- "c("
  # Anregungen, Massen, Daempfer- und Federkonstanten auflisten	
  for (i in 1:length(odenet$masses)) {
    # Masse
    strParams <- paste(strParams, "m.", i, " = ", odenet$masses[i], ", ", sep = "")
    # Daempfer
    strParams <- paste(strParams, "d.", i, " = ", diag(odenet$dampers)[i], ", ", sep = "")
    # Feder
    strParams <- paste(strParams, "k.", i, " = ", diag(odenet$springs)[i], sep = "")
    # Daempfer und Feder zu den verknuepften Massen
    for (j in 1:length(odenet$masses)) {
      # Aktuellen Knoten ueberspringen
      if (j == i)
        next
      if (odenet$dampers[i, j] != 0) {
        strParams <- paste(strParams, ", d.", i, ".", j, " = ", odenet$dampers[i, j], ", ", sep = "")
      }
      if (odenet$springs[i, j] != 0) {
        strParams <- paste(strParams, "k.", i, ".", j, " = ", odenet$springs[i, j], sep = "")
      }
    }
    # Komma oder Abschluss
    if (i < length(odenet$masses))
      strParams <- paste(strParams, ",")
    else
      strParams <- paste(strParams, ")", sep = "")
  }	
  # Rueckgabe
  return(eval(parse(text = strParams)))
}

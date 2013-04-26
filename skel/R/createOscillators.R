#' Creates Set of Differential Equations
#' 
#' Creates the set of differential equations of order one from the \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a function with a set of differential equations of order one to use in a numerical step
#' @examples
#' if (interactive()) {
#'   masses <- c(1, 2)
#'   dampers <- diag(c(0.1, 0.5))
#'   dampers[1, 2] <- 0.05
#'   springs <- diag(c(4, 10))
#'   springs[1, 2] <- 6
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   createOscillators(odenet)
#' }
createOscillators <- function(odenet) {
  UseMethod("createOscillators")
}

#' @S3method createOscillators ODEnetwork
createOscillators.ODEnetwork <- function(odenet) {
  # Quelltext erstellen
  strFunktion <- "with(as.list(c(cState, cParameters)), {"
  # Testausgabe
#   strTemp <- "print(c(unname(cState), unname(createState(odenet))))"
#   strTemp <- "x.1 <- cStateNew[1]"
#   strFunktion <- c(strFunktion, strTemp)
#   strTemp <- "v.1 <- cStateNew[2]"
#   strFunktion <- c(strFunktion, strTemp)
#   strTemp <- "print(unname(c(x.1, cStateNew[1], v.1, cStateNew[2])))"
#   strFunktion <- c(strFunktion, strTemp)
#   strTemp <- "if (cTime > 0) print(c(cTime, lagvalue(cTime)))"
#   strFunktion <- c(strFunktion, strTemp)
  strTemp <- bquote(currState <- createState(odenet, cTime))
  strFunktion <- c(strFunktion, strTemp)
  strTemp <- bquote(print(unname(c(cTime, currState))))
  strFunktion <- c(strFunktion, strTemp)
  
  # Einzelnen Knoten durchgehen und die Differentialgleichungen erstellen
  for (i in 1:length(odenet$masses)) {
    # keine äußere Anregung mehr vorhanden, alle F. sind 0
    # Erstelle das Differentialgleichungs-System der Feder-Masse-Schwingers
    # Es werden nur Differentialgleichungen der 1. Ordnung verwendet
    # Test
    # dx <- v
    if (i == 0)
      strTemp <- paste("dx.", i, " <- 0", sep = "")
    else
      strTemp <- paste("dx.", i, " <- v.", i, sep = "")
    strFunktion <- c(strFunktion, strTemp)
    # dv1 <- (F1 - d*v1 - k*x1 - d12*(v1-v2) - k12*(x1-x2)) / m1
    if (i == 0)
      strTemp <- paste("dv.", i, " <- 0", sep = "")
    else {
      strTemp <- paste("dv.", i, " <- (", sep = "")
      # Dämpfer der aktuellen Masse hinzufügen
      if (odenet$dampers[i, i] != 0)
        strTemp <- paste(strTemp, " - d.", i, "*v.", i, sep = "")
      # Feder der aktuellen Masse hinzufügen
      if (odenet$springs[i, i] != 0)
        strTemp <- paste(strTemp, " - k.", i, "*x.", i, sep = "")
      # Feder und Daempfer aller Koppelelemente
      for (j in 1:length(odenet$masses)) {
        # aktuellen Knoten ueberspringen
        if (j == i)
          next
        # Daempfer und Feder, falls vorhanden
        if (odenet$dampers[i, j] != 0) {
          strTemp <- paste(strTemp, " - d.", i, ".", j, "*(v.", i, "-v.", j, ")", sep = "")
        }
        if (odenet$springs[i, j] != 0) {
          strTemp <- paste(strTemp, " - k.", i, ".", j, "*(x.", i, "-x.", j, ")", sep = "")
        }
      }
      # Abschluss: Klammer schließen und durch Masse teilen
      strTemp <- paste(strTemp, ")/m.", i, sep = "")
    }
    strFunktion <- c(strFunktion, strTemp)
  }
  
  # Rueckgabeliste der Werte des Zustandsraumes
  strTemp <- "list(c("
  for (i in 1:length(odenet$masses)) {
    # Auslenkung und Geschwindigkeit der Feder-Masse-Schwinger
    strTemp <- paste(strTemp, "dx.", i, ", dv.", i, sep = "")
    if (i < length(odenet$masses))
      strTemp <- paste(strTemp, ",")
    else
      strTemp <- paste(strTemp, "))", sep = "")
  }
  strFunktion <- c(strFunktion, strTemp)
  
  # Ende von 'with(as.list(
  strFunktion <- c(strFunktion, "})")
  
  # leere Funktion erstellen
  fktOszillator <- function() {}
  # Eingabeparameter einstellen
  formals(fktOszillator) <- alist(cTime=, cState=, cParameters=)
  # Funktionstext in Funktion verpacken
  expstrFunktion <- parse(text = strFunktion)
  # Funktion in den Körper der leeren Funktion packen
  body(fktOszillator) <- as.call(c(as.name("{"), expstrFunktion))
  
  # Fertige Funktion des DGL-Systems ausgeben
  fktOszillator
}

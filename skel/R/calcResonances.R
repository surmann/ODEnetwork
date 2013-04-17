#' Calculate Resonance Frequencies
#' 
#' Calculates the resonance frequencies of a given \code{\link{ODEnetwork}}.
#' The resonance frequencies are calculated without respect to the dampers and neighourhood structure.
#'
#' @param odenetwork [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a data frame with a vector of resonance frequencies.
#' @export
#' @examples
#' calcResonances(odenetwork)
calcResonances <- function(odenet) {
  UseMethod("calcResonances", odenet)
}
calcResonances.ODEnetwork <- function(odenet) {
  # test parameter
  checkArg(odenet, "ODEnetwork", na.ok=FALSE)
  
  # Resonanzfrequenz
  cResonances <- sqrt(diag(odenet$springs) / odenet$masses) / (2*pi)
  # Quotient aus Erregerfrequenz und Resonanzfrequenz ausrechnen
#   cEta <- dfAnr$f / cResonances
#   cLehrD <- diag(odenet$dampers) / (2 * sqrt(diag(odenet$springs) * odenet$masses)
  # Vergrößerungsfaktor (Wiki: alpha1)
#   cResRise <- 1 / sqrt((1 - cEta^2)^2 + (2 * cLehrD * cEta)^2)
  # Faktor zum Multiplizieren mit der anregenden Amplitude: alpha1/k
#   cResRise <- cResRise / diag(odenet$springs)
  
  # Rückgabe
#   return(data.frame(cResonances = cResonances, ResQuotient = cEta, ResonanceRise = cResRise))
  return(data.frame(cResonances = cResonances))
}

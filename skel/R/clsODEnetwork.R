#' Constructor of the class ODEnetwork
#' 
#' Creates a list of class \code{ODEnetwork}.
#'
#' @param masses [\code{vector}] of length n\cr
#'   The masses of the mechanical oscillators.
#' @param dampers [\code{matrix}}] quadratic of size n\cr
#'   The dampers of the mechanical oscillators on the main diagonal.
#'   Connecting dampers between oscillators on the upper triangel.
#'   (Will be copied automatically to create a symmetric matrix.)
#' @param springs [\code{matrix}}] quadratic of size n\cr
#'   The springs are defined in the network like matrix of dampers.
#' @return a list of class [\code{\link{ODEnetwork}}].
#' @exportClass
#' @examples
#' mM <- c(40, 10, 10)
#' mD <- diag(c(1, 5, 0))
#' mD[1, 2] <- 1
#' mD[2, 3] <- 1
#' mK <- diag(c(50, 50, 0))
#' mK[1, 2] <- 10
#' mK[2, 3] <- 10
#' odenet <- ODEnetwork(mM, mD, mK)
ODEnetwork <- function(masses, dampers, springs) {
  # test parameter
  checkArg(masses, "vector", min.len=1L, na.ok=FALSE)
  checkArg(dampers, "matrix", min.len=1L, na.ok=FALSE)
  checkArg(springs, "matrix", min.len=1L, na.ok=FALSE)
  # test on equal dimenstions
  if (var(c(length(masses), dim(dampers), dim(springs))) != 0)
    stop("All parameter must be of the same length or size!")
  
  # copy upper triangonal to lower triangonal => symmetric matrix
  dampers[lower.tri(dampers)] <- dampers[upper.tri(dampers)]
  springs[lower.tri(springs)] <- springs[upper.tri(springs)]
  # create class ODEnetwork
  odenet <- list(masses = masses, dampers = dampers, springs = springs)
  class(odenet) <- "ODEnetwork"
  # return
  odenet
}

#' Constructor of the class ODEnetwork
#' 
#' Creates a list of class \code{ODEnetwork}.
#' The coordinate type can be set to cartesian (position and
#' velocitiy) or to polar coordinates (angle and magnitude).
#'
#' @param masses [\code{vector}] of length n\cr
#'   The masses of the mechanical oscillators.
#' @param dampers [\code{matrix}] quadratic of size n\cr
#'   The dampers of the mechanical oscillators on the main diagonal.
#'   Connecting dampers between oscillators on the upper triangel.
#'   (Will be copied automatically to create a symmetric matrix.)
#' @param springs [\code{matrix}] quadratic of size n\cr
#'   The springs are defined in the network like matrix of dampers.
#' @param cartesian [\code{boolean(1)}]\cr
#'    If \code{TRUE}, \code{state1} and \code{state2} are position and velocity,
#'    otherwise angle and magnitude.
#'    Default is \code{TRUE}.
#' @return a list of class [\code{\link{ODEnetwork}}].
#' @export
#' @examples
#' mM <- c(40, 10, 10)
#' mD <- diag(c(1, 5, 0))
#' mD[1, 2] <- 1
#' mD[2, 3] <- 1
#' mK <- diag(c(50, 50, 0))
#' mK[1, 2] <- 10
#' mK[2, 3] <- 10
#' odenet <- ODEnetwork(mM, mD, mK)
ODEnetwork <- function(masses, dampers, springs, cartesian=TRUE) {
  checkArg(masses, "numeric", min.len=1L, na.ok=FALSE)
  checkArg(masses, "vector", min.len=1L, na.ok=FALSE)
  checkArg(dampers, "numeric", min.len=1L, na.ok=FALSE)
  checkArg(dampers, "matrix", min.len=1L, na.ok=FALSE)
  checkArg(springs, "numeric", min.len=1L, na.ok=FALSE)
  checkArg(springs, "matrix", min.len=1L, na.ok=FALSE)
  checkArg(cartesian, "logical", len=1, na.ok=FALSE)
  
  # test on equal dimenstions
  if (var(c(length(masses), dim(dampers), dim(springs))) != 0)
    stop("All parameter have be of the same length or size!")
  # mass has to be positive
  if (sum(masses <= 0) > 0)
    stop("All masses have to be positive!")

  # set state type
  if (cartesian)
    coordtype <- "cartesian"
  else
    coordtype <- "polar"
  # copy upper triangonal to lower triangonal => symmetric matrix
  dampers[lower.tri(dampers)] <- dampers[upper.tri(dampers)]
  springs[lower.tri(springs)] <- springs[upper.tri(springs)]
  setClasses(list(masses = masses
                  , dampers = dampers
                  , springs = springs
                  , coordtype = coordtype)
             , "ODEnetwork")
}

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
#' @param distances [\code{matrix}] quadratic of size n\cr
#'    Describes the length of each spring.
#'    Elements on the main diagonal describe spring length connecting the masses to the ground.
#'    All upper triangle elements describe spring distance between two masses i < j.
#'    Default is \code{NA}, which is equivalent to a zero matrix.
#'    (Negative value will be copied automatically to lower triangle.)
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
ODEnetwork <- function(masses, dampers, springs, cartesian=TRUE, distances=NA) {
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
    stop("All masses have to be positive.")
  # positive springs
  if (sum(springs < 0) > 0)
    stop("Springs must be nonzero.")
  # check distances
  if (!is.matrix(distances) && !is.numeric(distances))
    distances <- diag(0, length(masses))
  if (is.matrix(distances))
    distances[!is.numeric(distances)] <- 0
  
  # copy upper triangle to lower triangle => symmetric matrix
  dampers[lower.tri(dampers)] <- t(dampers)[lower.tri(dampers)]
  springs[lower.tri(springs)] <- t(springs)[lower.tri(springs)]
  distances[lower.tri(distances)] <- -t(distances)[lower.tri(distances)]
  
  # set state type
  if (cartesian)
    coordtype <- "cartesian"
  else
    coordtype <- "polar"
  
  # add empty states
  states <- cbind(state1 = rep(0, length(masses)), state2 = rep(0, length(masses)))
  
  # return network
  setClasses(list(masses = masses
                  , dampers = dampers
                  , springs = springs
                  , distances = distances
                  , coordtype = coordtype
                  , state = states)
             , "ODEnetwork")
}

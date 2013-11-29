#' Estimate distances between oscillators
#' 
#' Estimates the distances between the oscillators of a \code{\link{ODEnetwork}}
#' from an equilibrium state.
#' 
#'  @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#'  @param equilibrium [\code{numeric(n)}]\cr
#'    The desired equilibrium positions of the oscillators.
#'  @param distGround [\code{character(1)}] or [\code{character(n)}]\cr
#'    \code{"combined"} estimates one value for all distances of the oscillators to the ground.
#'    Optimisation starts from \code{median(equilibrium)}.\cr
#'    \code{"individual"} estimates individual distance values for every oscillator.
#'    Optimisation starts from \code{equilibrium}.\cr
#'    \code{"fixed"} no estimation of the distances to the ground. 
#'    Set to diagonal of distances matrix in \code{\link{ODEnetwork}}.\cr
#'    \code{character(n)} specifies groups of oscillators which distances to the ground are 
#'    estimated by the same value. Optimisation starts from \code{median(equilibrium)} of the 
#'    specified groups.\cr
#'    Default is \code{"combined"}
#'  @return an extended list of class [\code{\link{ODEnetwork}}].\cr
#'    Matrix of distances is added or overwritten.
#'  @export
#'  @examples
#'    masses <- c(1, 1)
#'    dampers <- diag(c(1, 1))
#'    springs <- diag(c(1, 1))
#'    springs[1, 2] <- 1
#'    equilibrium <- c(1/3, 5/3)
#'    odenet <- ODEnetwork(masses, dampers, springs)
#'    estimateDistances(odenet, equilibrium)$distances
#'    estimateDistances(odenet, equilibrium, distGround="individual")$distances

estimateDistances <- function(odenet, equilibrium, distGround=c("combined", "individual", "fixed", c("A", "B", "123", "A"))) {
  UseMethod("estimateDistances")
}

#' @S3method estimateDistances ODEnetwork
estimateDistances.ODEnetwork <- function(odenet, equilibrium, distGround="combined") {
  # number of oscillators
  cN <- length(odenet$masses)
  # Equilibrium
  checkArg(equilibrium, "numeric", na.ok=FALSE)
  checkArg(equilibrium, "vector", len=cN, na.ok=FALSE)
  # distances to the ground
  checkArg(distGround, "character", na.ok=FALSE)
  if (length(distGround) != 1 && length(distGround) != cN) {
    stopf("The length of the distances to the ground has to be 1 or n.")
  }
  # check arguments of distGround
  if (cN > 1 && length(distGround) == 1) {
    checkArg(distGround, choices=c("combined", "individual", "fixed"))
  }
  
  # delete names
  names(equilibrium) <- NULL
  
  # exception for one mass
  if (cN == 1 && distGround != "fixed") {
    odenet <- updateOscillators(odenet, ParamVec=c(r.1=equilibrium))
    return(odenet)
  }
  
  # create parameter vector
  cParams <- numeric()
  # check distance estimation to the ground
  if (length(distGround) == 1) {
    if (distGround == "combined") {
      # one parameter for all distances to the ground
      cParams <- c(r.glob = median(equilibrium))
    } else if (distGround == "individual") {
      # one parameter for each distance
      cParams <- c(equilibrium)
      names(cParams) <- paste("r.glob", 1:cN, sep=".")
    }
  } else {
    # character vector indicates the groups for the parameter estimation
    for (grp in unique(distGround)) {
      cParams <- c(cParams, median(equilibrium[distGround == grp]))
      names(cParams)[length(cParams)] <- paste("r.glob", paste(which(distGround == grp), collapse = "."), sep = ".")
    }
  }

  # add distances between oscillators with respect to springs and dampers to parameter vector
  mConnect <- odenet$springs != 0
  for (iRow in 1:(cN-1)) {
    for (iCol in (iRow+1):cN) {
      if (mConnect[iRow, iCol]) {
        cParams <- c(cParams, odenet$distances[iRow, iCol])
        names(cParams)[length(cParams)] <- paste("r", iRow, iCol, sep = ".")
      }
    }
  }
  
  # exit, if no free parameters available
  if (length(cParams) == 0) {
    message("All parameters are fixed.")
    return(odenet)
  }
  
  # calculate target vector
  mK <- odenet$springs
  diag(mK) <- -rowSums(mK)
  mK <- -mK
  bTarget <- -mK %*% equilibrium
  
  # define cost function
  distCost <- function(cParameters) {
    cParameters <- splitGlobalParams(cParameters)
    odenet <- updateOscillators(odenet, ParamVec=cParameters)
    # get distances and convert to correct form
    mR <- odenet$distances
    diag(mR) <- -diag(mR)
    mR[lower.tri(mR)] <- -mR[lower.tri(mR)]
    # calculate vector b with b_i = sum(k_ij*r_ij, j=1..n)
    b <- diag(odenet$springs %*% t(mR))
    # return SSE
#     return(sum((b-bTarget)^2))
    # return residuals
    return(bTarget-b)
  }
  # split the parameter vector with respect to estimate (grouped) global distances
  splitGlobalParams <- function(cParameters) {
    if (sum(grepl("r\\.glob", names(cParameters))) > 0) {
      # estimate different groups of global distances
      # extract the values
      globVal <- cParameters[grep("r\\.glob", names(cParameters))]
      cParameters <- cParameters[-grep("r\\.glob", names(cParameters))]
      # one global distance, or different ones
      if (length(globVal) == 1) {
        lstMassGrps <- list(1:length(odenet$masses))
      } else {
        lstMassGrps <- gsub("r\\.glob\\.", "", names(globVal))
        lstMassGrps <- strsplit(lstMassGrps, ".", fixed = TRUE)
      }
      # multiply values to the correct r.i's
      for (i in length(lstMassGrps):1) {
        cParameters <- c(rep(globVal[i], length(lstMassGrps[[i]])), cParameters)
        names(cParameters)[1:length(lstMassGrps[[i]])] <- paste("r", lstMassGrps[[i]], sep = ".")
      }
    }
    return(cParameters)
  }
  
  # optimise parameters
#   optimFit <- optim(cParams, distCost, method="BFGS", control=list(maxit=1000))
  print(paste("Params:", length(cParams)))
  print(paste("Resid:", length(distCost(cParams))))
  nlslmFit <- nls.lm(par=cParams, fn=distCost)
#   # Throw warnings
#   if (optimFit$convergence != 0) {
#     warningf(paste("No successful completition. Code:", optimFit$convergence))
#   }
#   if (optimFit$value > 1e-7 * distCost(cParams)) {
#   if (optimFit$value > 10 * sqrt(.Machine$double.eps) * distCost(cParams)) {
#     warningf(paste("The SSE of the distances is large:", optimFit$value))
#   }

  # update the optimal values to the odenet
  odenet <- updateOscillators(odenet, ParamVec=splitGlobalParams(nlslmFit$par))
  
  return(odenet)
}

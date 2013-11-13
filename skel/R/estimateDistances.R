#' Estimate distances between oscillators
#' 
#' Estimates the distances between the oscillators of a \code{\link{ODEnetwork}}
#' from an equilibrium state.
#' 
#'  @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#'  @param equilibrium [\code{numeric(n)}]\cr
#'    The desired equilibrium positions of the oscillators.
#'  @param globalDist [\code{numeric(1)}] or [\code{numeric(n)}] or [\code{character(n)}]\cr
#'    Distance of the oscillators from the ground.
#'    On length 1, the distance is set globally.
#'    The vector of length n contains a distance for every oscillator.
#'    Default is \code{NA}, which estimates the global distance from the equilibrium.
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
#'    odenet <- estimateDistances(odenet, equilibrium)

estimateDistances <- function(odenet, equilibrium, globalDist=NA) {
  UseMethod("estimateDistances")
}

#' @S3method estimateDistances ODEnetwork
estimateDistances.ODEnetwork <- function(odenet, equilibrium, globalDist=NA) {
  # number of oscillators
  cN <- length(odenet$masses)
  # Equilibrium
  checkArg(equilibrium, "numeric", na.ok=FALSE)
  checkArg(equilibrium, "vector", len=cN, na.ok=FALSE)
  # global distance
  if (sum(is.na(globalDist)) > 0) {
    globalDist <- NA
  } else {
    checkArg(globalDist, c("numeric", "character"), na.ok=FALSE)
    checkArg(globalDist, "vector", na.ok=FALSE)
    if (length(globalDist) != 1 && length(globalDist) != cN) {
      stopf("The length of the global distance has to be 1 or n.")
    }
  }
  
  # delete names
  names(equilibrium) <- NULL
  
  # exception for one mass
  if (cN == 1) {
    if (sum(is.na(globalDist)) > 0)
      odenet <- updateOscillators(odenet, ParamVec=c(r.1=equilibrium))
    else
      odenet <- updateOscillators(odenet, ParamVec=c(r.1=globalDist))
    return(odenet)
  }
  
  # create parameter vector
  cParams <- numeric()
  # check global distance
  if (sum(is.na(globalDist)) > 0) {
    # one parameter to estimate the global distance
    globalDist <- median(equilibrium)
    cParams <- c(cParams, r.glob = globalDist)
  } else if (is.character(globalDist)) {
    # character vector indicates the groups for the parameter estimation
    for (grp in unique(globalDist)) {
      cParams <- c(cParams, median(equilibrium[globalDist == grp]))
      names(cParams)[length(cParams)] <- paste("r.glob", paste(which(globalDist == grp), collapse = "."), sep = ".")
    }
  } else {
    # set the global distance to fixed values
    if (length(globalDist) == 1){
      cTemp <- rep(globalDist, cN)
    } else {
      cTemp <- globalDist
    }
    names(cTemp) <- paste("r", 1:cN, sep = ".")
    odenet <- updateOscillators(odenet, ParamVec=cTemp)
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
  
  # exit, if no free parameters left
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
    return(sum((b-bTarget)^2))
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
  optimFit <- optim(cParams, distCost, control=list(maxit=1000))
  # Throw warnings
  if (optimFit$convergence != 0) {
    warningf(paste("No successful completition. Code:", optimFit$convergence))
  }
  if (optimFit$value > 1e-7 * distCost(cParams)) {
    warningf(paste("The SSE of the distances is large:", optimFit$value))
  }

  # update the optimal values to the odenet
  odenet <- updateOscillators(odenet, ParamVec=splitGlobalParams(optimFit$par))
  
  return(odenet)
}

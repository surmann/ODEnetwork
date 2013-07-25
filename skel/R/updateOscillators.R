#' Update oscillator parameter of an existing ODE network.
#' 
#' Updates the parameters of an existing ODE network.
#' The function overwrites the parameters in the vector and matricies.
#' However, it will not change the size of the network.
#' It is possible to set a new vector and new matricies, or single parameters in a names vector.
#' 
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param ParamVec [\code{vector}] of length n\cr
#'   Named vector to overwrite corresponding parameters.
#'   If the vector is set, the following parameters are ignored.
#' @param masses [\code{vector}] of length n\cr
#'   The masses of the mechanical oscillators.
#' @param dampers [\code{matrix}] quadratic of size n\cr
#'   The dampers of the mechanical oscillators on the main diagonal.
#'   Connecting dampers between oscillators on the upper triangel.
#'   (Will be copied automatically to create a symmetric matrix.)
#' @param springs [\code{matrix}] quadratic of size n\cr
#'   The springs are defined in the network like matrix of dampers.
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @export
#' @examples
#' masses <- c(1:5)
#' dampers <- diag(11:15)
#' springs <- diag(21:25)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' odenet <- updateOscillators(odenet, masses = c(3:7))
#' odenet <- updateOscillators(odenet, c(k.1.2 = 201, k.3.5 = 202))
#' # Warning: Following value is ignored, because it is on the lower triangle
#' odenet <- updateOscillators(odenet, c(d.2.1 = 101))
updateOscillators <- function(odenet, ParamVec, masses, dampers, springs) {
  UseMethod("updateOscillators")
}

#' @S3method updateOscillators ODEnetwork
updateOscillators.ODEnetwork <- function(odenet, ParamVec=NA, masses=NA, dampers=NA, springs=NA) {
  if (sum(!is.na(ParamVec)) > 0) {
    # checking arguments
    checkArg(ParamVec, "numeric", min.len=1L, na.ok=FALSE)
    checkArg(ParamVec, "vector", min.len=1L, na.ok=FALSE)
    # delete matricies
    masses <- NA
    dampers <- NA
    springs <- NA
    
    # extract parameter settings from the vector to change odenet parameters
    # masses
    cPosSource <- grep("^m\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      masses <- odenet$masses
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      masses[cPosTarget] <- ParamVec[cPosSource]
    }
    
    # dampers
    # diagonal elements d.2
    cPosSource <- grep("^d\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      dampers <- odenet$dampers
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      diag(dampers)[cPosTarget] <- ParamVec[cPosSource]
    }
    # off diagonal elements d.1.45
    cPosSource <- grep("^d(\\.\\d+){2}$", names(ParamVec))
    if (length(cPosSource) > 0) {
      if (sum(!is.na(dampers)) == 0)
        dampers <- odenet$dampers
      cTemp <- sub("\\D+", "", names(ParamVec)[cPosSource])
      cPosTarget <- as.numeric(sub("\\.\\d+$", "", cTemp))
      cPosTarget2 <- as.numeric(sub("^\\d+\\.", "", cTemp))
      # delete lower triangonal
      cTemp <- cPosTarget < cPosTarget2
      cPosTarget <- cPosTarget[cTemp]
      cPosTarget2 <- cPosTarget2[cTemp]
      for (i in 1:length(cPosTarget)) {
        dampers[cPosTarget[i], cPosTarget2[i]] <- ParamVec[cPosSource[i]]
      }
    }

    # springs
    # diagonal elements k.2
    cPosSource <- grep("^k\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      springs <- odenet$springs
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      diag(springs)[cPosTarget] <- ParamVec[cPosSource]
    }
    # off diagonal elements k.1.45
    cPosSource <- grep("^k(\\.\\d+){2}$", names(ParamVec))
    if (length(cPosSource) > 0) {
      if (sum(!is.na(springs)) == 0)
        springs <- odenet$springs
      cTemp <- sub("\\D+", "", names(ParamVec)[cPosSource])
      cPosTarget <- as.numeric(sub("\\.\\d+$", "", cTemp))
      cPosTarget2 <- as.numeric(sub("^\\d+\\.", "", cTemp))
      # delete lower triangonal
      cTemp <- cPosTarget < cPosTarget2
      cPosTarget <- cPosTarget[cTemp]
      cPosTarget2 <- cPosTarget2[cTemp]
      for (i in 1:length(cPosTarget)) {
        springs[cPosTarget[i], cPosTarget2[i]] <- ParamVec[cPosSource[i]]
      }
    }
  } else {
    if ((sum(!is.na(masses)) + sum(!is.na(dampers)) + sum(!is.na(springs))) == 0)
      stop("Set at least one parameter.")
  }
  
  cLen <- length(odenet$masses)
  if (sum(!is.na(masses)) > 0) {
    checkArg(masses, "numeric", len=cLen, na.ok=FALSE)
    checkArg(masses, "vector", na.ok=FALSE)
    if (sum(masses <= 0) > 0)
      stop("All masses have to be positive!")
    odenet$masses <- masses
  }
  if (sum(!is.na(dampers)) > 0) {
    checkArg(dampers, "numeric", len=cLen^2, na.ok=FALSE)
    checkArg(dampers, "matrix", na.ok=FALSE)
    # positive damping
    if (sum(dampers < 0) > 0)
      stop("Damping must be nonzero.")
    # copy upper triangle to lower triangle => symmetric matrix
    dampers[lower.tri(dampers)] <- t(dampers)[lower.tri(dampers)]
    odenet$dampers <- dampers
  }
  if (sum(!is.na(springs)) > 0) {
    checkArg(springs, "numeric", len=cLen^2, na.ok=FALSE)
    checkArg(springs, "matrix", na.ok=FALSE)
    # positive springs
    if (sum(springs < 0) > 0)
      stop("Springs must be nonzero.")
    # copy upper triangle to lower triangle => symmetric matrix
    springs[lower.tri(springs)] <- t(springs)[lower.tri(springs)]
    odenet$springs <- springs
  }

  # return
  odenet
}

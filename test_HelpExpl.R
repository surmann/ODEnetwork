## =============================================================================
## FORCING FUNCTION: The sediment oxygen consumption example - R-code:
## =============================================================================

## Forcing function data
Flux <- matrix(ncol=2,byrow=TRUE,data=c(
  1, 0.654, 11, 0.167,   21, 0.060, 41, 0.070, 73,0.277, 83,0.186,
  93,0.140,103, 0.255,  113, 0.231,123, 0.309,133,1.127,143,1.923,
  153,1.091,163,1.001,  173, 1.691,183, 1.404,194,1.226,204,0.767,
  214, 0.893,224,0.737, 234,0.772,244, 0.726,254,0.624,264,0.439,
  274,0.168,284 ,0.280, 294,0.202,304, 0.193,315,0.286,325,0.599,
  335, 1.889,345, 0.996,355,0.681,365,1.135))

parms <- c(k=0.01)

times <- 1:500

## the model
sediment <- function( t, cState, cParameters) {
  with(as.list(c(cState, cParameters)), {
    if (!is.na(Depo(t))) {
      ddepo <- 0
      dO2 <- Depo(t) - k * O2
    } else {
      ddepo <- 0.01
      dO2 <- depo - k * O2
    }
    list (c(dO2, ddepo), depo = Depo(t))
  })
}

# the forcing functions; rule = 2 avoids NaNs in interpolation
Depo <- approxfun(x = Flux[,1], y = Flux[,2], method = "linear", rule = 1)

Out <- ode(y = c(O2 = 63, depo = 0), times = times, func = sediment, parms = parms)
plot(Out)

## same forcing functions, now constant interpolation
Depo <- approxfun(x = Flux[,1], y = Flux[,2], method = "constant",
                  f = 0.5, rule = 2)

Out2 <- ode(times = times, func = sediment, y = c(O2 = 63), parms = parms)

mf <- par(mfrow = c(2, 1))
plot (Out, which = "depo", type = "l", lwd = 2, mfrow = NULL)
lines(Out2[,"time"], Out2[,"depo"], col = "red", lwd = 2)

plot (Out, which = "O2", type = "l", lwd = 2, mfrow = NULL)
lines(Out2[,"time"], Out2[,"O2"], col = "red", lwd = 2)

## =============================================================================
## SCOC is the same model, as implemented in FORTRAN
## =============================================================================

out<- SCOC(times, parms = parms, Flux = Flux)

plot(out[,"time"], out[,"Depo"], type = "l", col = "red")
lines(out[,"time"], out[,"Mineralisation"], col = "blue")

## Constant interpolation of forcing function - left side of interval
fcontrol <- list(method = "constant")

out2 <- SCOC(times, parms = parms, Flux = Flux, fcontrol = fcontrol)

plot(out2[,"time"], out2[,"Depo"], type = "l", col = "red")
lines(out2[,"time"], out2[,"Mineralisation"], col = "blue")


## Not run: 
## =============================================================================
## show examples (see respective help pages for details)
## =============================================================================

example(aquaphy)

## show package vignette with tutorial about how to use compiled models
## + source code of the vignette
## + directory with C and FORTRAN sources
vignette("compiledCode")
edit(vignette("compiledCode"))
browseURL(paste(system.file(package = "deSolve"), "/doc", sep = ""))

## End(Not run)




## =============================================================================
## 1. EVENTS in a data.frame
## =============================================================================

## derivative function: derivatives set to 0
derivs <- function(t, var, parms) {
  list(dvar = rep(0, 2))
}

yini <- c(v1 = 1, v2 = 2)
times <- seq(0, 20, by = 0.1)

eventdat <- data.frame(var = c("v1", "v2", "v2", "v1"),
                       time = c(1, 1, 5, 9) ,
                       value = c(1, 2, 3, 4),
                       method = c("add", "mult", "rep", "add"))
eventdat

out <- vode(func = derivs, y = yini, times = times, parms = NULL,
            events = list(data = eventdat))
plot(out)

##
eventdat <- data.frame(var = c(rep("v1", 10), rep("v2", 10)), 
                       time = c(1:10, 1:10),
                       value = runif(20),
                       method = rep("add", 20))
eventdat

out <- ode(func = derivs, y = yini, times = times, parms = NULL,
           events = list(data = eventdat))

plot(out)

## =============================================================================
## 2. EVENTS in a function
## =============================================================================

## derivative function: rate of change v1 = 0, v2 reduced at first-order rate
derivs <- function(t, var, parms) {
  list(c(-1, -0.5 * var[2]))
}


# events: add 1 to v1, multiply v2 with random number
eventfun <- function(t, y, parms){
  with (as.list(y),{
    v1 <- v1 + 1
    v2 <- 5 * runif(1)
    return(c(v1, v2))
  })
}

yini <- c(v1 = 1, v2 = 2)
times <- seq(0, 20, by = 0.1)

out <- ode(func = derivs, y = yini, times = times, parms = NULL,
           events = list(func = eventfun, time = 1:9) )
plot(out, type = "l")

## =============================================================================
## 3. EVENTS triggered by a root function
## =============================================================================

## derivative: simple first-order decay
derivs <- function(t, y, pars) {
  return(list(-0.1 * y))
}

## event triggered if state variable = 0.5
rootfun <- function (t, y, pars) {
#   return(y - 0.5)
#   return(as.numeric(y > 0.5))
  return(as.numeric(FALSE))
}

## sets state variable = 1                                                  
eventfun <- function(t, y, pars) {
#   return(y = 1)
  return(y = fundata(t))
}

fundata <- approxfun(x = c(1, 100), y = c(1, 10))

yini <- 2
times <- seq(0, 100, 0.1)

## uses ode to solve; root = TRUE specifies that the event is
## triggered by a root.
out <- ode(times = times, y = yini, func = derivs, parms = NULL,
           events = list(func = eventfun, root = TRUE),
           rootfun = rootfun)

plot(out, type = "l")

## time of the root:
troot <- attributes(out)$troot
points(troot, rep(0.5, length(troot)))


## =============================================================================
## 4. More ROOT examples: Rotation function
## =============================================================================
Rotate <- function(t, x, p )
  list(c( x[2],
          -x[1]  ))

## Root = when second state variable = 0
rootfun <- function(t, x, p) x[2]

## "event" returns state variables unchanged
eventfun <- function(t, x, p) x
times <- seq(from = 0, to = 15, by = 0.1)

## 1. No event: stops at first root
out1 <- ode(func = Rotate, y = c(5, 5), parms = 0,
            times = times, rootfun = rootfun)
tail(out1)

## 2. Continues till end of times and records the roots           
out <- ode(func = Rotate, y = c(5, 5), parms = 0,
           times = times, rootfun = rootfun,
           events = list(func = eventfun, root = TRUE) )

plot(out)
troot <- attributes(out)$troot  # time of roots
points(troot,rep(0, length (troot)))

## Multiple roots:  either one of the state variables = 0
root2 <- function(t, x, p) x

out2 <- ode(func = Rotate, y = c(5, 5), parms = 0,
            times = times, rootfun = root2,
            events = list(func = eventfun, root = TRUE) )

plot(out2, which = 2)
troot <- attributes(out2)$troot
indroot <- attributes(out2)$indroot  # which root was found
points(troot, rep(0, length (troot)), col = indroot, pch = 18, cex = 2)

## Multiple roots and stop at first time root 1.
out3 <- ode(func = Rotate, y = c(5, 5), parms = 0,
            times = times, rootfun = root2,
            events = list(func = eventfun, root = TRUE, terminalroot = 1))


## =============================================================================
## 5. Stop at 5th root - only works with radau.
## =============================================================================
Rotate <- function(t, x, p )
  list(c( x[2],
          -x[1],
          0  ))

## Root = when second state variable = 0
root3  <- function(t, x, p)  c(x[2], x[3] - 5)
event3 <- function (t, x, p) c(x[1:2], x[3]+1)
times <- seq(0, 15, 0.1)
out3 <- ode(func = Rotate, y = c(x1 = 5, x2 = 5, nroot = 0), 
            parms = 0, method = "radau",
            times = times, rootfun = root3,
            events = list(func = event3, root = TRUE, terminalroot = 2))
plot(out3)
attributes(out3)[c("troot", "nroot", "indroot")]

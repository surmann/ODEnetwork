library(methods)
library(devtools)
library(testthat)
library(checkmate)

if (interactive()) {
  library(BBmisc)
  library(deSolve)
  load_all(".", reset = TRUE)
} else {
  library(ODEnetwork)  
}

test_dir("tests/testthat")

library(methods)
library(devtools)
library(testthat)

library(BBmisc)
library(deSolve)

if (interactive()) {
  load_all(".", reset = TRUE)
} else {
  library(ODEnetwork)  
}
test_dir("tests/testthat")

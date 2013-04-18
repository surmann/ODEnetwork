library(methods)
library(devtools)
library(testthat)
library(BBmisc)

if (interactive()) {
  load_all("skel")
} else {
  library(ODEnetwork)  
}
test_dir("skel/inst/tests")

# ODEnetwork

[![CRAN status](https://www.r-pkg.org/badges/version/ODEnetwork)](https://cran.r-project.org/package=ODEnetwork)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/nqkrw6ayyresytjw/branch/master?svg=true)](https://ci.appveyor.com/project/DirkSurmann/odenetwork/branch/master)
[![Travis build Status](https://travis-ci.org/surmann/ODEnetwork.svg)](https://travis-ci.org/surmann/ODEnetwork)
[![Coverage Status](https://coveralls.io/repos/github/surmann/ODEnetwork/badge.svg?branch=master)](https://coveralls.io/github/surmann/ODEnetwork?branch=master)

The **ODEnetwork** package provides a framework to construct networks of second order differential equations in an efficient way.
A solution of each network is calculated analytically.
You are able to stress a network by changing the states of each differential equation.
In this case, the network is solved in a numerical way.
The states can be plotted over time.

## Installation instructions

You can install the current github version using the [devtools](https://github.com/hadley/devtools) package and the following command in R:
```R
devtools::install_github("surmann/ODEnetwork")
```

## Example 1

The first example shows the definition of a single harmonic oscillator with mass 1 and a damping constant of 0.5.
A spring connects the mass to the ground.
It has the spring constant 4 and a distance of 2 from the ground.
```R
masses <- 1
dampers <- as.matrix(0.5)
springs <- as.matrix(4)
distances <- as.matrix(2)

odenet <- ODEnetwork(masses, dampers, springs, distances = distances)

odenet <- setState(odenet, 3, 0)
```
A solution is calculated and plotted by the following code:
```R
odenet <- simuNetwork(odenet, seq(0, 10, by = 0.1))
plot(odenet)
plot(odenet, state = "1vs2")
```

## Example 2

A second example shows two connected oscillators over a time of 40s:
```R
masses <- c(1, 2)
dampers <- diag(c(0.02, 0.1))
dampers[1, 2] <- 0.1
springs <- diag(c(4, 1))
springs[1, 2] <- 2
distances <- matrix(c(0, 0, 1, 0), ncol = 2)

odenet <- ODEnetwork(masses, dampers, springs, distances = distances)

odenet <- setState(odenet, c(1, 1), c(0, 0))
odenet <- simuNetwork(odenet, seq(0, 40, by = 0.01))
calcResonances(odenet)
odenet$state
plot(odenet)
plot(odenet, state = "1")
plot(odenet, state = "2")
plot(odenet, state = "1vs2")
```
The method `setEvents` fixes the position of the first oscillator to given values between 10s and 15s.
In this example we chose a linear connection between the three time points.
```R
eventdata <- data.frame(var = c("x.1", "x.1", "x.1")
                        , time = c(10, 13, 15)
                        , value = c(0, 1, 1)
)
odenet <- setEvents(odenet, eventdata, type = "linear")
odenet <- simuNetwork(odenet, seq(0, 40, by = 0.01))
plot(odenet)
plot(odenet, state = "1")
plot(odenet, state = "2")
plot(odenet, state = "1vs2")
```

## Contact
Please use the [issue tracker](https://github.com/surmann/ODEnetwork/issues) for any suggestions, bugs or hints.

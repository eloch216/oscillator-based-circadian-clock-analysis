## Load and process the WARM weather data
source("process_WARM.R")

## Load and process the SURFRAD weather data
source("process_SURFRAD.R")

## Generate figures s1, s2, s3, and s4 and tables s1 and s2, which pertain to
## the weather data used as an input to the oscillator clock.
source("analyze_weather.R")

## Generate figures 2 and s5, which pertain to Poincare oscillators.
source("analyze_oscillators.R")

## Generate figures 1b, 3a, and 3b, which pertain to the coupling between the
## oscillator clock and its light environment
source("analyze_coupling.R")

## Generate figures 4a and 4b, which pertain to the response of an oscillator
## clock to its initial conditions or sudden changes in photoperiod length
source("analyze_response.R")

## Generate figures 1c, 5a, 5b, s6, s7, and s8, which pertain to the accuracy
## and stability of the oscillator clock
source("analyze_accuracy.R")

## Generate figures s3, 6a, 6b, and s9 and table s3, which pertain to the
## CROPGRO model. Note: table s3 may take a long time (multiple hours)
source("analyze_cropgro.R")

## Generate figures s11, 7a, and 7b, which pertain to a simple development model
## based on growing degree days (GDDs).
source("analyze_gdd.R")

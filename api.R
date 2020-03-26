library(plumber)
#* @get /normalDistribution
normalDistribution <- function(n = 10) {
  rnorm(n)
}
context("Testing Performances Computations")
library(dplyr)
library(VCA)

test_that("Precision works", {
  data(Glucose,package="VCA")
  expect_equal(round(perfPrecision(Glucose, 'result','run', 'day')[1,1],2),round(64.77732,2))
})
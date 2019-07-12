context("Testing Performances Computations")

test_that("Precision works", {
  library(tidyverse)
  library(VCA)
  data(Glucose,package="VCA")
  expect_equal(round(perfRepet(Glucose, 'result','run', 'day')[1,1],2),round(64.77732,2))
})


test_that("Plotting Precision works",{
  library(tidyverse)
  library(VCA)
  data(Glucose,package="VCA")
  
  p <- plotPrecision(Glucose, 'result','run', 'day')
  expect_identical(p$labels$x, "run")
  expect_identical(p$labels$y, "result")
  
})
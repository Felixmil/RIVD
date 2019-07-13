context('plots')
library(dplyr)
library(ggplot2)
library(VCA)


test_that("Plotting Precision works",{
  
  
  data(Glucose,package="VCA")
  
  p <- plotPrecision(Glucose, 'result','run', 'day')
  expect_identical(p$labels$x, "run")
  expect_identical(p$labels$y, "result")
  
})
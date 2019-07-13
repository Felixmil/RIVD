context("Testing Performances Computations")
library(dplyr)
library(VCA)

test_that("Precision works", {
  data(Glucose,package="VCA")
  Glucose2 <- rbind(Glucose, Glucose)
  Glucose2['lot'] <- rep(c('A','B'), each=80)
  expect_equal(round(perfPrecision(Glucose, 'result','run', 'day')[1,1],2),round(64.77732,2))
  expect_condition(names(perfPrecision(Glucose, 'result','run', 'day', by='lot')[2]) == "lot.B")
  
  
})
context("Testing Performances Computations")
library(dplyr)
library(VCA)

test_that("Precision works", {
  data(Glucose,package="VCA")
  Glucose2 <- rbind(Glucose, Glucose)
  Glucose2['lot'] <- rep(c('A','B'), each=80)
  Glucose3 <- cbind(Glucose2, site = rep(c('site1','site2'), each=20))
  expect_equal(round(perfPrecision(Glucose, 'result','run', 'day')[1,1],2),round(64.77732,2))
  expect_equal(names(perfPrecision(Glucose2, 'result','run', 'day', by='lot')[2]), "lot.B")
  expect_equal(round(perfPrecision(Glucose3, 'result','run', 'day', site= 'site')['repeatability','CV[%]'],2), round(1.461133,2))
  
})
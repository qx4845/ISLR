LoadStuff = function(){
  setwd("~/Dropbox/DataScience/Bayer/ISLR")
  library(ISLR) ## Load ISLR library
  library(MASS) ## Load Boston data frame
  Auto = read.csv("~/Dropbox/DataScience/Bayer/ISLR/Auto.csv", header=T, na.strings="?")
  Auto = na.omit(Auto)
  library(boot)
  library(leaps)
  Hitters = na.omit(Hitters)
  library(pls)
  library(splines)
  library(gam)
#  install.packages("akima",repos="http://cran.r-project.org",depend=T)
  library(akima)
  library(tree)
  library(randomForest)
  library(gbm)
  library(e1071)
  library(LiblineaR)
}

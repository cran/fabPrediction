## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----hiddenlibrary, include = FALSE-------------------------------------------
#  library(devtools)

## ----devtools-----------------------------------------------------------------
#  devtools::install_github("betsybersson/fabPrediction")
#  library(fabPrediction)

## ----cran, eval = FALSE-------------------------------------------------------
#  install_packages("fabPrediction")
#  library(fabPrediction)

## ----data---------------------------------------------------------------------
#  data(radon)
#  data(W)

## ----onecounty----------------------------------------------------------------
#  y_county9 = radon$radon[radon$group==9]

## ----fabcontinuous------------------------------------------------------------
#  fab.region = predictionInterval(y_county9,method = "FAB",
#                            alpha = .15,
#                            mu = 0.5,tau2 = 1)
#  fab.region$bounds

## ----plotfab, fig.height=4, fig.width = 6,warning=F---------------------------
#  plot(fab.region,
#       main="FAB Prediction Interval For County 9",xlab="log(radon)")

## ----plotdta, fig.height=4, fig.width = 6,warning=F---------------------------
#  plot(predictionInterval(y_county9,method = "DTA",alpha = .15),
#       main="DTA Prediction Interval For County 9",xlab="log(radon)")

## ----parametric,warning=F-----------------------------------------------------
#  predictionInterval(y_county9,method = "direct",alpha = .15)$bounds
#  predictionInterval(y_county9,method = "Bayes",alpha = .15,
#                     mu=0.5, tau2=1)$bounds

## ----county9, fig.height=4, fig.width = 6,warning=F---------------------------
#  params = fayHerriotEB(9,radon$radon,radon$group,W,X=rep(1,nrow(W)))
#  plot(predictionInterval(y_county9,method = "FAB",alpha = .15,
#                          mu = params$mu, tau2 = params$tau2),
#       main="FAB Prediction Interval For County 9 Using Indirect Information", xlab="log(radon)")

## ----catdata------------------------------------------------------------------
#  N.groups = c(10,50,75,100,150)
#  
#  set.seed(1)
#  prob = rdirichlet(50:1)
#  y = t(sapply(N.groups,function(j)rmultinom(1,j,prob)))

## ----group3-------------------------------------------------------------------
#  y_group3 = y[3,]
#  fab.set = predictionSet(y_group3,method = "FAB",
#                          gamma = c(50:1))

## ----group3plot, fig.height=4, fig.width = 6,warning=F------------------------
#  plot(fab.set, main = "FAB Prediction Set for Group 3",
#       cex.axis=.5)

## ----group3directplot, fig.height=4, fig.width = 6,warning=F------------------
#  plot(predictionSet(y_group3,method = "direct"),
#       main = "Direct Prediction Set for Group 3",
#       cex.axis=.5)

## ----group3bayesplot, fig.height=4, fig.width = 6,warning=F-------------------
#  plot(predictionSet(y_group3,method = "Bayes",gamma = c(50:1)),
#       main = "Bayes Prediction Set for Group 3",
#       cex.axis=.5)

## ----computeMLE, cache=T------------------------------------------------------
#  gamma0 = polyaMLE(y[-3,], method="separate")

## ----plotFABcat, fig.height=4, fig.width = 6,warning=F------------------------
#  plot(predictionSet(y_group3,method = "FAB",gamma = gamma0),
#       main = "FAB Prediction Set for Group 3 using Indirect Information",
#       cex.axis=.5)


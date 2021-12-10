# Setup for some high dimensional simulations ----------------------------------
library(dbarts)
library(Rforestry)
library(ggplot2)
library(dplyr)
library(reshape)


# Logistic regression ----------------------------------------------------------
lr <- function(
  X_train,
  Tr_train
) {
  fit <- glm(Tr_train ~.,
             data = data.frame(X_train,
                               Tr_train = Tr_train),
             family = "binomial")

  ps_estimate <- predict(fit,
                         newdata = data.frame(X_train),
                         type = "response")
  ps_estimate <- ifelse(ps_estimate > .99, .99,
                        ifelse(ps_estimate < .01, .01,
                               ps_estimate))
  return(unname(ps_estimate))
}

# Random forest ----------------------------------------------------------------
rf <- function(
  X_train,
  Tr_train
) {
  library(Rforestry)
  fit <- forestry(x = data.frame(X_train),
                  y = Tr_train,
                  OOBhonest= TRUE)

  ps_estimate <- predict(fit,
                         newdata = data.frame(X_train),
                         aggregation = "oob"
  )
  ps_estimate <- ifelse(ps_estimate > .99, .99,
                        ifelse(ps_estimate < .01, .01,
                               ps_estimate))
  return(ps_estimate)
}

bt <- function(
  X_train,
  Tr_train
) {
  library(dbarts)

  fit <- bart(x.train = data.frame(X_train),
              y.train = Tr_train,
              verbose = FALSE,
              keeptrees = TRUE)

  ps_estimate <- predict(fit, data.frame(X_train))
  return(apply(ps_estimate, 2, mean))
}


stacked_rf <- function(
  X_train,
  Tr_train
) {
  library(Rforestry)

  fit <- forestry(x = data.frame(X_train),
                  y = Tr_train,
                  OOBhonest= TRUE)
  preds <- predict(fit,
                   newdata = data.frame(X_train),
                   aggregation = "oob")

  # Get the indices and weights for each observation
  fit <- glm(Tr_train ~.,
             data = data.frame(X_train = preds,
                               Tr_train = Tr_train),
             family = "binomial")

  ps_estimate <- unname(predict(fit,
                                newdata = data.frame(X_train = preds),
                                type = "response"))


  return(ps_estimate)
}

Xfun <- function(n, d) {
  matrix(runif(n * d), nrow = n, ncol = d)
}

nonlinear <- function(X) {
  ps <- 1 / (1 + exp(-12 * (X[, 1] - 0.5))) * 1 / (1 + exp(-12 * (X[, 2] - 0.5)))
  ps <- ifelse(ps < .1,.1, ifelse(ps > .9,.9,ps))
  return(ps)
}


linear <- function(X) {
  beta <- c(runif(10,min=0,max = .1), rep(0,90))
  ps <- X %*% beta
  return(ps[,1])
}

truncated <- function(X) {
  beta <- c(runif(10,min=0,max = .2), rep(0,90))
  ps <- X %*% beta
  ps <- ifelse(ps < unname(quantile(ps, probs = c(.2))), unname(quantile(ps, probs = c(.2))),
               ifelse(ps > unname(quantile(ps, probs = c(.8))), unname(quantile(ps, probs = c(.2))), ps))
  return(ps[,1])
}





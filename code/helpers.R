library(dbarts)
library(Rforestry)
library(ggplot2)
library(dplyr)
library(reshape)

source("code/helpers_analysis.R")

# Evaluate P score functions ---------------------------------------------------
smooth_ps <- function(x) {
  probs <- .2*sin(15*x)+.4*x+.1
  return(probs)
}

linear_ps <- function(x) {
  probs <-  .5*x + .1
  probs <-  min(max(probs,.01), .99)

  logit <- function(x){return(exp(x)/(1+exp(x)))}
  return(sapply(probs, logit))
}

nonlinear_ps <- function(x) {
  # For now we keep the number of bins to be 100 X N depending on whatever N is
  n_bins <- round(100*length(x))
  bin_width <- 1/n_bins
  probs <- ifelse(x %% bin_width < (bin_width/2),
                  .9,
                  .1)

  return(probs)
}

constant_ps <- function(x) {
  # For now we keep the number of bins to be 100 X N depending on whatever N is
  probs <- rep(.3, length(x))

  return(probs)
}

smooth_ps <- function(x) {
  probs <- .2*sin(15*x)+.4*x+.1
  return(probs)
}

# Horvitz thompson estimator for the ATE ---------------------------------------
ht <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(iWeigReg)

  estimate <- ate.HT(y = Y_train,
                     tr = Tr_train,
                     p = p_scores)
  return(estimate$mu[1])
}

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
                  saveable = TRUE,
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

xgb <- function(
  X_train,
  Tr_train
) {
  # Fit the model
  fit <- xgb_helper(Xobs = X_train,
                    Yobs = Tr_train)

  preds <- xgb_predict(estimator = fit, feat = X_train)
  return(preds)
}

# Estimate the treatment effect with the given method and model
est_te <- function(
  p_score,
  reg_func,
  true_p=FALSE,
  n=1000
) {
  X <- runif(n)
  p_scores <- sapply(X, p_score)
  Tr <- sapply(p_scores, function(x){return(rbinom(1,1,prob = x))})
  Y <- sapply(X, p_score)
  Y <- sapply(Y, function(x){return(rbinom(1,1,prob = x))})
  Y <- ifelse(Tr, Y,0)

  p_est <- reg_func(X_train = X,
                    Tr_train = Tr)


  te_est <- ht(X_train = X,
               Tr_train = Tr,
               Y_train = Y,
               p_scores = ifelse(true_p, p_scores, p_est))
  return(te_est)
}




# Plotting function ------------------------------------------------------------
plot_preds <- function(
  ps_func,
  title
) {
  set.seed(24892)
  X <- runif(1000)
  p_scores <- sapply(X, ps_func)
  Tr <- sapply(p_scores, function(x){return(rbinom(1,1,prob=x))})

  est_rf <- rf(X, Tr)
  est_lr <- lr(X, Tr)
  est_bt <- bt(X, Tr)
  est_cr <- stacked_rf(X, Tr)
  est_xgb <- xgb(data.frame(V1 = X),Tr)


  summary(est_rf)
  summary(est_lr)
  summary(est_bt)
  summary(est_cr)
  summary(p_scores)

  # Plot the quantiles of the predicted propensity scores against those of the true PS
  p <- 1:10/10-.05
  data.frame(BART = quantile(est_bt, probs = p),
             RandomForest = quantile(est_rf, probs = p),
             LogisticRegression = quantile(est_lr, probs = p),
             CorrectedRF = quantile(est_cr, probs = p),
             XGboost = quantile(est_xgb, probs = p),
             TruePropensityScore = quantile(p_scores, probs = p),
             prob = p) %>%
    melt("prob") %>%
    dplyr::rename(Estimator = variable) %>%
    ggplot(aes(x = prob, y = value, color = Estimator)) +
    geom_line()+
    scale_color_viridis_d()+
    theme_classic()+
    labs(title = title, x = "Quantile of Propensity Score",y="Predicted Value") -> p1


  sqrt(mean((est_rf - p_scores)**2))
  sqrt(mean((est_lr - p_scores)**2))
  sqrt(mean((est_bt - p_scores)**2))
  sqrt(mean((est_cr - p_scores)**2))
  return(p1)
}

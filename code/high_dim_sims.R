library(dbarts)
library(Rforestry)
library(ggplot2)
library(dplyr)
library(reshape)
library(xtable)

source("~/Desktop/propensityEstimationML/code/helpers_high.R")
source("~/Desktop/propensityEstimationML/code/helpers.R")

results <- data.frame(Estimator = c("BART","Random Forest","Logistic Regression",
                                    "Corrected Random Forest","XGboost"))


save = FALSE
# Plot Linear Propensity Scores ------------------------------------------------
p_func <- linear
title <- "Linear High Dimensional Simulation"

set.seed(24892)
X <- Xfun(n=1000,d=100)
colnames(X) <- paste0("V",1:ncol(X))
p_scores <- p_func(X)
Tr <- sapply(p_scores, function(x){return(rbinom(1,1,prob=x))})

est_rf <- rf(X, Tr)
est_lr <- lr(X, Tr)
est_bt <- bt(X, Tr)
est_cr <- stacked_rf(X, Tr)
est_xgb <- xgb(X, Tr)


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
  labs(title = title, x = "Quantile of Propensity Score",y="Predicted Value")->p1

plot(p1)
if (save) {
  ggsave(filename = "~/Desktop/p_est_project/figures/hd_linear.pdf", height = 4, width = 7)
}


results$Linear <- c(sqrt(mean((est_bt - p_scores)**2)), sqrt(mean((est_rf - p_scores)**2)),
                    sqrt(mean((est_lr - p_scores)**2)), sqrt(mean((est_cr - p_scores)**2)), sqrt(mean((est_xgb - p_scores)**2)))

# Plot nonlinear propensity score ----------------------------------------------
p_func <- nonlinear
title <- "Non Linear High Dimensional Simulation"

set.seed(24892)
X <- Xfun(n=1000,d=100)
colnames(X) <- paste0("V",1:ncol(X))
p_scores <- p_func(X)
Tr <- sapply(p_scores, function(x){return(rbinom(1,1,prob=x))})

est_rf <- rf(X, Tr)
est_lr <- lr(X, Tr)
est_bt <- bt(X, Tr)
est_cr <- stacked_rf(X, Tr)
est_xgb <- xgb(X, Tr)



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
  labs(title = title, x = "Quantile of Propensity Score",y="Predicted Value")->p1

plot(p1)
if (save){
  ggsave(filename = "~/Desktop/p_est_project/figures/hd_nonlinear.pdf", height = 4, width = 7)
}


results$Nonlinear <- c(sqrt(mean((est_bt - p_scores)**2)), sqrt(mean((est_rf - p_scores)**2)),
                       sqrt(mean((est_lr - p_scores)**2)), sqrt(mean((est_cr - p_scores)**2)), sqrt(mean((est_xgb - p_scores)**2)))



# Plot truncated propensity score ----------------------------------------------
p_func <- truncated
title <- "Truncated Linear High Dimensional Simulation"

set.seed(24892)
X <- Xfun(n=1000,d=100)
colnames(X) <- paste0("V",1:ncol(X))
p_scores <- p_func(X)
Tr <- sapply(p_scores, function(x){return(rbinom(1,1,prob=x))})

est_rf <- rf(X, Tr)
est_lr <- lr(X, Tr)
est_bt <- bt(X, Tr)
est_cr <- stacked_rf(X, Tr)
est_xgb <- xgb(X, Tr)


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
  labs(title = title, x = "Quantile of Propensity Score",y="Predicted Value")->p1

plot(p1)
if (save) {
  ggsave(filename = "~/Desktop/p_est_project/figures/hd_truncated.pdf", height = 4, width = 7)
}


results$TruncatedLinear <- c(sqrt(mean((est_bt - p_scores)**2)), sqrt(mean((est_rf - p_scores)**2)),
                             sqrt(mean((est_lr - p_scores)**2)), sqrt(mean((est_cr - p_scores)**2)), sqrt(mean((est_xgb - p_scores)**2)))


# Make latex plot
xtable(results, digits = 4,caption = "RMSE of various algorithms in the three high dimensional simulations")


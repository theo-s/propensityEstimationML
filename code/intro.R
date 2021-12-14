library(dbarts)
library(Rforestry)
library(ggplot2)
library(dplyr)
library(reshape)

source("~/Desktop/propensityEstimationML/code/helpers.R")

smooth_ps <- function(x) {
  probs <- .2*sin(15*x)+.4*x+.1
  return(probs)
}
bt <- function(
  X_train,
  Tr_train
) {
  library(dbarts)

  fit <- bart(x.train = data.frame(X_train),
              y.train = Tr_train,
              verbose = FALSE,
              keeptrees = TRUE,
              power=3,
              base = .8,
              ntree=10
              )

  ps_estimate <- predict(fit, data.frame(X_train))
  return(apply(ps_estimate, 2, mean))
}

xgb <- function(
  X_train,
  Tr_train
) {
  # Fit the model
  fit <- xgb_helper(Xobs = X_train,
                    Yobs = Tr_train,
                    tune_length = 10)

  preds <- xgb_predict(estimator = fit, newdata = X_train)
  return(preds)
}



x <- runif(1000)
pscores <- sapply(x, smooth_ps)
Tr <- sapply(pscores, function(x){return(rbinom(1,1,prob=x))})

p_est_rf <- rf(X_train = x,
               Tr_train = Tr)

p_est_bart <- bt(X_train = x,
                Tr_train = Tr)

p_est_xgb <- xgb(X_train = data.frame(X_train = x),
                 Tr_train = Tr)


data.frame(RF = p_est_rf,
           #BART = p_est_bart,
           XGboost = p_est_xgb,
           TruePropensityScore = pscores,
           X = x) %>%
  melt(id = "X") %>%
  dplyr::rename(Estimator = variable) %>%
  ggplot(aes(x = X, y = value, color = Estimator)) +
  geom_point(alpha = .5)+
  scale_color_viridis_d()+
  theme_classic()+
  labs(y = "Estimated Propensity Score", x = "X1")
ggsave("~/Desktop/propensityEstimationML/figures/intro.pdf", width = 8, height = 5)




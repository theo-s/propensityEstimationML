library(Rforestry)
library(xgboost)
library(dplyr)
library(causalToolbox) # For data generation
library(caret) # For create_folds


source("~/Desktop/propensityEstimationML/code/helpers_analysis.R")
# Define the DR learner from https://arxiv.org/pdf/2004.14497.pdf.
# This contains two versions, one using Bayesian Additive Regression Trees
# for the base learners, dnd one using Random Forest for the base learners

# Wrapper for RF to pred with several learners
rf_DR_pred <- function(estimator_list, newdata) {
  preds_1 <- predict(object = estimator_list[[1]], newdata = newdata)
  preds_2 <- predict(object = estimator_list[[2]], newdata = newdata)
  preds_3 <- predict(object = estimator_list[[3]], newdata = newdata)

  return(apply(rbind(preds_1,preds_2,preds_3), 2, mean))
}

# Wrapper to get DR preds from XGboost based DR learner
xgb_DR_pred <- function(estimator_list, newdata) {
  preds_1 <- xgb_predict(estimator = estimator_list[[1]], newdata = newdata)
  preds_2 <- xgb_predict(estimator = estimator_list[[2]], newdata = newdata)
  preds_3 <- xgb_predict(estimator = estimator_list[[3]], newdata = newdata)

  return(apply(rbind(preds_1,preds_2,preds_3), 2, mean))
}

# Implementation of the DR learner using XGboost
# for the base learners. trunc_level controls the truncation level for the
# estimated propensity score. Values below trunc_level are set to trunc_level,
# and values above 1-trunc_level are set to 1-trunc_level
DR_Learner_XGB <- function(feature_train,
                           w_train,
                           yobs_train,
                           corrected=FALSE,
                           trunc_level = .05) {
  library(dplyr)
  library(caret)

  print("Running DR Learner- BART base learners")
  # First do data splitting
  initial_split <-  caret::createFolds(yobs_train, k = 3)

  # First fold ================================================================
  # Nuisance training
  # Propensity Score regression using fold 1
  print("Fitting Propensity Score Regression")
  # Phase 3 Pseudo outcome regression
  pseudo_reg_data <- feature_train[initial_split$Fold3,]
  pseudo_out <- yobs_train[initial_split$Fold3]
  pseudo_ind <- w_train[initial_split$Fold3]


  if (corrected) {
    reg_p_score <- stacked_xgb(feature_train[initial_split$Fold1,],
                               w_train[initial_split$Fold1],
                               pseudo_reg_data
                               )
  } else {
    propensity_reg <- xgb_helper(Xobs = feature_train[initial_split$Fold1,],
                                 Yobs = w_train[initial_split$Fold1]
    )
    # trunc_level <- .02
    reg_p_score <- xgb_predict(propensity_reg, newdata = pseudo_reg_data)
  }

  print(summary(reg_p_score))
  # Outcome Regression using fold 2
  outcome_reg_data <- feature_train[initial_split$Fold2,]
  outcome_reg_outcome <- yobs_train[initial_split$Fold2]

  treat_idx <- which(w_train[initial_split$Fold2] == 1)
  contr_idx <- which(w_train[initial_split$Fold2] == 0)

  print("Fitting Treatment Outcome Regression")
  treat_reg <- xgb_helper(Xobs = outcome_reg_data[treat_idx,],
                          Yobs = outcome_reg_outcome[treat_idx]
                          )

  print("Fitting Control Outcome Regression")
  contr_reg <- xgb_helper(Xobs = outcome_reg_data[contr_idx,],
                          Yobs = outcome_reg_outcome[contr_idx]
                          )


  # Make sure we truncate the estimated propensity score for stability
  data.frame(X1 = reg_p_score) %>% dplyr::mutate(X1 = case_when(X1 < trunc_level ~ trunc_level,
                                                                X1 > 1-trunc_level ~ 1-trunc_level,
                                                                TRUE ~ X1)) %>%
    dplyr::select(X1) -> truncated_reg_pscore

  trunc_p <- truncated_reg_pscore[,1]
  c_pred <- xgb_predict(contr_reg, newdata = pseudo_reg_data)
  t_pred <- xgb_predict(treat_reg, newdata = pseudo_reg_data)
  tailored_pred <- ifelse(pseudo_ind,
                          t_pred,
                          c_pred)

  # Now create the pseudo outcome
  print("Fitting Pseudo Outcome Regression")
  pseudo_outcome <- ((pseudo_ind - trunc_p) / (trunc_p*(1-trunc_p)))*(pseudo_out - tailored_pred) + t_pred - c_pred
  pseudo_outcome_reg_1 <- xgb_helper(Xobs = pseudo_reg_data,
                                     Yobs = pseudo_outcome
                                     )

  # Second fold ================================================================
  # Propensity Score regression using fold 3
  print("Fitting Propensity Score Regression")
  propensity_reg <- xgb_helper(Xobs = feature_train[initial_split$Fold3,],
                               Yobs = w_train[initial_split$Fold3]
                               )

  # Outcome Regression using fold 2
  outcome_reg_data <- feature_train[initial_split$Fold1,]
  outcome_reg_outcome <- yobs_train[initial_split$Fold1]

  treat_idx <- which(w_train[initial_split$Fold1] == 1)
  contr_idx <- which(w_train[initial_split$Fold1] == 0)

  print("Fitting Treatment Outcome Regression")
  treat_reg <- xgb_helper(Xobs = outcome_reg_data[treat_idx,],
                          Yobs = outcome_reg_outcome[treat_idx]
                          )

  print("Fitting Control Outcome Regression")
  contr_reg <- xgb_helper(Xobs = outcome_reg_data[contr_idx,],
                          Yobs = outcome_reg_outcome[contr_idx]
                          )

  # Phase 3 Pseudo outcome regression
  pseudo_reg_data <- feature_train[initial_split$Fold2,]
  pseudo_out <- yobs_train[initial_split$Fold2]
  pseudo_ind <- w_train[initial_split$Fold2]

  # trunc_level <- .02
  reg_p_score <- xgb_predict(propensity_reg, newdata = pseudo_reg_data)

  # Make sure we truncate the estimated propensity score for stability
  data.frame(X1 = reg_p_score) %>% dplyr::mutate(X1 = case_when(X1 < trunc_level ~ trunc_level,
                                                                X1 > 1-trunc_level ~ 1-trunc_level,
                                                                TRUE ~ X1)) %>%
    dplyr::select(X1) -> truncated_reg_pscore

  trunc_p <- truncated_reg_pscore[,1]
  c_pred <- xgb_predict(contr_reg, newdata = pseudo_reg_data)
  t_pred <- xgb_predict(treat_reg, newdata = pseudo_reg_data)
  tailored_pred <- ifelse(pseudo_ind,
                          t_pred,
                          c_pred)

  # Now create the pseudo outcome
  print("Fitting Pseudo Outcome Regression")
  pseudo_outcome <- ((pseudo_ind - trunc_p) / (trunc_p*(1-trunc_p)))*(pseudo_out - tailored_pred) + t_pred - c_pred
  pseudo_outcome_reg_2 <- xgb_helper(Xobs = pseudo_reg_data,
                                     Yobs = pseudo_outcome
                                     )

  # Third fold ================================================================
  # Propensity Score regression using fold 3
  print("Fitting Propensity Score Regression")
  propensity_reg <- xgb_helper(Xobs = feature_train[initial_split$Fold2,],
                               Yobs = w_train[initial_split$Fold2]
                               )

  # Outcome Regression using fold 2
  outcome_reg_data <- feature_train[initial_split$Fold3,]
  outcome_reg_outcome <- yobs_train[initial_split$Fold3]

  treat_idx <- which(w_train[initial_split$Fold3] == 1)
  contr_idx <- which(w_train[initial_split$Fold3] == 0)

  print("Fitting Treatment Outcome Regression")
  treat_reg <- xgb_helper(Xobs = outcome_reg_data[treat_idx,],
                          Yobs = outcome_reg_outcome[treat_idx]
                          )

  print("Fitting Control Outcome Regression")
  contr_reg <- xgb_helper(Xobs = outcome_reg_data[contr_idx,],
                          Yobs = outcome_reg_outcome[contr_idx]
                          )

  # Phase 3 Pseudo outcome regression
  pseudo_reg_data <- feature_train[initial_split$Fold1,]
  pseudo_out <- yobs_train[initial_split$Fold1]
  pseudo_ind <- w_train[initial_split$Fold1]

  # trunc_level <- .02
  reg_p_score <- xgb_predict(propensity_reg, newdata = pseudo_reg_data)

  # Make sure we truncate the estimated propensity score for stability
  data.frame(X1 = reg_p_score) %>% dplyr::mutate(X1 = case_when(X1 < trunc_level ~ trunc_level,
                                                                X1 > 1-trunc_level ~ 1-trunc_level,
                                                                TRUE ~ X1)) %>%
    dplyr::select(X1) -> truncated_reg_pscore

  trunc_p <- truncated_reg_pscore[,1]
  c_pred <- xgb_predict(contr_reg, newdata = pseudo_reg_data)
  t_pred <- xgb_predict(treat_reg, newdata = pseudo_reg_data)
  tailored_pred <- ifelse(pseudo_ind,
                          t_pred,
                          c_pred)

  # Now create the pseudo outcome
  print("Fitting Pseudo Outcome Regression")
  pseudo_outcome <- ((pseudo_ind - trunc_p) / (trunc_p*(1-trunc_p)))*(pseudo_out - tailored_pred) + t_pred - c_pred
  pseudo_outcome_reg_3 <- xgb_helper(Xobs = pseudo_reg_data,
                                     Yobs = pseudo_outcome
                                     )

  return(list(pseudo_outcome_reg_1,pseudo_outcome_reg_2,pseudo_outcome_reg_3))
}


# Implementation of the DR learner using Random Forest, as implemented in the
# Rforestry package as the base learner
DR_Learner_RF <- function(feature_train,
                          w_train,
                          yobs_train,
                          corrected = FALSE,
                          trunc_level = .05) {
  library(Rforestry)
  library(dplyr)
  library(caret)

  print("Running DR Learner- RF base learners")
  # First do data splitting
  initial_split <-  caret::createFolds(yobs_train, k = 3)

  # Nuisance training
  # First fold =================================================================
  # Propensity Score regression using fold 1
  print("Fitting Propensity Score Regression")
  # trunc_level <- .02
  # Phase 3 Pseudo outcome regression
  pseudo_reg_data <- feature_train[initial_split$Fold3,]
  pseudo_out <- yobs_train[initial_split$Fold3]
  pseudo_ind <- w_train[initial_split$Fold3]

  if (corrected) {
    reg_p_score <- stacked_rf(X_train = feature_train[initial_split$Fold1,],
                              Tr_train = w_train[initial_split$Fold1],
                              X_test = pseudo_reg_data)
  } else {
    propensity_reg <- forestry(x = feature_train[initial_split$Fold1,],
                               y = w_train[initial_split$Fold1],
                               nthread = 0,
                               saveable = TRUE,
                               mtry = max(floor(ncol(feature_train[initial_split$Fold1,]) / 3), 2),
                               nodesizeStrictSpl = 20)
    reg_p_score <- predict(object = propensity_reg, newdata = pseudo_reg_data)
  }

  print(summary(reg_p_score))


  # Outcome Regression using fold 2
  outcome_reg_data <- feature_train[initial_split$Fold2,]
  outcome_reg_outcome <- yobs_train[initial_split$Fold2]

  treat_idx <- which(w_train[initial_split$Fold2] == 1)
  contr_idx <- which(w_train[initial_split$Fold2] == 0)

  print("Fitting Treatment Outcome Regression")
  treat_reg <- forestry(x = outcome_reg_data[treat_idx,],
                        y = outcome_reg_outcome[treat_idx],
                        nthread = 0,
                        saveable = TRUE,
                        mtry = max(floor(ncol(outcome_reg_data[treat_idx,]) / 3), 2),
                        nodesizeStrictSpl = 20)

  print("Fitting Control Outcome Regression")
  contr_reg <- forestry(x = outcome_reg_data[contr_idx,],
                        y = outcome_reg_outcome[contr_idx],
                        nthread = 0,
                        saveable = TRUE,
                        mtry = max(floor(ncol(outcome_reg_data[contr_idx,]) / 3), 2),
                        nodesizeStrictSpl = 20)





  # Make sure we truncate the estimated propensity score for stability
  data.frame(X1 = reg_p_score) %>% dplyr::mutate(X1 = case_when(X1 < trunc_level ~ trunc_level,
                                                                X1 > 1-trunc_level ~ 1-trunc_level,
                                                                TRUE ~ X1)) %>%
    dplyr::select(X1) -> truncated_reg_pscore

  trunc_p <- truncated_reg_pscore[,1]
  c_pred <- predict(contr_reg, newdata = pseudo_reg_data)
  t_pred <- predict(treat_reg, newdata = pseudo_reg_data)
  tailored_pred <- ifelse(pseudo_ind,
                          t_pred,
                          c_pred)

  # Now create the pseudo outcome
  print("Fitting Pseudo Outcome Regression")
  pseudo_outcome <- ((pseudo_ind - trunc_p) / (trunc_p*(1-trunc_p)))*(pseudo_out - tailored_pred) + t_pred - c_pred
  pseudo_outcome_reg_1 <- forestry(x = pseudo_reg_data,
                                   y = pseudo_outcome,
                                   nthread = 0,
                                   saveable = TRUE,
                                   mtry = max(floor(ncol(pseudo_reg_data) / 3), 2),
                                   nodesizeStrictSpl = 20)

  # Second fold =================================================================
  # Propensity Score regression using fold 1
  print("Fitting Propensity Score Regression")
  propensity_reg <- forestry(x = feature_train[initial_split$Fold3,],
                             y = w_train[initial_split$Fold3],
                             nthread = 0,
                             saveable = TRUE,
                             mtry = max(floor(ncol(feature_train[initial_split$Fold1,]) / 3), 2),
                             nodesizeStrictSpl = 20)

  # Outcome Regression using fold 2
  outcome_reg_data <- feature_train[initial_split$Fold1,]
  outcome_reg_outcome <- yobs_train[initial_split$Fold1]

  treat_idx <- which(w_train[initial_split$Fold1] == 1)
  contr_idx <- which(w_train[initial_split$Fold1] == 0)

  print("Fitting Treatment Outcome Regression")
  treat_reg <- forestry(x = outcome_reg_data[treat_idx,],
                        y = outcome_reg_outcome[treat_idx],
                        nthread = 0,
                        saveable = TRUE,
                        mtry = max(floor(ncol(outcome_reg_data[treat_idx,]) / 3), 2),
                        nodesizeStrictSpl = 20)

  print("Fitting Control Outcome Regression")
  contr_reg <- forestry(x = outcome_reg_data[contr_idx,],
                        y = outcome_reg_outcome[contr_idx],
                        nthread = 0,
                        saveable = TRUE,
                        mtry = max(floor(ncol(outcome_reg_data[contr_idx,]) / 3), 2),
                        nodesizeStrictSpl = 20)

  # Phase 3 Pseudo outcome regression
  pseudo_reg_data <- feature_train[initial_split$Fold2,]
  pseudo_out <- yobs_train[initial_split$Fold2]
  pseudo_ind <- w_train[initial_split$Fold2]

  # trunc_level <- .02
  reg_p_score <- predict(propensity_reg, newdata = pseudo_reg_data)

  # Make sure we truncate the estimated propensity score for stability
  data.frame(X1 = reg_p_score) %>% dplyr::mutate(X1 = case_when(X1 < trunc_level ~ trunc_level,
                                                                X1 > 1-trunc_level ~ 1-trunc_level,
                                                                TRUE ~ X1)) %>%
    dplyr::select(X1) -> truncated_reg_pscore

  trunc_p <- truncated_reg_pscore[,1]
  c_pred <- predict(contr_reg, newdata = pseudo_reg_data)
  t_pred <- predict(treat_reg, newdata = pseudo_reg_data)
  tailored_pred <- ifelse(pseudo_ind,
                          t_pred,
                          c_pred)

  # Now create the pseudo outcome
  print("Fitting Pseudo Outcome Regression")
  pseudo_outcome <- ((pseudo_ind - trunc_p) / (trunc_p*(1-trunc_p)))*(pseudo_out - tailored_pred) + t_pred - c_pred
  pseudo_outcome_reg_2 <- forestry(x = pseudo_reg_data,
                                   y = pseudo_outcome,
                                   nthread = 0,
                                   saveable = TRUE,
                                   mtry = max(floor(ncol(pseudo_reg_data) / 3), 2),
                                   nodesizeStrictSpl = 20)

  # Third fold =================================================================
  # Propensity Score regression using fold 1
  print("Fitting Propensity Score Regression")
  propensity_reg <- forestry(x = feature_train[initial_split$Fold2,],
                             y = w_train[initial_split$Fold2],
                             nthread = 0,
                             saveable = TRUE,
                             mtry = max(floor(ncol(feature_train[initial_split$Fold1,]) / 3), 2),
                             nodesizeStrictSpl = 20)

  # Outcome Regression using fold 2
  outcome_reg_data <- feature_train[initial_split$Fold3,]
  outcome_reg_outcome <- yobs_train[initial_split$Fold3]

  treat_idx <- which(w_train[initial_split$Fold3] == 1)
  contr_idx <- which(w_train[initial_split$Fold3] == 0)

  print("Fitting Treatment Outcome Regression")
  treat_reg <- forestry(x = outcome_reg_data[treat_idx,],
                        y = outcome_reg_outcome[treat_idx],
                        nthread = 0,
                        saveable = TRUE,
                        mtry = max(floor(ncol(outcome_reg_data[treat_idx,]) / 3), 2),
                        nodesizeStrictSpl = 20)

  print("Fitting Control Outcome Regression")
  contr_reg <- forestry(x = outcome_reg_data[contr_idx,],
                        y = outcome_reg_outcome[contr_idx],
                        nthread = 0,
                        saveable = TRUE,
                        mtry = max(floor(ncol(outcome_reg_data[contr_idx,]) / 3), 2),
                        nodesizeStrictSpl = 20)

  # Phase 3 Pseudo outcome regression
  pseudo_reg_data <- feature_train[initial_split$Fold1,]
  pseudo_out <- yobs_train[initial_split$Fold1]
  pseudo_ind <- w_train[initial_split$Fold1]

  # trunc_level <- .02
  reg_p_score <- predict(propensity_reg, newdata = pseudo_reg_data)

  # Make sure we truncate the estimated propensity score for stability
  data.frame(X1 = reg_p_score) %>% dplyr::mutate(X1 = case_when(X1 < trunc_level ~ trunc_level,
                                                                X1 > 1-trunc_level ~ 1-trunc_level,
                                                                TRUE ~ X1)) %>%
    dplyr::select(X1) -> truncated_reg_pscore

  trunc_p <- truncated_reg_pscore[,1]
  c_pred <- predict(contr_reg, newdata = pseudo_reg_data)
  t_pred <- predict(treat_reg, newdata = pseudo_reg_data)
  tailored_pred <- ifelse(pseudo_ind,
                          t_pred,
                          c_pred)

  # Now create the pseudo outcome
  print("Fitting Pseudo Outcome Regression")
  pseudo_outcome <- ((pseudo_ind - trunc_p) / (trunc_p*(1-trunc_p)))*(pseudo_out - tailored_pred) + t_pred - c_pred
  pseudo_outcome_reg_3 <- forestry(x = pseudo_reg_data,
                                   y = pseudo_outcome,
                                   nthread = 0,
                                   saveable = TRUE,
                                   mtry = max(floor(ncol(pseudo_reg_data) / 3), 2),
                                   nodesizeStrictSpl = 20)

  return(list(pseudo_outcome_reg_1,pseudo_outcome_reg_2,pseudo_outcome_reg_3))
}



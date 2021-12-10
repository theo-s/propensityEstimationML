library(dbarts)
library(Rforestry)
library(ggplot2)
library(dplyr)
library(reshape)

source("~/Desktop/p_est_project/code/helpers.R")

# Plot for some different P score functions ------------------------------------
p <- plot_preds(ps_func = smooth_ps,
                title = "Smooth Propensity Score")
p
ggsave(filename = "~/Desktop/p_est_project/figures/smooth.pdf", height = 4, width = 7)

p <- plot_preds(ps_func = linear_ps,
                title = "Linear Propensity Score")
p
ggsave(filename = "~/Desktop/p_est_project/figures/linear.pdf", height = 4, width = 7)

p <- plot_preds(ps_func = constant_ps,
                title = "Constant Propensity Score")
p
ggsave(filename = "~/Desktop/p_est_project/figures/constant.pdf", height = 4, width = 7)

p <- plot_preds(ps_func = nonlinear_ps,
                title = "Binned Propensity Score")
p
ggsave(filename = "~/Desktop/p_est_project/figures/binned.pdf", height = 4, width = 7)



# treatment effect simulations -------------------------------------------------
# set.seed(234141)
#
# # Run for range of estimators and dgp's
# models <- list("linear" = linear_ps,
#                "constant" = constant_ps,
#                "nonlinear" = nonlinear_ps,
#                "smooth" = smooth_ps)
#
# estimators <- list("true" = lr,
#                    "logistic" = lr,
#                    "randomforest" = rf,
#                    "bart" = bt,
#                    "corrected" = stacked_rf)
#
# results <- expand.grid(names(estimators),names(models))
# results$Est <- NA
# results$True <- NA
#
# colnames(results) <- c("Estimator","DataSet","Est","True")
# results$True <- ifelse(results$DataSet == "linear", .5861,
#                        ifelse(results$DataSet == "smooth", .32346,
#                               ifelse(results$DataSet == "nonlinear",.5,
#                                      .3)))
#
# for (estimator in names(estimators)) {
#   for (model in names(models)) {
#       print(paste0(model,"  ",estimator))
#       te <- est_te(p_score = models[[model]],
#                    reg_func = estimators[[estimator]],
#                    n=10000,
#                    true_p = ifelse(estimator == "true", TRUE,FALSE))
#       print(te)
#       results$Est[which(results$Estimator == estimator && results$DataSet == model)] <- te
#   }
# }
# print(results)




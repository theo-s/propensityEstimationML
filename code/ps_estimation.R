library(dbarts)
library(Rforestry)
library(ggplot2)
library(dplyr)
library(reshape)

source("~/Desktop/propensityEstimationML/code/helpers.R")

save = FALSE
# Plot for some different P score functions ------------------------------------
p <- plot_preds(ps_func = smooth_ps,
                title = "Smooth Propensity Score")
p
if (save) {
  ggsave(filename = "~/Desktop/propensityEstimationML/figures/smooth.pdf", height = 4, width = 7)
}


p <- plot_preds(ps_func = linear_ps,
                title = "Linear Propensity Score")
p
if (save) {
  ggsave(filename = "~/Desktop/propensityEstimationML/figures/linear.pdf", height = 4, width = 7)
}


p <- plot_preds(ps_func = constant_ps,
                title = "Constant Propensity Score")
p
if (save) {
  ggsave(filename = "~/Desktop/propensityEstimationML/figures/constant.pdf", height = 4, width = 7)
}


p <- plot_preds(ps_func = nonlinear_ps,
                title = "Binned Propensity Score")
p
if (save) {
  ggsave(filename = "~/Desktop/propensityEstimationML/figures/binned.pdf", height = 4, width = 7)
}





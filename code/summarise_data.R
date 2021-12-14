library(xgboost)
library(Rforestry)
library(xtable)
library(dplyr)
library(cobalt)
library(causalToolbox)

source("~/Desktop/propensityEstimationML/code/helpers_analysis.R")
source("~/Desktop/propensityEstimationML/code/helpers_dr.R")

data <- read.csv(file = "~/Desktop/propensityEstimationML/data/ca2018.csv")

# The census data is from California in 2018 and contains the following variables:
# ==============================================================================
# 'AGEP'  -----  Age
# 'COW'   -----  Class of worker
# 'SCHL'  -----  Level of schooling
# 'MAR'   -----  Marital status
# 'OCCP'  -----  Occupation
# 'WKHP'  -----  Work hours per week
# 'POBP'  -----  Place of birth
# 'CIT'   -----  Citizenship status
# 'DIS'   -----  Disability code
# 'SEX'   -----  Gender code
# 'RAC1P' -----  Recorded race code

# Make class of worker, occupation, place of birth, race categorical variables
data %>%
  mutate(COW = as.factor(COW),
         OCCP = as.factor(OCCP),
         POBP = as.factor(POBP),
         RAC1P = as.factor(RAC1P)) -> data_filtered

# Get the difference in number of observations
complete <- data_filtered[complete.cases(data_filtered),]
nrow(data_filtered)
nrow(complete)

# Plot the covariate distributions for each feature ----------------------------
set.seed(2332)
sample_idx <- sample(1:nrow(complete),size=1000,replace = FALSE)

for (var_idx in 1:(ncol(complete)-1)) {
  # Make plot for var_i on downsampled data


  p1 <- bal.plot(x = complete[sample_idx,],
                 var.name = names(complete)[var_idx],
                 treat = complete$HED[sample_idx])
  plot(p1)
  ggsave(filename = paste0("~/Desktop/propensityEstimationML/figures/",
                           names(complete)[var_idx],
                           "balance.pdf"),
         height = 5, width = 6)
}

# Make a table with the distribution summaries ---------------------------------
p <- data.frame(matrix(nrow=0,ncol=7))
p <- data.frame(p)
colnames(p) <- c("Min","1st Qu.","Median","Mean","3rd Qu.","Max","NA's")

for (var_idx in 1:(ncol(complete)-1)) {
  # Make plot for var_i on downsampled data
  print(names(data_filtered)[var_idx])
  if (!is.factor(data_filtered[,var_idx])) {

    s <- summary(data_filtered[,var_idx])
    if (length(unname(s)) == 6) {
      s <- c(s,0)
    }
    p[var_idx,] <- unname(s)

  }
}

xtable(p, caption = c("Summary of the distributions of the continuous covariates"))



# Save summary of covariates ---------------------------------------------------
data_summary <- data.frame(Variable = c('AGEP','COW','SCHL','MAR','OCCP','WKHP',
                                        'POBP','CIT','DIS','SEX','RAC1P','PINCP','HED'),
                           Description = c("Age", "Class of worker","Level of schooling",
                                           "Marital status", "Occupation", "Work hours per week",
                                           "Place of birth", "Citizenship status",
                                           "Disability code", "Gender code",
                                           "Recorded race code",
                                           "Reported income (USD)",
                                           "Higher Education Indicator \n (>= 21 Years of education)"))

xtable(data_summary)


# Make love plot
p1 <- love.plot(x = complete %>% dplyr::select(-OCCP,-COW,-POBP,-RAC1P,-SCHL),
               treat = complete$HED)
plot(p1)
ggsave(filename = "~/Desktop/propensityEstimationML/figures/love_plot.pdf",
       width = 7, height=5)







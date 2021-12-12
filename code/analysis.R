library(xgboost)
library(Rforestry)
library(dplyr)

source("code/helpers_analysis.R")
source("code/helpers_dr.R")

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

# Processing Step ==============================================================

# Make class of worker, occupation, place of birth, race categorical variables
data %>%
  mutate(COW = as.factor(COW),
         OCCP = as.factor(OCCP),
         POBP = as.factor(POBP),
         RAC1P = as.factor(RAC1P)) -> data_filtered



x <- iris[,-1]
y <- iris[,1]
Tr <- sapply(c(1:150), function(x){return(rbinom(1,1,.5))})



dr <- DR_Learner_RF(feature_train = x,
                    w_train = Tr,
                    yobs_train = y)


pred_cate <- rf_DR_pred(dr,
                        newdata = x)


drx <- DR_Learner_XGB(feature_train = x,
                      w_train = Tr,
                      yobs_train = y)



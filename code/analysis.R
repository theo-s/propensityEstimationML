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

# Processing Step ==============================================================

# Make class of worker, occupation, place of birth, race categorical variables
data %>%
  mutate(COW = as.factor(COW),
         OCCP = as.factor(OCCP),
         POBP = as.factor(POBP),
         RAC1P = as.factor(RAC1P)) -> data_filtered

# Now train DR Learners with XGboost and random forest + T Learner -------------
x <- data_filtered %>%
  dplyr::select(-HED,-PINCP)
y <- data_filtered$PINCP
Tr <- data_filtered$HED

ate_naive <- mean(y[Tr==1]) - mean(y[Tr==0])
print(ate_naive)

# Train DR random forest
dr <- DR_Learner_RF(feature_train = x,
                    w_train = Tr,
                    corrected = TRUE,
                    yobs_train = y)


pred_dr <- rf_DR_pred(dr,
                      newdata = x)
print(paste0("ATE (DR Learner RF): ",mean(pred_dr)))


# Remove all observations with missing categorical variables
data_complete <- data_filtered %>%
  dplyr::select(-OCCP,-COW,-POBP,-RAC1P)
data_complete <- data_complete[complete.cases(data_complete),]

x_complete <- data_complete %>%
  dplyr::select(-HED,-PINCP)
y_complete <- data_complete$PINCP
Tr_complete <- data_complete$HED

drx <- DR_Learner_XGB(feature_train = x_complete,
                      w_train = Tr_complete,
                      yobs_train = y_complete)

pred_xgb <- xgb_DR_pred(drx,
                        newdata = x_complete)
print(paste0("ATE (DR Learner XGboost): ",mean(pred_xgb)))

# Train T-Learner
tl <- T_RF(feat = x_complete,
           tr = Tr_complete,
           yobs = y_complete,
           mu0.forestry = list(relevant.Variable = 1:ncol(x_complete), ntree = 1000, replace = TRUE,
                               sample.fraction = 0.9, mtry = ncol(x_complete), nodesizeSpl = 1, nodesizeAvg = 3,
                               nodesizeStrictSpl = 20, nodesizeStrictAvg = 1, splitratio = 1, middleSplit = FALSE,
                               OOBhonest = TRUE),
           mu1.forestry = list(relevant.Variable = 1:ncol(x_complete), ntree = 1000, replace = TRUE,
                               sample.fraction = 0.9, mtry = ncol(x_complete), nodesizeSpl = 1, nodesizeAvg = 3,
                               nodesizeStrictSpl = 20, nodesizeStrictAvg = 1, splitratio = 1, middleSplit = FALSE,
                               OOBhonest = TRUE)
           )

pred_tl <- EstimateCate(tl, feature_new = x_complete)
print(paste0("ATE (T Learner): ",mean(pred_tl)))

results <- data.frame(Estimator = c("DR Learner (RF)","DR Learner (XGboost)","T Learner (RF)","Difference In Means"),
                      ATE = c(mean(pred_dr),mean(pred_xgb), mean(pred_tl),ate_naive))

write.csv(x = results, "~/Desktop/propensityEstimationML/data/results.csv")
xtable(results, caption = "Average treatment effect of education on earnings based on different estimators.")


# Now explore some heterogeneity in the treatment effects ----------------------
quantile(pred_dr, probs= c(.9,.99,.999))

data.frame(CATE = quantile(pred_dr, probs = seq(.01,.99,by=.01)),
           Quantile = 1:99) %>%
  ggplot(aes(x=Quantile,y = CATE))+
  geom_line()+
  theme_bw()+
  labs(x= "Quantile",y = "CATE Estimate")

ggsave(filename = "~/Desktop/propensityEstimationML/figures/cate_quantiles.pdf",
       width = 7, height = 5)


# Get the uplift by occupation -------------------------------------------------
data_cate <- data.frame(data_filtered,CATE = pred_dr)
data_cate %>%
  group_by(OCCP) %>%
  summarise(GroupTE = mean(CATE)) %>%
  dplyr::select(GroupTE) %>%
  summary() -> occupation_te


xtable(occupation_te, caption = "Summary of average treatment effect estimates when grouped by occupation")

data_cate %>%
  group_by(POBP) %>%
  summarise(GroupTE = mean(CATE)) %>%
  dplyr::select(GroupTE) %>%
  summary() -> pob_te


xtable(pob_te, caption = "Summary of average treatment effect estimates when grouped by place of birth")

data_cate %>%
  group_by(SEX) %>%
  summarise(GroupTE = mean(CATE)) %>%
  dplyr::select(GroupTE)  -> gender_te


xtable(gender_te, caption = "Group average treatment effect estimates when grouped by gender")


data_cate %>%
  group_by(RAC1P) %>%
  summarise(GroupTE = mean(CATE)) %>%
  dplyr::select(GroupTE)  -> race_te


xtable(race_te, caption = "Group average treatment effect estimates when grouped by race")



# Plot some continuous variables -----------------------------------------------
data_cate %>%
  dplyr::select(AGEP,CATE) %>%
  group_by(AGEP) %>%
  summarise(CATE = mean(CATE)) %>%
  ggplot(aes(x = AGEP, y = CATE))+
  geom_line()+
  theme_classic()+
  labs(x = "AGE (years)")
ggsave(filename = "~/Desktop/propensityEstimationML/figures/age_cate.pdf",
       width = 7, height = 5)

data_cate %>%
  dplyr::filter(SCHL > 20) %>%
  dplyr::select(SCHL,CATE) %>%
  group_by(SCHL) %>%
  summarise(CATE = mean(CATE)) %>%
  ggplot(aes(x = SCHL, y = CATE))+
  geom_line()+
  theme_classic()+
  labs(x = "Schooling (years)")
ggsave(filename = "~/Desktop/propensityEstimationML/figures/schl_cate.pdf",
       width = 7, height = 5)

data_cate %>%
  dplyr::select(WKHP,CATE) %>%
  group_by(WKHP) %>%
  summarise(CATE = mean(CATE)) %>%
  ggplot(aes(x = WKHP, y = CATE))+
  geom_line()+
  theme_classic()+
  labs(x = "Work Hours Per Week (hours)")
ggsave(filename = "~/Desktop/propensityEstimationML/figures/wkhp_cate.pdf",
       width = 7, height = 5)




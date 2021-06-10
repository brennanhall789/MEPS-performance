library(boot)
library(caret)
library(glmnet)
library(car)
library(ModelMetrics)

source('data_prep.R')

detach(projectData)
detach(testData)
detach(logprojectData)
attach(projectData)

##### Model Selection Process

##### model with all terms #####
model1 <- glm(FY19_Percent ~ ., data = projectData)  ## switch to glm() to utilize cv.glmnet() below
(s1 <- summary(model1))
# Diagnostics
par(mfrow=c(2,2))
for(i in 1:4){
  plot(model1, which=i+1)
}
par(mfrow=c(1,1))
model1.res <- residuals(model1)
shapiro.test(model1.res)  # normality test
lmtest::bptest(model1)    # constant variance test
car::vif(model1)          # multicollinearity test
car::durbinWatsonTest(model1) # durbin watson test for autocorrelation
## Model's in-sample predictions
rmse(projectData$FY19_Percent, model1$fitted.values)
s1$adj.r.squared

# Cross-Validation prediction error
cv1 <- cv.glm(projectData, model1)
cv1$delta[1]

#####

##### model without cba_q3 #####
# CBA_Q3 is chosen because of high VIF and it has only 1 instance of 
# factor level 0 which is a problem for cross-validation
model2 <- lm(FY19_Percent ~ FY18_Percent + 
                CBA_Q1 + CICO_Q1 + HIV_Q1 + FBP_Q1 + TNG_Q1 + DSP_Q1 +
                CBA_Q2 + CICO_Q2 + HIV_Q2 + FBP_Q2 + TNG_Q2 + DSP_Q2 +
                CICO_Q3 + HIV_Q3 + FBP_Q3 + TNG_Q3 + DSP_Q3 + CLIP_Q3 +
                CICO_Q4 + HIV_Q4 + FBP_Q4 + TNG_Q4, data = projectData)
(s2 <- summary(model2))
# Diagnostics
par(mfrow=c(2,2))
for(i in 1:4){
  plot(model2, which=i+1)
}
par(mfrow=c(1,1))
model2.res <- residuals(model2)
shapiro.test(model2.res)  # normality test
lmtest::bptest(model2)    # constant variance test
car::vif(model2)          # multicollinearity test
car::durbinWatsonTest(model2) # durbin watson test for autocorrelation
## Model in-sample predictions
rmse(projectData$FY19_Percent, model2$fitted.values)
s2$adj.r.squared

# Cross-Validation prediction error
cv2 <- cv.glm(projectData, model2)
cv2$delta[1]
#####

##### model with interactions #####
# include interactions with checkin/checkout (time of completion interacting with performance?)
# also remove IBA because FY19 columns have additional levels that we cannot model with FY18 data
model3 <- glm(FY19_Percent ~ FY18_Percent + 
                CBA_Q1 + log(CICO_Q1)*(HIV_Q1 + DSP_Q1) + FBP_Q1 + TNG_Q1 +
                CBA_Q2 + log(CICO_Q2)*(HIV_Q2 + DSP_Q2) + FBP_Q2 + TNG_Q2 + 
                CLIP_Q3*HIV_Q3 + log(CICO_Q3)*(HIV_Q3 + DSP_Q3) + FBP_Q3 + TNG_Q3 + CLIP_Q3*(DSP_Q3) +
                log(CICO_Q4)*HIV_Q4 + FBP_Q4 + TNG_Q4,
              data = projectData)
# model3 <- glm(FY19_Percent ~ FY18_Percent + 
#                 CBA_Q1 + log(CICO_Q1)*HIV_Q1 + FBP_Q1 + TNG_Q1 + log(CICO_Q1)*sqrt(DSP_Q1) +
#                 CBA_Q2 + log(CICO_Q2)*HIV_Q2 + FBP_Q2 + TNG_Q2 + log(CICO_Q2)*sqrt(DSP_Q2) +
#                 CLIP_Q3*HIV_Q3 + log(CICO_Q3)*HIV_Q3 + FBP_Q3 + TNG_Q3 + CLIP_Q3*sqrt(DSP_Q3) + log(CICO_Q3)*sqrt(DSP_Q3) +
#                 log(CICO_Q4)*HIV_Q4 + FBP_Q4 + TNG_Q4,
#                 data = projectData)
(s3 <- summary(model3))
# Diagnostics
par(mfrow=c(2,2))
for(i in c(1,2,3,5)){
  plot(model3, which=i)
}
par(mfrow=c(1,1))
model3.res <- residuals(model3)
shapiro.test(model3.res)  # normality test
lmtest::bptest(model3)    # constant variance test
car::vif(model3)          # multicollinearity test
car::durbinWatsonTest(model3) # durbin watson test for residual autocorrelation
# Performance of in-sample predictions
rmse(projectData$FY19_Percent, model3$fitted.values)
s3$adj.r.squared

# Cross-Validation prediction error
qpcR::PRESS(model3)
cv3 <- cv.glm(projectData, model3)
cv3$delta[1]   # cv estimate of prediction error
#####

# AIC variable reduction #
# This model is referred to as Model 5 in the report
step3 <- step(model3, trace = 0, direction='backward')
summary(step3)
par(mfrow=c(2,2))
for(i in 1:4){
  plot(step3, which=i+1)
}
par(mfrow=c(1,1))
step3.res <- residuals(step3)
shapiro.test(step3.res)  # normality test
lmtest::bptest(step3)    # constant variance test
car::vif(step3)          # multicollinearity test
car::durbinWatsonTest(step3) # durbin watson test for residual autocorrelation
rmse(projectData$FY19_Percent, step3$fitted.values)
summary(step3)$adj.r.squared
cvstep3 <- cv.glm(projectData, step3)
cvstep3$delta[1]   # cv estimate of prediction error

#####

##### 2nd model with interactions #####
# This model is not discussed in the report
# log transform of DSP chosen after looking at partial residual vs covariate plots in
# transformation_and_outliers.R file
# 
model4 <- lm(FY19_Percent ~ FY18_Percent + 
               CBA_Q1 + log(CICO_Q1) + HIV_Q1 + FBP_Q1 + TNG_Q1 + (DSP_Q1) +
               CBA_Q2 + log(CICO_Q2) + HIV_Q2 + FBP_Q2 + TNG_Q2 + (DSP_Q2) +
               log(CICO_Q3) + CLIP_Q3:HIV_Q3 + HIV_Q3 + FBP_Q3 + TNG_Q3 + CLIP_Q3:(DSP_Q3) + (DSP_Q3) +
               log(CICO_Q4) + HIV_Q4 + FBP_Q4 + TNG_Q4,
             data = projectData)
(s4 <- summary(model4))
# Diagnostics
par(mfrow=c(2,2))
for(i in 1:4){
  plot(model4, which=i+1)
}
par(mfrow=c(1,1))
model4.res <- residuals(model4)
shapiro.test(model4.res)  # normality test
lmtest::bptest(model4)    # constant variance test
car::vif(model4)          # multicollinearity test
car::durbinWatsonTest(model4) # durbin watson test for residual autocorrelation
# Performance of in-sample predictions
rmse(projectData$FY19_Percent, model4$fitted.values)
s4$adj.r.squared

# Cross-Validation prediction error
cv4 <- cv.glm(projectData, model4)
cv4$delta[1]
#####

# AIC variable reduction #
step4 <- step(model4, trace = 0)
rmse(projectData$FY19_Percent, step4$fitted.values)
summary(step4)$adj.r.squared
cvstep4 <- cv.glm(projectData, step4)
cvstep4$delta[1]   # cv estimate of prediction error


##### Model log-odds #####
odds.model <- update(model3, log(FY19_Percent/(1-FY19_Percent)) ~ .,
             data = projectData)
(sodd <- summary(odds.model))
# Diagnostics
par(mfrow=c(2,2))
for(i in 1:4){
  plot(odds.model, which=i+1)
}
par(mfrow=c(1,1))
odds.res <- residuals(odds.model)
shapiro.test(model4.res)  # normality test
lmtest::bptest(odds.model)    # constant variance test
car::vif(odds.model)          # multicollinearity test
car::durbinWatsonTest(odds.model) # durbin watson test for autocorrelation
# Model predictions
rmse(projectData$FY19_Percent, odds.model$fitted.values)
sodd$adj.r.squared

# Cross-Validation prediction error
cvodd <- cv.glm(projectData, odds.model)
cvodd$delta[1]

#####


##### Lasso regression #####
## CV to solve for lambda hyperparameter
## set up predictor matrix and response vector for Lasso regression
# x_vars <- model.matrix(formula(step3), data = projectData)[,-1]
# y_var <- projectData$FY19_Percent
# 
# test_x_vars <- model.matrix(FY19_Percent ~ FY18_Percent + 
#                          CBA_Q1 + log(CICO_Q1) + HIV_Q1 + FBP_Q1 + TNG_Q1 + sqrt(DSP_Q1) +
#                          CBA_Q2 + log(CICO_Q2) + HIV_Q2 + FBP_Q2 + TNG_Q2 + sqrt(DSP_Q2) +
#                          log(CICO_Q3) + CLIP_Q3:HIV_Q3 + HIV_Q3 + FBP_Q3 + TNG_Q3 + CLIP_Q3:sqrt(DSP_Q3) + sqrt(DSP_Q3) +
#                          log(CICO_Q4) + HIV_Q4 + FBP_Q4 + TNG_Q4, data = testData)[,-1]
# test_y_var <- testData$FY19_Percent
# 
# lambdas <- 10^seq(2, -3, by = -.1)
# lasso_reg  = cv.glmnet(x_vars, y_var, alpha = 1, family = 'gaussian', lambda = lambdas)
# lambda_best <- lasso_reg$lambda.min 
# lambda_best
# 
# lasso_model <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda_best, standardize = TRUE)
# 
# # in-sample prediction error
# predictions_lasso <- predict(lasso_model, s = lambda_best, newx = x_vars)
# rmse(y_var, predictions_lasso)
# lasso_model$dev.ratio
# 
# tLL <- lasso_model$nulldev - deviance(lasso_model)
# k <- lasso_model$df
# n <- lasso_model$nobs
# AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
# AICc
# 
# #####

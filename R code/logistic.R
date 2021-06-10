### Logistic regression to predict if MEPS will win Unit Pennant
library(pscl)
library(ROCR)

source('data_prep.R')
source('model_testing.R')

detach(projectData)
detach(testData)
detach(logprojectData)
attach(logprojectData)

##### Logistic model formulation #####
logmodel1 <- glm(cbind(FY19UNIT,4-FY19UNIT) ~ 
                  CBA_Q1 + log(CICO_Q1) + HIV_Q1 + FBP_Q1 + TNG_Q1 + DSP_Q1 + Per_Q1 +
                  CBA_Q2 + log(CICO_Q2) + HIV_Q2 + FBP_Q2 + TNG_Q2 + DSP_Q2 + Per_Q2 +
                   log(CICO_Q3) + HIV_Q3 + FBP_Q3 + TNG_Q3 + DSP_Q3 + CLIP_Q3 + Per_Q3 +
                   log(CICO_Q4) + HIV_Q4 + FBP_Q4 + TNG_Q4 + Per_Q4, 
                data = logprojectData, family = binomial(link = 'logit'))
summary(logmodel1)

logmodel2 <- glm(cbind(FY19UNIT,4-FY19UNIT) ~ 
                  CBA_Q1 + log(CICO_Q1) + HIV_Q1 + FBP_Q1 + TNG_Q1 + DSP_Q1 + Per_Q1 +
                  CBA_Q2 + log(CICO_Q2) + HIV_Q2 + FBP_Q2 + TNG_Q2 + DSP_Q2 + Per_Q2 +
                  log(CICO_Q3) + CLIP_Q3*HIV_Q3 + FBP_Q3 + TNG_Q3 + CLIP_Q3*DSP_Q3 + Per_Q3 +
                  log(CICO_Q4) + HIV_Q4 + FBP_Q4 + TNG_Q4 + Per_Q4, 
                data = logprojectData, family = binomial(link = 'logit'))
summary(logmodel2)

logstep2 <- step(logmodel2, direction = 'backward', trace = 0, scope = list(lower= ~ Per_Q1+Per_Q2+Per_Q3+Per_Q4))
summary(logstep2)


logmodel3 <- glm(cbind(FY19UNIT,4-FY19UNIT) ~ 
                 CBA_Q1 + log(CICO_Q1)*(HIV_Q1 + DSP_Q1) + FBP_Q1 + TNG_Q1 + Per_Q1 +
                 CBA_Q2 + log(CICO_Q2)*(HIV_Q2 + DSP_Q2) + FBP_Q2 + TNG_Q2 + Per_Q2 +
                 CLIP_Q3*(HIV_Q3 + DSP_Q3) + log(CICO_Q3)*(HIV_Q3 + DSP_Q3) + FBP_Q3 + TNG_Q3 + Per_Q3 +
                 log(CICO_Q4)*HIV_Q4 + FBP_Q4 + TNG_Q4 + Per_Q4,
                 data = logprojectData, family = binomial(link = 'logit'))
summary(logmodel3)

logstep3 <- step(logmodel3, direction = 'backward', trace = 0,
                 scope = list(lower= ~ Per_Q1+Per_Q2+Per_Q3+Per_Q4))
summary(logstep3)


logmodel4 <- glm(cbind(FY19UNIT,4-FY19UNIT) ~ 
                   CBA_Q1 + log(CICO_Q1)*(HIV_Q1 + DSP_Q1) + FBP_Q1 + TNG_Q1 + Per_Q1 +
                   CBA_Q2 + log(CICO_Q2)*(HIV_Q2 + DSP_Q2) + FBP_Q2 + TNG_Q2 + Per_Q2 +
                   CLIP_Q3*(HIV_Q3 + DSP_Q3) + log(CICO_Q3)*(HIV_Q3 + DSP_Q3) + FBP_Q3 + TNG_Q3 + Per_Q3 +
                   log(CICO_Q4)*HIV_Q4 + FBP_Q4 + TNG_Q4 + Per_Q4,
                 data = logprojectData, family = binomial(link = 'logit'))
summary(logmodel4)

logstep4 <- step(logmodel4, direction = 'backward', trace = 0, k=log(nrow(logprojectData)),
                 scope = list(lower= ~ Per_Q1+Per_Q2+Per_Q3+Per_Q4))
summary(logstep4)


anova(logstep3,logmodel1, test = 'Chisq')

logmodel <- logstep3
#####

##### Diagnostics for chosen logistic model #####
# Check phi parameter ~= 1
df <- summary(logmodel)$df.residual
mu.hat <- predict(logmodel, type = "response")
eta.hat <- predict(logmodel, type = "link")
V.fun <- function(p) {p*(1-p)}
V <- V.fun(mu.hat)
r <- (logprojectData$FY19UNIT - mu.hat)/sqrt(V)
X <- sum(r^2)
(phi <- X/(df))   # perhaps a beta-binomial model may work better to model overdispersion

res.dev <- residuals(logmodel, type = "deviance")
plot(res.dev ~ eta.hat, main="Residuals vs Fitted values",
     xlab="Fitted values", ylab="Deviance Residuals") # expected to be bad 


##### res vs covariates plots #####
# residuals against the covariates #
# test for a possible wrong choice of link function and/or 
# missing covariates.
matrix <- model.matrix(logmodel)[,-1]
xterm = colnames(matrix)

par(mfrow=c(3,3))
for(i in 1:length(xterm)){
  plot(res.dev ~ matrix[,i], xlab=xterm[i], main = paste("Residuals vs", xterm[i]), ylab="Residuals")
  lines(lowess(res.dev ~ matrix[,i]), col="red", lty=2)
}
par(mfrow=c(1,1))
####

# partial residuals vs covariates plots #
# tests for higher order terms
par.res <- residuals(logmodel, type = "partial")

par(mfrow=c(3,3))
for(i in 1:length(xterm)){
  plot(par.res[,i] ~ matrix[,i], xlab=xterm[i], main = paste("Partial Residuals vs", xterm[i]), ylab="Partial Residuals")
  lines(lowess(par.res[,i] ~ matrix[,i]), col="red", lty=2)
}
par(mfrow=c(1,1))

#####

##### outlier analysis #####
# The plot of the Cook's Distance indicates there are no observations with large 
# Cook's distance (>0.5).
# The Residuals vs. Leverage plot shows that there could be a few observations 
# with high leverage but none with both high leverage and high Cook's distance.
plot(logmodel,which=4)
plot(logmodel,which=5)


# Performance of in-sample predictions
rmse(logprojectData$FY19UNIT, 4*logmodel$fitted.values)
# McFadden's pseudo R-squared
(prsquared = pscl::pR2(logmodel)["McFadden"])    # pR^2 > .4 is good

# Cross-Validation prediction error
cv3 <- cv.glm(logprojectData, logmodel)
cv3$delta[1] 

#####


##### Prediction of Pennant awards #####
## predictions for proportion of # of pennant awards in 2020 per MEPS based on 2019
(log_unit_pred <- predict(logmodel, newdata=log_testData, type = 'response'))
select(dataFY19_Overall, MEPS) %>% mutate(Pennant = log_unit_pred*4) -> log_preds

#####


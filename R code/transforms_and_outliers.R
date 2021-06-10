### Test boxcox transform and transform of predictors
source('data_prep.R')
source('model_formulation.R')

detach(projectData)
detach(testData)
detach(logprojectData)
attach(projectData)

# model of consideration
fit = step3
fsumm <- summary(fit)

## Test for necessary predictor transformations with component residual plots
crPlots(fit)

##### res vs covariates plots #####
# residuals against the covariates #
# test for a possible wrong choice of link function and/or 
# missing covariates.
matrix <- model.matrix(fit)[,-1]
xterm = colnames(matrix)

res.dev <- residuals(fit, type = 'deviance')

par(mfrow=c(3,3))
for(i in 1:length(xterm)){
  plot(res.dev ~ matrix[,i], xlab=xterm[i], ylab="Residual Deviance", main = paste("Residuals vs", xterm[i]))
  lines(lowess(res.dev ~ matrix[,i]), col="red", lty=2)
}
par(mfrow=c(1,1))
####

# partial residuals vs covariates plots #
# tests for higher order terms
par.res <- residuals(fit, type = "partial")

par(mfrow=c(3,3))
for(i in 1:length(xterm)){
  plot(par.res[,i] ~ matrix[,i], xlab=xterm[i],ylab="Partial Residuals",
       main = paste("Partial Residuals vs", xterm[i]))
  lines(lowess(par.res[,i] ~ matrix[,i]), col="red", lty=2)
}
par(mfrow=c(1,1))

## Test for response transformations with boxcox
hist(fit$residuals)
box1 <- boxCox(fit)
best.lambda<- box1$x[which.max(box1$y)]
best.lambda
model_box <- update(fit, FY19_Percent^best.lambda~.)
boxsumm <- summary(model_box)

# Check boxcox model Diagnostics and evaluate
par(mfrow=c(2,2))
for(i in 1:4){
  plot(model_box, which=i+1)
}
par(mfrow=c(1,1))
model_box.res <- residuals(model_box)
shapiro.test(model_box.res)  # normality test
lmtest::bptest(model_box)    # constant variance test
car::vif(model_box)          # multicollinearity test
car::durbinWatsonTest(model_box) # durbin watson test for autocorrelation
# Model's in-sample predictions
rmse(projectData$FY19_Percent, model_box$fitted.values)
boxsumm$adj.r.squared

# Cross-Validation prediction error
cvbox <- cv.glm(projectData, model_box)
cvbox$delta[1]

## after testing all non transformed models, any boxcox model performs worse in all
## performance measures (R^2, PRESS, RMSE)


##### Analyze influential observations #####
# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs


# Influential Observations
# added variable plots to look  for outliers or influential points
avPlots(fit)
plotdb <- dfbetaPlots(fit,id.method = 'x')
plot(fit,which=5)
# Cook's Distance (CD) plot
cooksd <- cooks.distance(fit)
# identify CD values > 4/(n-p-2) 
cutoff <- 4/((nrow(projectData)-length(fit$coefficients)-2)) 
influential <- which(cooksd > cutoff)
projectData[influential,]
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


#####
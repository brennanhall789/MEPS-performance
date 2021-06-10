## Model Prediction on 2019 data to generate 2020 output##
source('model_formulation.R')

detach(projectData)
detach(testData)
detach(logprojectData)
attach(testData)

# best model
model = step3
####

# need to alter 19 data loading to model each quarter individually to predict
# results of each quarter for MOQ award and Unit Pennant award
# then use agg results to predict MOY award


##### Predictions #####

## test 2020 predictions
# Prediction on year (agg) data
model %>% predict(newdata = testData) -> model_pred
#model %>% predict(newx = test_x_vars) -> model_pred
# Prediction on each quarter
model %>% predict(newdata = FY19Q1) -> model_predq1
model %>% predict(newdata = FY19Q2) -> model_predq2
model %>% predict(newdata = FY19Q3) -> model_predq3
model %>% predict(newdata = FY19Q4) -> model_predq4

## 
meps_pred <- bind_cols(MEPS=dataFY19$MEPS, PRED=model_pred, 
                       PRED_Q1=model_predq1,
                       PRED_Q2=model_predq2,
                       PRED_Q3=model_predq3,
                       PRED_Q4=model_predq4)
summary(meps_pred)
## rank order by category large/med/small
# rank by color code in spreadsheet
meps_pred %>% mutate(CAT = 'large') -> meps_pred
for (i in 22:65){
  if(22 <= i & i <=43){
    meps_pred[i,'CAT'] <- 'med'
  }
  if(44 <= i & i <=65){
    meps_pred[i,'CAT'] <- 'small'
  }
}


## determine winners of awards
# MOY award goes to top 1 of each category for year (agg score)
meps_pred %>% group_by(CAT) %>% top_n(1, PRED) -> MOY

# MOQ award goes to top 3 of each category each quarter
meps_pred %>% group_by(CAT) %>% top_n(3, PRED_Q1) %>% select(MEPS,PRED_Q1,CAT) -> MOQ1
meps_pred %>% group_by(CAT) %>% top_n(3, PRED_Q2) %>% select(MEPS,PRED_Q2,CAT) -> MOQ2
meps_pred %>% group_by(CAT) %>% top_n(3, PRED_Q3) %>% select(MEPS,PRED_Q3,CAT) -> MOQ3
meps_pred %>% group_by(CAT) %>% top_n(3, PRED_Q4) %>% select(MEPS,PRED_Q4,CAT) -> MOQ4

# UNIT award goes to MEPS with percentage >= .9 each quarter
meps_pred %>% mutate(UNIT=ifelse(PRED_Q1 >= 0.9,"Yes","No")) %>% select(MEPS,PRED_Q1,UNIT) -> UNIT1
meps_pred %>% mutate(UNIT=ifelse(PRED_Q2 >= 0.9,"Yes","No")) %>% select(MEPS,PRED_Q2,UNIT) -> UNIT2
meps_pred %>% mutate(UNIT=ifelse(PRED_Q3 >= 0.9,"Yes","No")) %>% select(MEPS,PRED_Q3,UNIT) -> UNIT3
meps_pred %>% mutate(UNIT=ifelse(PRED_Q4 >= 0.9,"Yes","No")) %>% select(MEPS,PRED_Q4,UNIT) -> UNIT4

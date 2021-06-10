library(openxlsx)
library(tidyverse)
library(dplyr)
filename = "ProjectData+(FY21_Q3Spring)+(3).xlsx"

##### Data Prep for Model Formulation #####
dataFY19_Overall = read.xlsx(filename,
                             sheet = "FY19 Overall",
                             rows = c(2:67),
                             cols = c(1:5),
                             colNames = TRUE,
                             na.strings = 'N/A')
names(dataFY19_Overall) <- c("MEPS","FY19_Possible","FY19_Earned","FY19_Percent","FY19_Rank")

dataFY18_Overall = read.xlsx(filename,
                            sheet = "FY18 Overall",
                            rows = c(2:67),
                            cols = c(1:5),
                            colNames = TRUE,
                            na.strings = 'N/A')
names(dataFY18_Overall) <- c("MEPS","FY18_Possible","FY18_Earned","FY18_Percent","FY18_Rank")

aggregateScores <- cbind(dataFY19_Overall["MEPS"], dataFY19_Overall["FY19_Percent"], dataFY18_Overall["FY18_Percent"])


dataFY18 <- dataFY19_Overall["MEPS"]

for(i in 1:4){
  sheet <- paste0("FY18 Q", i)
  tmp <- read.xlsx(filename,
                  sheet = sheet,
                  rows = c(2:67),
                  cols = c(3:16),
                  colNames = TRUE,
                  na.strings = 'N/A')
  dataFY18 <- cbind(dataFY18, tmp)
}

names(dataFY18) <- c("MEPS","BE_Q1","CICO_Q1","TLC_Q1","TST_Q1", "DSP_Q1", "HIV_Q1", "CLIP_Q1", "FBP_Q1","IRP_Q1", 
                    "CBA_Q1", "IBA_Q1", "ToA_Q1", "ToE_Q1", "TNG_Q1","BE_Q2","CICO_Q2","TLC_Q2","TST_Q2", "DSP_Q2", 
                    "HIV_Q2", "CLIP_Q2", "FBP_Q2","IRP_Q2", "CBA_Q2", "IBA_Q2", "ToA_Q2", "ToE_Q2", "TNG_Q2","BE_Q3",
                    "CICO_Q3","TLC_Q3","TST_Q3", "DSP_Q3", "HIV_Q3", "CLIP_Q3", "FBP_Q3","IRP_Q3", "CBA_Q3", "IBA_Q3", 
                    "ToA_Q3", "ToE_Q3", "TNG_Q3","BE_Q4","CICO_Q4","TLC_Q4","TST_Q4", "DSP_Q4", "HIV_Q4", "CLIP_Q4", 
                    "FBP_Q4","IRP_Q4", "CBA_Q4", "IBA_Q4", "ToA_Q4", "ToE_Q4", "TNG_Q4")

projectData <- cbind(aggregateScores, dataFY18[2:57])


hs <- createStyle(fontName = "Times New Roman", fontSize = 12, border = "bottom", borderStyle = "thin",
                 textDecoration = "bold")
write.xlsx(projectData, "projectData.xlsx", headerStyle = hs)


# remove columns with all NAs across at least 3 quarters
na_vars <- c('BE_Q1','BE_Q2','BE_Q3','BE_Q4','TST_Q1','TST_Q2','TST_Q3','TST_Q4',
            'CLIP_Q1','CLIP_Q2','CLIP_Q4','IRP_Q1','IRP_Q2','IRP_Q3','IRP_Q4')
projectData %>% select(-all_of(na_vars)) -> projectData

## Removing uninformative variables
# 'MEPS' is an identifier and not useful for prediction
# 'timeliness of evals' only has values of 'NA' or '5' in 3 out of 4 quarters
# 'timeliness of awards' only has values of 'NA' or '5' in Q4
rm_vars <- c('MEPS','ToE_Q1','ToE_Q2','ToE_Q3','ToE_Q4','ToA_Q1','ToA_Q2','ToA_Q3','ToA_Q4')
projectData %>% select(-all_of(rm_vars)) -> projectData

# set relevant variables as factors
factor_vars = c('TLC_Q1','TLC_Q2','TLC_Q3','TLC_Q4','CBA_Q1','CBA_Q2','CBA_Q3','CBA_Q4',
                'IBA_Q1','IBA_Q2','IBA_Q3','IBA_Q4')
for (var in factor_vars){
  projectData[,var] %>% factor() -> projectData[,var]
}
# for (var in names(projectData)){
#   print(class(projectData[,var]))
# }

# center and scale numerical predictor variables to reduce collinearity
projectData %>% select(-FY19_Percent,-FY18_Percent,-contains("CICO_"),-contains("DSP_")) %>% 
  select_if(is.numeric) %>% colnames() -> num_vars
projectData[,num_vars] %>% scale() -> projectData[,num_vars]

# look at covariance matrix
projectData %>% select_if(~is.numeric(.x)) %>% cor()   
# Note all entries of DSP_Q4 equal '10', making its variance 0
# remove TLC variable for leave-one-out analysis b/c they have only one instance of value 0 and has insignificant impact to model (see via anova test)
projectData %>% select(-DSP_Q4,-contains("TLC_")) -> projectData

summary(projectData)

##### outputs projectData #####

##### Data Prep for Model Testing #####
# load FY19 agg data #
dataFY19_Overall = read.xlsx(filename,
                             sheet = "FY19 Overall",
                             rows = c(2:67),
                             cols = c(1:5),
                             colNames = TRUE,
                             na.strings = 'N/A')
names(dataFY19_Overall) <- c("MEPS","FY19_Possible","FY19_Earned","FY18_Percent","FY19_Rank")  
## change FY19_Percent name to FY18 to match the variable name in model formula
aggregateScores <- cbind(dataFY19_Overall["MEPS"], dataFY19_Overall["FY18_Percent"])

dataFY19 <- dataFY19_Overall["MEPS"]
sheet <- "FY19 Q1"
tmp <- read.xlsx(filename,
                 sheet = sheet,
                 rows = c(2:67),
                 cols = c(3:15),
                 colNames = TRUE,
                 na.strings = 'N/A')
dataFY19 <- cbind(dataFY19, tmp)
for(i in 2:4){
  sheet <- paste0("FY19 Q", i)
  tmp <- read.xlsx(filename,
                   sheet = sheet,
                   rows = c(2:67),
                   cols = c(3:14,16),
                   colNames = TRUE,
                   na.strings = 'N/A')
  dataFY19 <- cbind(dataFY19, tmp)
}

names(dataFY19) <- c("MEPS","CICO_Q1","TLC_Q1","TST_Q1", "DSP_Q1", "HIV_Q1", "CLIP_Q1", "FBP_Q1", 
                     "CBA_Q1", "IBA_Q1", "ToA_Q1", "ToE_Q1","STNG_Q1", "TNG_Q1","CICO_Q2","TLC_Q2","TST_Q2", "DSP_Q2", 
                     "HIV_Q2", "CLIP_Q2", "FBP_Q2", "CBA_Q2", "IBA_Q2", "ToA_Q2", "ToE_Q2","STNG_Q2", "TNG_Q2",
                     "CICO_Q3","TLC_Q3","TST_Q3", "DSP_Q3", "HIV_Q3", "CLIP_Q3", "FBP_Q3", "CBA_Q3", 
                     "IBA_Q3", "ToA_Q3", "ToE_Q3","STNG_Q3", "TNG_Q3","CICO_Q4","TLC_Q4","TST_Q4", "DSP_Q4", "HIV_Q4", 
                     "CLIP_Q4", "FBP_Q4", "CBA_Q4", "IBA_Q4", "ToA_Q4", "ToE_Q4","STNG_Q4", "TNG_Q4")
testData <- cbind(aggregateScores, dataFY19[2:length(dataFY19)])

# remove columns with all NAs across all quarters
na_vars <- c('TST_Q1','TST_Q2','TST_Q3','TST_Q4','CLIP_Q1','CLIP_Q2','CLIP_Q4',
             'STNG_Q1','STNG_Q2','STNG_Q3','STNG_Q4')
testData %>% select(-all_of(na_vars)) -> testData

## Removing uninformative variables (contains only 1 factor level or NA values)
rm_vars <- c('MEPS','ToE_Q1','ToE_Q2','ToE_Q3','ToE_Q4','ToA_Q1','ToA_Q2','ToA_Q3','ToA_Q4')
testData %>% select(-all_of(rm_vars)) -> testData

# set relevant variables as factors
factor_vars = c('TLC_Q1','TLC_Q2','TLC_Q3','TLC_Q4','CBA_Q1','CBA_Q2','CBA_Q3','CBA_Q4',
                'IBA_Q1','IBA_Q2','IBA_Q3','IBA_Q4')
for (var in factor_vars){
  testData[,var] %>% factor() -> testData[,var]
}

# center and scale numerical predictor variables to reduce collinearity
testData %>% select(-FY18_Percent,-contains("CICO_"),-contains("DSP_")) %>% 
  select_if(is.numeric) %>% colnames() -> num_vars
testData[,num_vars] %>% scale() -> testData[,num_vars]

testData %>% select(-DSP_Q4,-contains("TLC_")) -> testData
summary(testData)

##### load FY19 quarterly data ###
# Q1
FY19Q1 <- read.xlsx(filename,
                    sheet = "FY19 Q1",
                    rows = c(2:67),
                    cols = c(3:15,19),
                    colNames = TRUE,
                    na.strings = 'N/A')
names(FY19Q1) <- c("CICO_Q1","TLC_Q1","TST_Q1", "DSP_Q1", "HIV_Q1", "CLIP_Q1", "FBP_Q1", 
                   "CBA_Q1", "IBA_Q1", "ToA_Q1", "ToE_Q1","STNG_Q1", "TNG_Q1",'FY18_Percent')
na_varsq1 <- c('TST_Q1','CLIP_Q1','STNG_Q1')
FY19Q1 %>% select(-all_of(na_varsq1)) -> FY19Q1
rm_varsq1 <- c('ToE_Q1','ToA_Q1')
FY19Q1 %>% select(-all_of(rm_varsq1)) -> FY19Q1
factor_varsq1 = c('TLC_Q1','CBA_Q1','IBA_Q1')
for (var in factor_varsq1){
  FY19Q1[,var] %>% factor() -> FY19Q1[,var]
}
FY19Q1 %>% select(-FY18_Percent,-contains("CICO_"),-contains("DSP_")) %>% 
  select_if(is.numeric) %>% colnames() -> num_vars
FY19Q1[,num_vars] %>% scale() -> FY19Q1[,num_vars]

FY19Q1 %>% select(-contains("TLC_")) -> FY19Q1

summary(FY19Q1)

# Q2
FY19Q2 <- read.xlsx(filename,
                    sheet = "FY19 Q2",
                    rows = c(2:67),
                    cols = c(3:14,16,20),
                    colNames = TRUE,
                    na.strings = 'N/A')
names(FY19Q2) <- c("CICO_Q2","TLC_Q2","TST_Q2", "DSP_Q2", "HIV_Q2", "CLIP_Q2", "FBP_Q2", 
                   "CBA_Q2", "IBA_Q2", "ToA_Q2", "ToE_Q2","STNG_Q2", "TNG_Q2",'FY18_Percent')
na_varsq2 <- c('TST_Q2','CLIP_Q2','STNG_Q2')
FY19Q2 %>% select(-all_of(na_varsq2)) -> FY19Q2
rm_varsq2 <- c('ToE_Q2','ToA_Q2')
FY19Q2 %>% select(-all_of(rm_varsq2)) -> FY19Q2
factor_varsq2 = c('TLC_Q2','CBA_Q2','IBA_Q2')
for (var in factor_varsq2){
  FY19Q2[,var] %>% factor() -> FY19Q2[,var]
}
FY19Q2 %>% select(-FY18_Percent,-contains("CICO_"),-contains("DSP_")) %>% 
  select_if(is.numeric) %>% colnames() -> num_vars
FY19Q2[,num_vars] %>% scale() -> FY19Q2[,num_vars]

FY19Q2 %>% select(-contains("TLC_")) -> FY19Q2

summary(FY19Q2)

# Q3
FY19Q3 <- read.xlsx(filename,
                    sheet = "FY19 Q3",
                    rows = c(2:67),
                    cols = c(3:14,16,20),
                    colNames = TRUE,
                    na.strings = 'N/A')
names(FY19Q3) <- c("CICO_Q3","TLC_Q3","TST_Q3", "DSP_Q3", "HIV_Q3", "CLIP_Q3", "FBP_Q3", 
                   "CBA_Q3", "IBA_Q3", "ToA_Q3", "ToE_Q3","STNG_Q3", "TNG_Q3",'FY18_Percent')
na_varsq3 <- c('TST_Q3','STNG_Q3')
FY19Q3 %>% select(-all_of(na_varsq3)) -> FY19Q3
rm_varsq3 <- c('ToE_Q3','ToA_Q3')
FY19Q3 %>% select(-all_of(rm_varsq3)) -> FY19Q3
factor_varsq3 = c('TLC_Q3','CBA_Q3','IBA_Q3')
for (var in factor_varsq3){
  FY19Q3[,var] %>% factor() -> FY19Q3[,var]
}
FY19Q3 %>% select(-FY18_Percent,-contains("CICO_"),-contains("DSP_")) %>% 
  select_if(is.numeric) %>% colnames() -> num_vars
FY19Q3[,num_vars] %>% scale() -> FY19Q3[,num_vars]

FY19Q3 %>% select(-contains("TLC_")) -> FY19Q3

summary(FY19Q3)

# Q4
FY19Q4 <- read.xlsx(filename,
                    sheet = "FY19 Q4",
                    rows = c(2:67),
                    cols = c(3:14,16,20),
                    colNames = TRUE,
                    na.strings = 'N/A')
names(FY19Q4) <- c("CICO_Q4","TLC_Q4","TST_Q4", "DSP_Q4", "HIV_Q4", "CLIP_Q4", "FBP_Q4", 
                   "CBA_Q4", "IBA_Q4", "ToA_Q4", "ToE_Q4","STNG_Q4", "TNG_Q4",'FY18_Percent')
na_varsq4 <- c('TST_Q4','CLIP_Q4','STNG_Q4')
FY19Q4 %>% select(-all_of(na_varsq4)) -> FY19Q4
rm_varsq4 <- c('ToE_Q4','ToA_Q4')
FY19Q4 %>% select(-all_of(rm_varsq4)) -> FY19Q4
factor_varsq4 = c('TLC_Q4','CBA_Q4','IBA_Q4')
for (var in factor_varsq4){
  FY19Q4[,var] %>% factor() -> FY19Q4[,var]
}
FY19Q4 %>% select(-FY18_Percent,-contains("CICO_"),-contains("DSP_")) %>% 
  select_if(is.numeric) %>% colnames() -> num_vars
FY19Q4[,num_vars] %>% scale() -> FY19Q4[,num_vars]

FY19Q4 %>% select(-DSP_Q4,-contains("TLC_")) -> FY19Q4

summary(FY19Q4)

##### outputs testData and FY19 data for each quarter #####

##### Data Prep for Logistic Regression #####
log_dataFY19_Overall = read.xlsx(filename,
                                 sheet = "FY19 Overall",
                                 rows = c(2:67),
                                 cols = c(1:5),
                                 colNames = TRUE,
                                 na.strings = 'N/A')
names(log_dataFY19_Overall) <- c("MEPS","FY19_Possible","FY19_Earned","FY19_Percent","FY19_Rank")

log_dataFY19 <- dataFY19_Overall["MEPS"]
sheet <- "FY19 Q1"
tmp <- read.xlsx(filename,
                 sheet = sheet,
                 rows = c(2:67),
                 cols = c(3:15,19),
                 colNames = TRUE,
                 na.strings = 'N/A')
log_dataFY19 <- cbind(log_dataFY19, tmp)
for(i in 2:4){
  sheet <- paste0("FY19 Q", i)
  tmp <- read.xlsx(filename,
                   sheet = sheet,
                   rows = c(2:67),
                   cols = c(3:14,16,20),
                   colNames = TRUE,
                   na.strings = 'N/A')
  log_dataFY19 <- cbind(log_dataFY19, tmp)
}

names(log_dataFY19) <- c("MEPS","CICO_Q1","TLC_Q1","TST_Q1", "DSP_Q1", "HIV_Q1", "CLIP_Q1", "FBP_Q1", 
                     "CBA_Q1", "IBA_Q1", "ToA_Q1", "ToE_Q1","STNG_Q1", "TNG_Q1","Per_Q1","CICO_Q2","TLC_Q2","TST_Q2", "DSP_Q2", 
                     "HIV_Q2", "CLIP_Q2", "FBP_Q2", "CBA_Q2", "IBA_Q2", "ToA_Q2", "ToE_Q2","STNG_Q2", "TNG_Q2","Per_Q2",
                     "CICO_Q3","TLC_Q3","TST_Q3", "DSP_Q3", "HIV_Q3", "CLIP_Q3", "FBP_Q3", "CBA_Q3", 
                     "IBA_Q3", "ToA_Q3", "ToE_Q3","STNG_Q3", "TNG_Q3","Per_Q3","CICO_Q4","TLC_Q4","TST_Q4", "DSP_Q4", "HIV_Q4", 
                     "CLIP_Q4", "FBP_Q4", "CBA_Q4", "IBA_Q4", "ToA_Q4", "ToE_Q4","STNG_Q4", "TNG_Q4","Per_Q4")

for(i in 1:nrow(log_dataFY19)){   
  count=0
  if(log_dataFY19[i,"Per_Q1"]>=.90){count = count+1}
  if(log_dataFY19[i,"Per_Q2"]>=.90){count = count+1}
  if(log_dataFY19[i,"Per_Q3"]>=.90){count = count+1}
  if(log_dataFY19[i,"Per_Q4"]>=.90){count = count+1}
  log_dataFY19[i,"FY19UNIT"] = count
}
log_aggscores <- cbind(log_dataFY19["MEPS"], log_dataFY19["FY19UNIT"])
log_dataFY18 <- log_dataFY19_Overall["MEPS"]

for(i in 1:4){
  sheet <- paste0("FY18 Q", i)
  tmp <- read.xlsx(filename,
                   sheet = sheet,
                   rows = c(2:67),
                   cols = c(3:16,20),
                   colNames = TRUE,
                   na.strings = 'N/A')
  log_dataFY18 <- cbind(log_dataFY18, tmp)
}

names(log_dataFY18) <- c("MEPS","BE_Q1","CICO_Q1","TLC_Q1","TST_Q1", "DSP_Q1", "HIV_Q1", "CLIP_Q1", "FBP_Q1","IRP_Q1", 
                         "CBA_Q1", "IBA_Q1", "ToA_Q1", "ToE_Q1", "TNG_Q1","Per_Q1","BE_Q2","CICO_Q2","TLC_Q2","TST_Q2", "DSP_Q2", 
                         "HIV_Q2", "CLIP_Q2", "FBP_Q2","IRP_Q2", "CBA_Q2", "IBA_Q2", "ToA_Q2", "ToE_Q2", "TNG_Q2","Per_Q2","BE_Q3",
                         "CICO_Q3","TLC_Q3","TST_Q3", "DSP_Q3", "HIV_Q3", "CLIP_Q3", "FBP_Q3","IRP_Q3", "CBA_Q3", "IBA_Q3", 
                         "ToA_Q3", "ToE_Q3", "TNG_Q3","Per_Q3","BE_Q4","CICO_Q4","TLC_Q4","TST_Q4", "DSP_Q4", "HIV_Q4", "CLIP_Q4", 
                         "FBP_Q4","IRP_Q4", "CBA_Q4", "IBA_Q4", "ToA_Q4", "ToE_Q4", "TNG_Q4","Per_Q4")
logprojectData <- cbind(log_aggscores, log_dataFY18[2:61])
log_testData <- log_dataFY19

hs <- createStyle(fontName = "Times New Roman", fontSize = 12, border = "bottom", borderStyle = "thin",
                  textDecoration = "bold")
write.xlsx(logprojectData, "logprojectData.xlsx", headerStyle = hs)

# for(i in 1:nrow(logprojectData)){   
#   count=0
#   if(logprojectData[i,"Per_Q1"]>=.90){count = count+1}
#   if(logprojectData[i,"Per_Q2"]>=.90){count = count+1}
#   if(logprojectData[i,"Per_Q3"]>=.90){count = count+1}
#   if(logprojectData[i,"Per_Q4"]>=.90){count = count+1}
#   logprojectData[i,"FY18UNIT"] = count
# }

na_vars <- c('TST_Q1','TST_Q2','TST_Q3','TST_Q4','CLIP_Q1','CLIP_Q2','CLIP_Q4')
logprojectData %>% select(-all_of(na_vars)) -> logprojectData
test_na_vars <- c('TST_Q1','TST_Q2','TST_Q3','TST_Q4','CLIP_Q1','CLIP_Q2','CLIP_Q4',
             'STNG_Q1','STNG_Q2','STNG_Q3','STNG_Q4')
log_testData %>% select(-all_of(test_na_vars)) -> log_testData
rm_vars <- c('MEPS','ToE_Q1','ToE_Q2','ToE_Q3','ToE_Q4','ToA_Q1','ToA_Q2','ToA_Q3','ToA_Q4')
logprojectData %>% select(-all_of(rm_vars)) -> logprojectData
log_testData %>% select(-all_of(rm_vars)) -> log_testData
factor_vars = c('TLC_Q1','TLC_Q2','TLC_Q3','TLC_Q4','CBA_Q1','CBA_Q2','CBA_Q3','CBA_Q4',
                'IBA_Q1','IBA_Q2','IBA_Q3','IBA_Q4')
for (var in factor_vars){
  logprojectData[,var] %>% factor() -> logprojectData[,var]
}
for (var in factor_vars){
  log_testData[,var] %>% factor() -> log_testData[,var]
}

logprojectData %>% select(-FY19UNIT,-contains('Per_'),-contains("CICO_"),-contains("DSP_")) %>% select_if(is.numeric) %>% colnames() -> num_vars
logprojectData[,num_vars] %>% scale() -> logprojectData[,num_vars]
log_testData[,num_vars] %>% scale() -> log_testData[,num_vars]

logprojectData %>% select(-DSP_Q4,-contains("TLC_")) -> logprojectData
log_testData %>% select(-DSP_Q4,-contains("TLC_")) -> log_testData

summary(logprojectData)
summary(log_testData)
##### outputs logprojectData #####
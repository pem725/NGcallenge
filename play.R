library(readr)
train <- read_csv("~/analytics challenge/AnalyticsChallenge1-Train.csv")
train$Attrition = as.factor(train$Attrition)
train$BusinessTravel = as.factor(train$BusinessTravel)
train$Department = as.factor(train$Department)
train$EducationField = as.factor(train$EducationField)
train$Gender = as.factor(train$Gender)
train$JobRole = as.factor(train$JobRole)
train$MaritalStatus = as.factor(train$MaritalStatus)
train$Over18 = as.factor(train$Over18)
train$OverTime = as.factor(train$OverTime)
train = train[c(-9,-10,-22)]
test <- read_csv("~/analytics challenge/AnalyticsChallenge1-Testing.csv")
test$BusinessTravel = as.factor(test$BusinessTravel)
test$Department = as.factor(test$Department)
test$EducationField = as.factor(test$EducationField)
test$Gender = as.factor(test$Gender)
test$JobRole = as.factor(test$JobRole)
test$MaritalStatus = as.factor(test$MaritalStatus)
test$Over18 = as.factor(test$Over18)
test$OverTime = as.factor(test$OverTime)
test = test[c(-8,-9,-21)]



library(kernlab)
sfit_lin = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = "vanilladot")
sfit_pol = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = "polydot")
sfit_rad = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = 'rbfdot')
sfit_tan = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = 'tanhdot')
sfit_lap = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = 'laplacedot')
sfit_bes = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = 'besseldot')
sfit_ANV = ksvm(Attrition ~ ., data = train, type = "C-svc", kernel = 'anovadot')

sfit_lin
sfit_pol
sfit_rad
sfit_tan
sfit_lap
sfit_bes
sfit_ANV

stest_lin = predict(sfit_lin, train)
stest_lin
table(stest_lin, train$Attrition)
agree = stest_lin == train$Attrition
table(agree)
prop.table(table(agree))

stest_bes = predict(sfit_bes, train)
stest_bes
table(stest_bes, train$Attrition)
agree2 = stest_bes == train$Attrition
table(agree2)
prop.table(table(agree2))

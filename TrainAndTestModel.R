train <- read.csv("./AnalyticsChallenge1-Train.csv",header=T)
test <- read.csv("./AnalyticsChallenge1-Testing.csv",header=T)

library(rpart)

# grow tree
fit <- rpart(Attrition~Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeCount+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StandardHours+StockOptionLevel+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,method="class",data=train)
printcp(fit)
plotcp(fit)
summary(fit)

# plot tree
plot(fit,uniform=T,main="Classificaiton for Attrition - 1st pass")
text(fit,use.n=T,all=T,cex=.7)

### An improved predictive model using random forest prections
library(randomForest)
fitrf <- randomForest(Attrition~Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeCount+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StandardHours+StockOptionLevel+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data=train)
print(fitrf)
importance(fitrf)
fitrf
plot(fitrf)

### now an even better tool...

library(devtools)
devtools::install_github("PhilippPro/OOBCurve")

library(ranger)
library(randomForest)
library(mlr)

df <- train
df$EmployeeNumber <- NULL
classif.task <- makeClassifTask(id="train",data=df,target="Attrition")

# Classification
mod = randomForest(Attrition~Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeCount+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StandardHours+StockOptionLevel+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data=train, num.trees = 500, keep.inbag = TRUE)
# Alternatively use ranger
# mod = ranger(Class ~., data = data, ntree = 100, keep.inbag = TRUE)
# Alternatively use train of mlr
# mod = train(makeLearner("classif.ranger", keep.inbag = TRUE), sonar.task)

# Application of the main function
results = OOBCurve::OOBCurve(mod, measures = list(mmce, auc, brier), task = classif.task, data = train)
# Plot the generated results
plot(results$mmce, type = "l", ylab = "oob-mmce", xlab = "ntrees")
plot(results$auc, type = "l", ylab = "oob-auc", xlab = "ntrees")
plot(results$brier, type = "l", ylab = "oob-brier-score", xlab = "ntrees")

## ok, this is a start.  More later


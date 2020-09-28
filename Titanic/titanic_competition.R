rm(list = ls(all.names = T))


library(tidyverse)
library(PerformanceAnalytics)
library(C50)
library(gmodels)


# # regression methods
# reg_data <- read_csv("train.csv", col_names = TRUE)
# 
# reg_data$Pclass <- as.factor(recode(reg_data$Pclass, "1" = "First", "2" = "Second", "3" = "Third"))
# reg_data$Sex <- as.factor(reg_data$Sex)
# reg_data$Embarked <- as.factor(reg_data$Embarked)
# 
# # remove unnecessary features
# str(reg_data)
# 
# # create training set
# set.seed(123)
# train_i <- sample(nrow(reg_data), .75*nrow(reg_data))
# 
# train_data <- reg_data[train_i,]
# test_data <- reg_data[-train_i,]
# 
# # test training set proportion
# prop.table(table(train_data$Survived)) # good seed value
# 
# 
# # run regression
# log_model <- glm(formula = Survived ~ ., data = train_data[,-c(1,4,9,11)], family = "binomial")
# 
# # get predictions
# predictions <- predict(log_model, newdata = test_data[,-c(1,4,9,11)], type = "response")
# status <- ifelse(predictions > .5, 1, 0)
# 
# 
# 


# decisions tree method ---- 

# load data
tdata <- read_csv("train.csv", col_names = TRUE)

# Which features seem to be good predictors of passenger survival?
# sex, 

# remove unimportant features based on inspection
tdata <- select(tdata,-c(1,4,9,11))

# convert remaining features to factor or numeric for tree algo                  
tdata$Pclass <-  as.factor(tdata$Pclass)
tdata$Survived <- as.factor(recode(tdata$Survived, "1" = "Yes", "0" = "No"))
tdata$Sex <- as.factor(tdata$Sex)
tdata$Embarked <- as.factor(tdata$Embarked)
tdata$SibSp <- as.factor(tdata$SibSp)
tdata$Parch <- as.factor(tdata$Parch)

# impute missing ages
summary(tdata$Age) # impute the median opposed to the mean

# Impute missing age values with the median age
#tdata$Age <- ifelse(is.na(tdata$Age), round(median(tdata$Age, na.rm = TRUE), 2), tdata$Age)

# Create missing value indicator for age
#donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

# view the structure
str(tdata)
summary(tdata)

# check original proportions
prop.table(table(tdata$Survived))

# create training set
set.seed(123)
train_i <- sample(nrow(tdata), .75*nrow(tdata))

train_data <- tdata[train_i,]
test_data <- tdata[-train_i,]

# test training set proportion
prop.table(table(train_data$Survived)) # good seed value

# EDA 

# following shows that as class decreases odd of survival decrease
CrossTable(tdata$Survived, tdata$Pclass, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# following shows minor increase in order of s,q,c - may be negligible 
CrossTable(tdata$Survived, tdata$Embarked, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# following shows that many more women survived than men
CrossTable(tdata$Survived, tdata$Sex, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

str(tdata)




# EDA END


# create the c50 model with boosting: trials = 10
#error_cost <- matrix(c(1,4,4,1), nrow = 2)
model <- C5.0(formula = Survived~., data = tdata, trials = 10)

summary(model)

predictions <- predict(model, tdata[,-1])



CrossTable(predictions, tdata$Survived, prop.r = F, prop.c = F, prop.chisq = F)



# # create the random forest model
# rf_model <- randomForest(train_data[,-c(1,4)], train_data$Survived, na.action = F)
# 
# summary(rf_model)
# 
# rf_testset <- na.omit(test_data[,-c(1,4)])
# 
# rf_predictions <- predict(rf_model, rf_testset)
# 
# CrossTable(rf_predictions, test_data$Survived, prop.r = F, prop.c = F, prop.chisq = F)






# Kaggle Submission
     
     





rm(list = ls(all.names = T))


library(tidyverse)
library(PerformanceAnalytics)
library(C50)
library(gmodels)
library(htmlwidgets)
library(randomForest)



# ATTEMPT 1: DECISION TREE METHOD


# QUESTIONS TO MAKE A GOOD MODEL
# 1. Which features seem to be good predictors of passenger survival?
# 2. Can any features be created?



# STEP 1:
# load data
tdata <- read_csv("train.csv", col_names = TRUE)


# PUT CLEANING AND CONVERTING STEPS INTO A FUNCTION AND CALL FUNCTION

clean_titanic_data <- function(tdata){
     
     # STEP 2:
     # REMOVE INSIGNIFICANT FEATURES
     tdata$Cabin <- NULL
       
        
         
     # STEP 3:
     # CLEAN AND CONVERT TICKET COLUMN TO TWO VECTORS THAT MIGHT HAVE CORRELATION
     # strip letters and punctuation from ticket column and convert to numeric
     while (sum(str_detect(tdata$Ticket,"[[:punct:]]")) > 0){
          
          tdata$Ticket <- str_replace(tdata$Ticket,"[[:punct:]]","")
          
     }
     tdata$Ticket <- str_trim(str_replace(tdata$Ticket,"[ABCDEFGHIJKLMNOPQRTSTUVWXYZ].+\\s", ""))
     tdata$Ticket <- str_trim(str_replace(tdata$Ticket,"[ABCDEFGHIJKLMNOPQRTSTUVWXYZ]\\s", ""))
     tdata$Ticket <- str_trim(str_replace(tdata$Ticket,"[ABCDEFGHIJKLMNOPQRTSTUVWXYZ].+$", "0"))
     
     
     tdata$TicketLength <- as.factor(nchar(tdata$Ticket)) # new feature
     tdata$TicketLeft <- factor(str_sub(tdata$Ticket, start = 1, end = 1), levels = c(0:9)) # new feature
     
     #table(factor(tdata$Survived), factor(tdata$TicketLength))
     #table(factor(tdata$Pclass), factor(tdata$TicketLength))
     
     
     
     # STEP 4:
     # CORRECT NA VALUES IN EMBARKED VECTOR
     # impute mode (S) for embarked NAs
     tdata$Embarked <- as.factor(ifelse(is.na(tdata$Embarked), "S", tdata$Embarked))
     
     
     
     # STEP 5:
     # CORRECT NA VALUES IN AGE VECTOR
     # impute missing age values with the median age by Pclass group
     # this makes sense because different class tickets have different ages generally, the more successful the higher the age, typically
     tdata$Age <- ifelse((is.na(tdata$Age)) & (tdata$Pclass == "1") & (tdata$Sex == "female"), 
                         35, tdata$Age)
     tdata$Age <- ifelse((is.na(tdata$Age)) & (tdata$Pclass == "1") & (tdata$Sex == "male"), 
                         40, tdata$Age)
     tdata$Age <- ifelse((is.na(tdata$Age)) & (tdata$Pclass == "2") & (tdata$Sex == "female"), 
                         28, tdata$Age)
     tdata$Age <- ifelse((is.na(tdata$Age)) & (tdata$Pclass == "2") & (tdata$Sex == "male"), 
                         30, tdata$Age)
     tdata$Age <- ifelse((is.na(tdata$Age)) & (tdata$Pclass == "3") & (tdata$Sex == "female"), 
                         21.5, tdata$Age)
     tdata$Age <- ifelse((is.na(tdata$Age)) & (tdata$Pclass == "3") & (tdata$Sex == "male"), 
                         25, tdata$Age)
     
     
     # STEP 5.5: NOT APPLICABLE UNTIL REACHING TEST SET
     # CORRECT NA VALUES IN FARE VECTOR
     # impute fare values with the average of similar group: ~ $7
     if( sum(is.na(tdata$Fare)) > 0){
             tdata$Fare <- ifelse((is.na(tdata$Fare)), 7, tdata$Fare)  
     }
     
     
     
     # STEP 6: 
     # CREATE BINNED AGE VECTOR 
     # set up cut-off values 
     age_breaks <- c(0,18,28,40,60,80)
     #age_breaks <- c(0,10,20,30,40,50,60,70,80)
     
     # specify interval/bin labels
     age_tags <- c("[0-18)","[18-28)", "[28-40)", "[40-60)", "[60-80)")
     #age_tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[60-70)", "[70,80)")
     
     # bucketing values into bins
     age_vector<- cut(tdata$Age, 
                      breaks = age_breaks, 
                      include.lowest = TRUE, 
                      right = FALSE, 
                      labels = age_tags)
     
     tdata$AgeBin <- as.factor(age_vector)
     
     
     
     # STEP 6.5: MAYBE ADD A FEATURE VECTOR THAT SHOWS WHICH AGES HAVE BEEN IMPUTED.
     
     
     
     # STEP 7:
     # CONVERT REMAINING FEATURES TO FACTORS                
     tdata$Pclass <-  as.factor(tdata$Pclass)
     if ("Survived" %in% colnames(tdata)){
             tdata$Survived <- as.factor(recode(tdata$Survived, "1" = "Yes", "0" = "No"))   
     }
     tdata$Sex <- as.factor(tdata$Sex)
     tdata$SibSp <- as.factor(tdata$SibSp)
     tdata$Parch <- ifelse(!(tdata$Parch %in% c(0:6)), 6, tdata$Parch) 
     tdata$Parch <- as.factor(tdata$Parch)
     
     # features to keep
     if ("Survived" %in% colnames(tdata)){
             kept_features <- c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked", "TicketLeft", "AgeBin")
     }
     else {
             kept_features <- c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked", "TicketLeft", "AgeBin")
     }
     
     return(select(tdata,kept_features))
     
}

# call function
tdata <- clean_titanic_data(tdata = tdata)



# STEP 8:
# VIEW THE STRUCTURE AND CHECK FOR MISSING VALUES AND OUTLIERS
str(tdata)
summary(tdata)



# STEP 9: 
# QUICK FACTOR EDA TO DETERMINE FEATURES TO KEEP

# the following shows that as class decreases odds of survival increase (higher status = better chances)
CrossTable(tdata$Survived, tdata$Pclass, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# the following shows survival by embarked from: S ~ 34%, Q ~ 39%, C ~ 55%
CrossTable(tdata$Survived, tdata$Embarked, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# the following shows that many more women survived than men: F ~ 74%, M ~ 19%
CrossTable(tdata$Survived, tdata$Sex, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# breakdown: 0 ~ 34%, 1 ~ 54%, 2 ~ 46%, 3 ~ 25%, 4 ~ 17%, 5 & 8 ~ 0%
CrossTable(tdata$Survived, tdata$SibSp, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# breakdown: 0 ~ 34%, 1:3 ~ 50-60%, 4-6 ~ 0%
CrossTable(tdata$Survived, tdata$Parch, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

# good variable because odds are stacked for 1-3, too spread out for ticket length
CrossTable(tdata$TicketLeft, tdata$Survived, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)

# the following shows that TicketLeft feature is strongly correlated with Pclass feature
# may be good to get rid of cleaned ticket feature
CrossTable(tdata$TicketLeft, tdata$Pclass, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)

# may be good to get rid of cleaned ticket feature
CrossTable(tdata$AgeBin, tdata$Survived, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)

# shows Fare at the extreme end helps to show survival
ggplot(data = tdata, mapping = aes(x = Fare))+
     geom_histogram()+
     facet_wrap(~ Survived)

# vector of kept features to put in function
# moved into the function



# STEP 10: CREATE TRAIN AND TEST SETS FOR MODEL
# check original proportions
prop.table(table(tdata$Survived))

# create training set
set.seed(123)
train_i <- sample(nrow(tdata), .75*nrow(tdata))

train_data <- tdata[train_i,]
test_data <- tdata[-train_i,]

# test training set proportion
prop.table(table(train_data$Survived)) # good seed value



# STEP 11: CREATE C50 MODEL AND GET PREDICTIONS AND ACCURACY SCORE
# create the c50 model with boosting: trials = 10
error_cost <- matrix(c(0,4,1,0), nrow = 2)
model <- C5.0(formula = Survived~., data = train_data, trials = 10)

summary(model)

# get predictions
predictions <- predict(model, test_data)

# score: 80%
mean(predictions == test_data$Survived)

# 15 false positives, 29 fale negatives
CrossTable(test_data$Survived, predictions, prop.r = F, prop.c = F, prop.chisq = F)



# STEP 12: CREATE RANDOM FOREST MODEL AND GET ACCURACY AND PREDICTIONS
# create the random forest model
rf_model <- randomForest(train_data[-1], train_data$Survived)

summary(rf_model)

# get predictions
rf_predictions <- predict(rf_model, test_data[,-1])

# score: 80%
mean(rf_predictions == test_data$Survived)

# 11 false positives, 25 false negatives
CrossTable(test_data$Survived, rf_predictions, prop.r = F, prop.c = F, prop.chisq = F)


# FINAL STEP: RUN TEST DATA THROUGH FUNCTION AND MODEL
# Kaggle Submission 1 - Random Forest Model

# load data     
kc_test_data <- read_csv("test.csv", col_names = TRUE)

# copy passenger IDs prior to functions
kc_submission <- kc_test_data[,1]

# clean data
kc_test_data <- clean_titanic_data(kc_test_data) 


# create model using full training data to get the highest utility------------------------------
# STEP 12: CREATE RANDOM FOREST MODEL AND GET ACCURACY AND PREDICTIONS
# create the random forest model
rf_model_kc <- randomForest(tdata[,-1], tdata$Survived)

summary(rf_model_kc)

# get predictions
rf_predictions_kc_test <- predict(rf_model_kc, tdata[,-1])

# score: 90% - to be expected when using data that trained the model for a test, this is just to get info
mean(rf_predictions_kc_test == tdata$Survived)

# 21 false positives, 66 false negatives - against itself
CrossTable(tdata$Survived, rf_predictions_kc_test, prop.r = F, prop.c = F, prop.chisq = F)
#------------------------------------------------------------------------------------------------


# get predictions from random forest model on actual test data
kc_predictions <- predict(rf_model_kc, newdata = kc_test_data)
     
# join predictions back to the original passenger IDs and conver to 0 1
kc_submission$Survived <- kc_predictions
kc_submission$Survived <- as.factor(recode(kc_submission$Survived, "Yes" = "1", "No" = "0"))

# view submission data
view(kc_submission)

# export to csv for submission
write_csv(kc_submission, "kc_submission_perez.csv")



# FINAL STEP: RUN TEST DATA THROUGH FUNCTION AND MODEL
# Kaggle Submission 2 - Logistic Regression Model

# create the log model
lr_model <- glm(Survived~., data = tdata, family = "binomial")
summary(lr_model)

# get predictions
kc_predictions_lr <- predict(model, newdata = kc_test_data, type = "class")

# create survival vector
survival_vector <- ifelse(kc_predictions_lr == "Yes", "1", "0")

# combine to passengerIDs
kc_submission$Survived <- survival_vector

# export to csv for submission
write_csv(kc_submission, "kc_submission_2_perez.csv")




# METHODS TO TRY LATER IN ORDER TO IMPROVE MODEL
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





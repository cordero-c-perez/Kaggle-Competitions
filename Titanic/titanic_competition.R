
rm(list = ls(all.names = T))


library(tidyverse)
library(gmodels)
library(htmlwidgets)
library(randomForest)
library(fastDummies)
library(caret)
library(irr)
library(C50)



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
     # CREATE TITLE FEATURE
     tdata$Title <- str_extract(tdata$Name,",.+\\.") %>% str_replace(", ","") %>% str_replace("Mrs.+", "Mrs.")
     
     
     
     # FIX CABIN FEATURE BY EXTRACTING FIRST CHARACTER
     tdata$Cabin <- ifelse(is.na(tdata$Cabin), "U", str_sub(tdata$Cabin, start = 1, end = 1))
     
        
         
     # STEP 3:
     # CLEAN AND CONVERT TICKET COLUMN TO TWO VECTORS THAT MIGHT HAVE CORRELATION
     # strip letters and punctuation from ticket column and convert to numeric
     # while (sum(str_detect(tdata$Ticket,"[[:punct:]]")) > 0){
     #      
     #      tdata$Ticket <- str_replace(tdata$Ticket,"[[:punct:]]","")
     #      
     # }
     # tdata$Ticket <- str_trim(str_replace(tdata$Ticket,"[ABCDEFGHIJKLMNOPQRTSTUVWXYZ].+\\s", ""))
     # tdata$Ticket <- str_trim(str_replace(tdata$Ticket,"[ABCDEFGHIJKLMNOPQRTSTUVWXYZ]\\s", ""))
     # tdata$Ticket <- str_trim(str_replace(tdata$Ticket,"[ABCDEFGHIJKLMNOPQRTSTUVWXYZ].+$", "0"))
     # 
     # 
     # tdata$TicketLength <- as.factor(nchar(tdata$Ticket)) # new feature
     # tdata$TicketLeft <- factor(str_sub(tdata$Ticket, start = 1, end = 1), levels = c(0:9)) # new feature
     # 
     # ggplot(data = tdata, mapping = aes(x = Pclass))+
     #         stat_count()+
     #         facet_wrap(~ Survived)
     tdata$Ticket <- NULL
     
     
     
     # STEP 4:
     # CORRECT NA VALUES IN EMBARKED VECTOR
     # impute mode (S) for embarked NAs
     tdata$Embarked <- as.factor(ifelse(is.na(tdata$Embarked), "S", tdata$Embarked))
     
     
     # Create Age Summary Table
     tdata_age_values <- tdata %>%
             group_by(Pclass,Sex,Title) %>% 
             summarize(median_age = median(Age, na.rm = T))
     
     
     
     # STEP 5:
     # CORRECT NA VALUES IN AGE VECTOR
     # impute missing age values with the median age by Pclass, Sex, and Title
     
     tdata$Age <- ifelse(is.na(tdata$Age), median(tdata$Age, na.rm = T), tdata$Age)
     # for (i in 1:length(tdata$PassengerId)){
     #         
     #         if (is.na(tdata$Age[i])){
     #                 
     #                 temp <- tdata_age_values %>%
     #                         filter (Pclass == tdata$Pclass[i]) %>% 
     #                         filter (Sex == tdata$Sex[i]) %>% 
     #                         filter (Title == tdata$Title[i]) 
     #                 
     #                 tdata$Age[i] <- temp$median_age
     #         }
     # }
     
     
     
     # STEP 5.5: NOT APPLICABLE UNTIL REACHING TEST SET
     # CORRECT NA VALUES IN FARE VECTOR
     # impute fare values with the average of similar group: ~ $7
     if ("Fare" %in% colnames(tdata)){
             if (sum(is.na(tdata$Fare)) > 0){
                tdata$Fare <- ifelse((is.na(tdata$Fare)), 7, tdata$Fare)  
             }
     }
     
     
     
     # STEP 6: 
     # CREATE BINNED AGE VECTOR 
     # set up cut-off values 
     # age_breaks <- c(0,18,28,40,60,80)
     # #age_breaks <- c(0,10,20,30,40,50,60,70,80)
     # 
     # # specify interval/bin labels
     # age_tags <- c("[0-18)","[18-28)", "[28-40)", "[40-60)", "[60-80)")
     # #age_tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[60-70)", "[70,80)")
     # 
     # # bucketing values into bins
     # age_vector<- cut(tdata$Age, 
     #                  breaks = age_breaks, 
     #                  include.lowest = TRUE, 
     #                  right = FALSE, 
     #                  labels = age_tags)
     # 
     # tdata$AgeBin <- as.factor(age_vector)
     
     
     
     # STEP 6.5: MAYBE ADD A FEATURE VECTOR THAT SHOWS WHICH AGES HAVE BEEN IMPUTED
     
     
     
     # STEP 7:
     # CONVERT REMAINING FEATURES TO FACTORS                
     tdata$Pclass <-  as.factor(tdata$Pclass)
     if ("Survived" %in% colnames(tdata)){
             tdata$Survived <- as.factor(tdata$Survived)  
     }
     tdata$Sex <- as.factor(tdata$Sex)
     tdata$SibSp <- as.factor(tdata$SibSp)
     tdata$Parch <- ifelse(!(tdata$Parch %in% c(0:6)), 6, tdata$Parch) 
     tdata$Parch <- as.factor(tdata$Parch)
     tdata$Fare <- scale(tdata$Fare)
     
     
     
     # CREATE DUMMY VARIABLES AND DROP UNIMPORTANT FEATURES
     tdata <- select(tdata, -c("PassengerId", "Name", "Parch", "Cabin", "Embarked", "SibSp")) %>% 
             dummy_cols(select_columns = c("Pclass", "Sex", "Title"), remove_selected_columns = T)
     
     
     return(tdata)
     
}

# call function
tdata <- clean_titanic_data(tdata = tdata)



# STEP 8:
# VIEW THE STRUCTURE AND CHECK FOR MISSING VALUES AND OUTLIERS
str(tdata)
summary(tdata)


# STEP 8.5:
# VIEW POSSIBLE COMBINAITONS MANUALLY IN A DATAFRAME
# tdata_comos <- tdata %>% 
#         group_by(Survived, Pclass, Sex, SibSp, Parch, Fare, Embarked, TicketLeft, AgeBin) %>% 
#         summarize(count = n())



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



# STEP 10: CREATE TRAIN AND TEST SETS FOR MODEL - CROSS VALIDATION

# create folds for 10 fold cross validation
set.seed(123)
folds <- createFolds(y = tdata$Survived, k = 10)

# define function that runs the model over each fold and retains accuracy and kappa score
cv_rf_function <- function(fold){
        train <- tdata[-fold,]
        test <- tdata[fold,]
        rf_model <- randomForest(train[-1], train$Survived)
        rf_predictions <- predict(rf_model, test[-1])
        rf_actual <- test$Survived
        mean_score <- mean(rf_predictions == rf_actual)
        kappa <- kappa2(data.frame(rf_actual, rf_predictions))$value
        return(mean_score)
}

cv_c50_function <- function(fold){
        train <- tdata[-folds$Fold01,]
        test <- tdata[folds$Fold01,]
        c50_model <- C5.0(train[-1], train$Survived)
        c50_predictions <- predict(c50_model, test[-1])
        c50_actual <- test$Survived
        mean_score <- mean(c50_predictions == c50_actual)
        kappa <- kappa2(data.frame(c50_actual, c50_predictions))$value
        return(mean_score)
}


# STEP 12: GET ACCURACY AND PREDICTIONS FROM MULTIPLE MODELS USING 10-FOLD CV

# Random Forest
rf_results <- lapply(folds, cv_rf_function)
mean(unlist(rf_results))

rf_model <- randomForest(tdata[-1], tdata$Survived)
summary(rf_model)
importance(rf_model)
varImpPlot(rf_model)


# C5.0
c50_results <- lapply(folds, cv_c50_function)
mean(unlist(c50_results))


# FINAL STEP: RUN TEST DATA THROUGH FUNCTION AND MODEL
# Kaggle Submission 1 - Random Forest Model

# load data     
kc_test_data <- read_csv("test.csv", col_names = TRUE)

# copy passenger IDs prior to functions
kc_submission <- kc_test_data[,1]

# clean data
kc_test_data <- clean_titanic_data(kc_test_data) 


# create model using full training data to get the highest utility------------------------------
# rf_model_kc <- randomForest(tdata[,-1], tdata$Survived, costs = error_cost)

# get predictions from random forest model on actual test data
kc_predictions <- predict(rf_model, newdata = kc_test_data)
     
# join predictions back to the original passenger IDs and convert to 0 1
kc_submission$Survived <- kc_predictions
kc_submission$Survived <- as.factor(recode(kc_submission$Survived, "Yes" = "1", "No" = "0"))

# view submission data
view(kc_submission)

# export to csv for submission
write_csv(kc_submission, "kc_submission_rf.csv")
#write_csv(kc_submission, "kc_submission_4_perez.csv")








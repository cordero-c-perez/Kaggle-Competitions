
rm(list = ls(all.names = T))


library(tidyverse)
#library(caret)
library(gmodels)
library(fastDummies)
library(class)
library(factoextra)
library(cluster)





# ATTEMPT 1: DECISION TREE METHOD


# QUESTIONS TO MAKE A GOOD MODEL
# 1. Which features seem to be good predictors of passenger survival?
# 2. Can any features be created?


# FUNCTIONS
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


# STEP 1:
# load data
tdata <- read_csv("test.csv", col_names = TRUE)




# PUT CLEANING AND CONVERTING STEPS INTO A FUNCTION AND CALL FUNCTION

clean_titanic_data <- function(tdata){
     
     # STEP 2:
     # REMOVE INSIGNIFICANT FEATURES
     tdata$Cabin <- NULL
     tdata$PassengerId <- NULL
     tdata$Name <- NULL
     tdata$Ticket <- NULL
     tdata$Parch <- NULL
     
     
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
     #tdata$Age <- ifelse(is.na(tdata$Age), mean(tdata$Age, na.rm = T), tdata$Age)
     
     
     
     # STEP 5.5: NOT APPLICABLE UNTIL REACHING TEST SET
     # CORRECT NA VALUES IN FARE VECTOR
     # impute fare values with the average of similar group: ~ $7
     if ("Fare" %in% colnames(tdata)){
          if (sum(is.na(tdata$Fare)) > 0){
               tdata$Fare <- ifelse((is.na(tdata$Fare)), 7, tdata$Fare)  
          }
     }
     
     

     # STEP 6.5: MAYBE ADD A FEATURE VECTOR THAT SHOWS WHICH AGES HAVE BEEN IMPUTED
     
     
     
     # STEP 7:
     # CONVERT REMAINING FEATURES TO NUMERIC AND SCALE 
     tdata <- as.data.frame(dummy_cols(tdata, select_columns = colnames(tdata)[c(1,2,4,6)], 
                                       remove_selected_columns = T)) %>% 
          mutate_at(c("Age", "Fare"), ~(range01(.) %>% as.vector))
  
     return(tdata)
     
}

# call function
tdata <- clean_titanic_data(tdata = tdata)




# STEP 10: CREATE TRAIN AND TEST SETS FOR MODEL
# check original proportions
prop.table(table(tdata$Survived))

# create training set
set.seed(123)
train_i <- sample(nrow(tdata), .80*nrow(tdata))

train_data <- tdata[train_i,]
test_data <- tdata[-train_i,]

# test training set proportion
prop.table(table(train_data$Survived)) # good seed value



# EXPLORE KNN MODEL
predictions <- knn(train = train_data[-1], test = test_data[-1], cl = train_data$Survived, k = 15)

mean(test_data$Survived == predictions)
CrossTable(test_data$Survived, predictions, prop.r = F, prop.c = F, prop.chisq = F)




# EXPLORE KMEANS MODEL
model_k <- kmeans(tdata[-1], centers = 2)
tdata <- tdata %>% mutate(cluster = model_k$cluster)
error_kmeans <- tdata %>% mutate(cluster = model_k$cluster) %>% 
     group_by(Survived,cluster) %>% summarise(count = n())


fviz_cluster(model_k, tdata[-1], geom = "point", ggtheme = theme_classic(), ellipse.type = "norm",
             main = str_to_title("cluster plot with normal cluster boundaries"))




# FINAL STEP: RUN TEST DATA THROUGH FUNCTION AND MODEL
# Kaggle Submission - Kmeans - Cluster Analysis

# load data     
kc_test_data <- read_csv("test.csv", col_names = TRUE)

# copy passenger IDs prior to functions
kc_submission <- kc_test_data[,1]

# clean data
kc_test_data <- clean_titanic_data(kc_test_data)

model_k <- kmeans(kc_test_data, centers = 2)

kc_test_data <- kc_test_data %>% mutate(cluster = model_k$cluster)


fviz_cluster(model_k, kc_test_data, geom = "point", ggtheme = theme_classic(), ellipse.type = "norm",
             main = str_to_title("cluster plot with normal cluster boundaries"))

# join predictions back to the original passenger IDs and conver to 0 1
kc_submission$Survived <- recode(kc_test_data$cluster, "2" = "0", "1" = "1")

# view submission data
view(kc_submission)

# write submission
write_csv(kc_submission, "kcs_ca.csv")




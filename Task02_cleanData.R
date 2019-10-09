# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PACKAGES
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# install.packages("gower",
# repos = "http://cran.r-project.org",
# dependencies = c("Depends", "Imports", "Suggests"))
# library(caret)

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# ASSIGNMENT

# Predict the customers' brand preferences that are missing from the incomplete surveys. 
# Conduct two classification methods in R: 
    # C5.0 or RandomForest
# Predict the brand preferences for the incomplete survey responses 
# Prepare a report with findings.

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ FILE
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load df data file
df<- read.csv("CompleteResponses.csv", header=TRUE)

# View loaded data
dim(df)
head(df)
tail(df)
typeof(df)
length(df)
class(df)
nrow(df)

# Check the data structure
str(df)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# CLEAN DATA

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Verify if any missing Values are present
anyNA(df)
sum(is.na(df))

# Check the Column Names
names(df)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# COERCION

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# typeof(df$salary)
# typeof(df$age)
# typeof(df$elevel) # should be factor?
# typeof(df$car) # should be factor?
# typeof(df$zipcode) # should be factor?
# typeof(df$credit)
# typeof(df$brand) # should be factor?

df$elevel<-as.factor(df$elevel)
df$car<-as.factor(df$car)
df$zipcode<-as.factor(df$zipcode)
df$brand<-as.factor(df$brand)

summary(df)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PRE-PROCESSING
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# NORMALIZE

# SVM and KNN are more sensitive to the scale of data than others since 
# the distance between the data points is very important.In our dataset 
# this process is not important.

# Create a function normalize().

# normalize <- function(x){
#     return ((x - min(x)) / (max(x) - min(x)))
# }

# Create Columns in the df with the normlized data
# i.e df$salary_norm<-normalize(df$salary)
# View(df)
# Plot with Historgram and Scatter Plot (salary vs salary_norm) to view

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SPLIT TRAIN AND TEST FROM THE df SET
# Using createDataPartition create training and test sets.
# As these are set inside a function traindf, testdf and fitcontrol are set as 
# global variables <<-
# return(df) is omitted as I am not returning a value

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df <- subset(df, select=c(1,2,7))
head(df)

fCV <- function(df) {
  
        # Set Seed
        set.seed(998)
        
        # Split data .20
        df<-df[sample(1:nrow(df),7000,replace=FALSE),]
        
        # define an 75%/25% train/test split of the dataset
        inTrain <- caret::createDataPartition(df$brand, p = .75, list = FALSE)
        traindf <<- df[inTrain,]
        testdf  <<- df[-inTrain,]
        
        # Create a 10 fold cross validation
        fitControl <<- caret::trainControl(method = "repeatedcv", 
                                          number = 10, 
                                          repeats = 3
        )
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RANDOM FOREST - AUTOMATIC GRID
# train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCV(df)

set.seed(997)


rfFit1 <- caret::train(brand ~.,
              data = traindf,
              method = "rf",
              trControl=fitControl,
              tuneLength = 1,
              preProcess = c("center", "scale")
            )

#training results
rfFit1

# PLOT Results
varImp(rfFit1)
plot(varImp(rfFit1),top=5)

# FINDINGS - SALARY 100, AGE 58.20, CREDIT 15.60


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RANDOM FOREST - AUTOMATIC GRID
# TRAINING SET CREATE MODEL => (tuneLength = 2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCV(df)

set.seed(996)

#train Random Forest Regression model with a tuneLenght = 2 (trains with 1 mtry value for RandomForest)
rfFit2 <- caret::train(brand ~.,
                      data = traindf,
                      method = "rf",
                      trControl=fitControl,
                      tuneLength = 2,
                      preProcess = c("center", "scale")
                      )

#training results
rfFit2

# PLOT Results
varImp(rfFit2)
plot(varImp(rfFit2),top=5)

# FINDINGS - SALARY 100, AGE 80.10 AND CREDIT 7.36

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RANDOM FOREST - MANUAL GRID

# Use Random Forest with 10-fold cross validation 
# manually tune 5 different mtry values.
# EXPAND GRID MTRY=C(1,2,3,4,5) tuneGrid=rfGrid

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCV(df)

set.seed(995)

#dataframe for manual tuning of mtry = 5
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- caret::train(brand~., 
                                    data = traindf, 
                                    method = "rf", 
                                    trControl=fitControl, 
                                    tuneGrid=rfGrid,
                                    preProcess = c("center", "scale")
                                    )
            )

#training results
rfFitm1

# PLOT Results
ggplot(rfFitm1) + theme(legend.position = "top")

varImp(rfFitm1)
plot(varImp(rfFitm1),top=5)

# FINDINGS - SALARY 100, AGE 39.62 AND CREDIT 21.73

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C5.0
# Build a model using a decision tree, C5.0 
# on the training set with 10-fold cross validation 
# an Automatic Tuning Grid 
# with a tuneLength of 2
# TRAINING SET CREATE MODEL => (tuneLength = 2)

# C5.0 uses entropy that quantifies randomness within a set of class values
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCV(df)

set.seed(994)

#train C5.0 Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit3 <- caret::train(brand ~.,
                      data = traindf,
                      method = "C5.0",
                      trControl=fitControl,
                      tuneLength = 2,
                      preProcess = c("center", "scale")
                    )

#training results
rfFit3

# PLOT Results
ggplot(rfFit3) + theme(legend.position = "top")
varImp(rfFit3)

plot(varImp(rfFit3),top=5)

# tree   FALSE   10      Accuracy: 0.9214120  Kappa: 0.8336242
# FINDINGS - SALARY 100, AGE 85.43 AND CAR3 69.87, ZIPCODE8 61.44 ELEVEL1 60.33

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# APPLY THE MODEL TO THE INCOMPLETE DATASET

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

newdf<- read.csv("SurveyIncomplete.csv", header=TRUE)

dim(newdf)
head(newdf)
str(newdf)

newdf$elevel<-as.factor(newdf$elevel)
newdf$car<-as.factor(newdf$car)
newdf$zipcode<-as.factor(newdf$zipcode)
newdf$brand<-as.factor(newdf$brand)

summary(newdf)

anyNA(newdf)

# The model that I selected was the C5.0

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PREDICTION
# Predict both classifications and probabilities for the test data set.
# As the newdf does not have any values in the brand column, we do the comparison between the 
# pred and the obs in the testdf.
# First create a new column testdf$pred_brand:
# testdf$pred_brand <- predict(rfFit3, newdata = testdf, type = "raw")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# RANDOM FOREST - AUTOMATIC GRID tuneLength = 1
testdf$pred_brand1 <- predict(rfFit1, newdata = testdf, type = "raw") #Do not use prob but raw
postResample(pred = testdf$pred_brand1, obs = testdf$brand)

# RANDOM FOREST - AUTOMATIC GRID tuneLength = 2
testdf$pred_brand2 <- predict(rfFit2, newdata = testdf, type = "raw")
postResample(pred = testdf$pred_brand2, obs = testdf$brand)

# RANDOM FOREST - MANUAL GRID
testdf$pred_brandm1 <- predict(rfFitm1, newdata = testdf, type = "raw")
postResample(pred = testdf$pred_brandm1, obs = testdf$brand)

# C5.0
testdf$pred_brand3 <- predict(rfFit3, newdata = testdf, type = "raw")
postResample(pred = testdf$pred_brand3, obs = testdf$brand)



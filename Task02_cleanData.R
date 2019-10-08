# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PACKAGES
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# install.packages("gower",
# repos = "http://cran.r-project.org",
# dependencies = c("Depends", "Imports", "Suggests"))
 library(caret)

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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SPLIT TRAIN AND TEST FROM THE df SET

# RANDOM FOREST - AUTOMATIC GRID

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TRAINING SET CREATE MODEL => (tuneLength = 1)

# Find the Top Fitting Attributes (RANDOM FOREST)
# Set Y Variable = brand
# Set Seed
set.seed(998)

# Split data .20
df<-df[sample(1:nrow(df),7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTrain<- caret::createDataPartition(df$brand, p = .75, list = FALSE)
train <- df[inTrain,]
test <- df[-inTrain,]

# Create a 10 fold cross validation
fitControl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit1 <- train(brand ~.,
                data = train,
                method = "rf",
                trControl=fitControl,
                tuneLength = 1)

#training results
rfFit1

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RANDOM FOREST - AUTOMATIC GRID
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TRAINING SET CREATE MODEL => (tuneLength = 2)

set.seed(997)

# Split data .20
df<-df[sample(1:nrow(df),7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTrain<- caret::createDataPartition(df$brand, p = .75, list = FALSE)
train <- df[inTrain,]
test <- df[-inTrain,]

# Create a 10 fold cross validation
fitControl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLenght = 2 (trains with 1 mtry value for RandomForest)
rfFit2 <- train(brand ~.,
                data = train,
                method = "rf",
                trControl=fitControl,
                tuneLength = 2)

#training results
rfFit2

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RANDOM FOREST - MANUAL GRID
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPAND GRID MTRY=C(1,2,3) tuneGrid=rfGrid

set.seed(996)

#create a 20% sample of the data
df <- df[sample(1:nrow(df), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTrain <- createDataPartition(df$brand, p = .75, list = FALSE)
train <- df[inTrain,]
test <- df[-inTrain,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(brand~., data = train, method = "rf", trControl=fitControl, tuneGrid=rfGrid))

#training results
rfFitm1

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C5.0
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TRAINING SET CREATE MODEL => (tuneLength = 1)

set.seed(995)

# Split data .20
df<-df[sample(1:nrow(df),7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTrain<- caret::createDataPartition(df$brand, p = .75, list = FALSE)
train <- df[inTrain,]
test <- df[-inTrain,]

# Create a 10 fold cross validation
fitControl <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train C5.0 Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit3 <- train(brand ~.,
                data = train,
                method = "C5.0",
                trControl=fitControl,
                tuneLength = 1)

#training results
rfFit3


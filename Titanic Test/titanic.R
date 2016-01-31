prop.table(table(train$Survived,train$Sex),1)
train$Child <- NA
train$Child[train$Age<18]<-"Child"
train$Child[train$Age>=18]<-"Adult"
train$Survived <- as.factor(train$Survived)

test <- read.csv('munged_test.csv')
test$Child <- as.factor(test$Child)
test$Child[test$Age<18]<-"Child"
test$Child[test$Age>=18]<-"Adult"



#Predicting on a copy of the test set
train_males <- train[train$Sex == "male",]
train_females <- train[train$Sex == "female",]
test_males <- test[test$Sex == "male",]
test_females <- test[test$Sex == "female",]
test_males$Survived <- 0
test_females$Survived <-0

#Checking the ocurrence of missing values
require(Amelia)
missmap(train,main = "Titanic training data: Missing Values", col = c("yellow","black")
        , legend = FALSE)

#Visualizing data
#Seeing who survived
barplot(table(train$Survived),names.arg = c("Died","Survived"),
        main= "Survived(Passengers' fate)",col = 'black');
# Seeing different pclasses
barplot(table(train$Pclass),names.arg = c("first","second","broke"),
        main= "Pclass(Passengers' travelling class)",col = 'firebrick');
#Genders
barplot(table(train$Sex), main="Sex (gender)", col="darkviolet")
#Age
hist(train$Age, main="Age", xlab = NULL, col="brown")
# Siblings and spouse
barplot(table(train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
# Parents and children
barplot(table(train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
# Fare paid
hist(train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
# Embarked place
barplot(table(train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

#Passengers fate by travelling class
mosaicplot(train$Pclass ~ train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

#Passengers' fate by Sex
mosaicplot(train$Sex ~ train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

# Males' fate by travelling class
mosaicplot(train_males$Pclass ~ train_males$Survived, 
           main="Males Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")
# Males' fate by SibSp
mosaicplot(train_males$SibSp ~ train_males$Survived, 
           main="males Fate by Siblings and spouse", shade=FALSE, 
           color=TRUE, xlab="SibSp", ylab="Survived")

# Males' fate by Parch
mosaicplot(train_males$Parch ~ train_males$Survived, 
           main="males Fate by Parents and Children", shade=FALSE, 
           color=TRUE, xlab="Parch", ylab="Survived")

# Females' fate by travelling class
mosaicplot(train_females$Pclass ~ train_females$Survived, 
           main="females Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

# Females' fate by SibSp
mosaicplot(train_females$SibSp ~ train_females$Survived, 
           main="Females Fate by Siblings and Spouses", shade=FALSE, 
           color=TRUE, xlab="SibSp", ylab="Survived")

# Females' fate by Parch
mosaicplot(train_females$Parch ~ train_females$Survived, 
           main="Females Fate by Parents and children", shade=FALSE, 
           color=TRUE, xlab="Parch", ylab="Survived")


# What about age?
boxplot(train$Age ~ train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

# Just Males
boxplot(train$Age ~ train$Survived, 
        main="Guys:Passenger Fate by Age",
        xlab="Survived", ylab="Age")

# Just Females
boxplot(train$Age ~ train$Survived, 
        main="Girls:Passenger Fate by Age",
        xlab="Survived", ylab="Age")
#All plots have the same average and quartiles

#Finally, checking ports of embarkation
mosaicplot(train$Embarked ~ train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

# Doing a correlogram
require(corrgram)
corrgram.data <- train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

# FOR MALES
corrgram.data <- train_males
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Males Titanic Training Data")

# FOR FEMALES
corrgram.data <- train_females
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

# Summarizing age
summary(train$Age)
# I could substitute missing values with the average age, but there are too many missing values
boxplot(train$Age ~ train$Pclass, 
        main="Passenger class by Age",
        xlab="Pclass", ylab="Age")

# I could actually average age by assigning the Pclass average, but there's a better approach

# Get the titles to determine age more accurately

## function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

#Somehow the function just doesn't work
title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", test$Name, TRUE)
title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
test$Title <- substr(test$Name, title.dot.start+2, title.comma.end-1)
# Finding unique values
unique(test$Title)

# See distribution of titles
options(digits=2)
require(Hmisc)
bystats(test$Age, test$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

# Missing titles are: Dr, Master, Miss, Mr, Mrs
## list of titles with missing Age value(s) requiring imputation
titles.na.test <- c("Dr", "Master", "Mrs", "Miss", "Mr")

imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

# Imputing values in the Age field
test$Age <- imputeMedian(test$Age, test$Title, 
                             titles.na.test)

# Imputing values for the Embarked field
test$Embarked[which(is.na(test$Embarked))] <- 'S'


# Finding Fare records 

subset(test, Fare < 7)[order(subset(test, Fare < 7)$Fare, 
                                 subset(test, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]


## impute missings on Fare feature with median fare by Pclass
test$Fare[ which(test$Fare == 0 )] <- NA
test$Fare <- imputeMedian(test$Fare, test$Pclass, 
                              as.numeric(levels(test$Pclass)))


#Plotting age by title
test$Title <- factor(test$Title,c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))

boxplot(test$Age ~ test$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")






## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
test$Title <- changeTitles(test, c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir"),"Noble")
test$Title <- changeTitles(test, c("the Countess", "Ms"), "Mrs")
test$Title <- changeTitles(test, c("Mlle", "Mme"), "Miss")
test$Title <- as.factor(test$Title)

require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

set.seed(23)
training.rows <- createDataPartition(train$Survived, 
                                     p = 0.8, list = FALSE)
train.batch <-train[training.rows, ]
test.batch <- train[-training.rows, ]

# Using logistic regression
Titanic.logit.1 <- glm(Survived ~ Sex + Pclass + Age + SibSp + Parch + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"))
Titanic.logit.1


1 - pchisq(315.1,df=9)

#Using anova test

anova(Titanic.logit.1, test="Chisq")

# Using glm
set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)



# Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


#Making a random forest model
measurements <- formula(Survived ~ Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Class,Fare)
model <- randomForest(measurements,data=train,importance=TRUE,ntree=2000)
model
varImpPlot(model)
predictions <- predict(model,test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)


# Making a neural net
neural_net <- neuralnet(formula = measurements, data = train, hidden = 3)


write.csv(submit, file = "firstforest.csv", row.names = FALSE)
forest_accuracy <- sum(test_lols$Survived == predictions)/nrow(test_lols)
table(pred = predictions, true = test_lols$Survived)




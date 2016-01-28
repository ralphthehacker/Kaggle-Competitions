prop.table(table(train$Survived,train$Sex),1)
train$Child <- NA
train$Child[train$Age<18]<-"Child"
train$Child[train$Age>=18]<-"Adult"

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
title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", train$Name, TRUE)
title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
train$Title <- substr(train$Name, title.dot.start+2, title.comma.end-1)
# Finding unique values
unique(train$Title)

# See distribution of titles
options(digits=2)
require(Hmisc)
bystats(train$Age, train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

# Missing titles are: Dr, Master, Miss, Mr, Mrs
## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

# Imputing values in the Age field
train$Age <- imputeMedian(train$Age, train$Title, 
                             titles.na.train)

# Imputing values for the Embarked field
train$Embarked[which(is.na(train$Embarked))] <- 'S'


# Finding Fare records 

subset(train, Fare < 7)[order(subset(train, Fare < 7)$Fare, 
                                 subset(train, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]


## impute missings on Fare feature with median fare by Pclass
train$Fare[ which(train$Fare == 0 )] <- NA
train$Fare <- imputeMedian(train$Fare, train$Pclass, 
                              as.numeric(levels(train$Pclass)))


#Plotting age by title
train$Title <- factor(train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))

boxplot(train$Age ~ train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")

#Making a random forest model
measurements <- formula(Survived ~ Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare)
model <- randomForest(measurements,data=train)
predictions <- predict(model,test_lols)
forest_accuracy <- sum(test_lols$Survived == predictions)/nrow(test_lols)
table(pred = predictions, true = test_lols$Survived)

## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
train$Title <- changeTitles(train, c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir"),"Noble")
train$Title <- changeTitles(train, c("the Countess", "Ms"), "Mrs")
train$Title <- changeTitles(train, c("Mlle", "Mme"), "Miss")
train$Title <- as.factor(train$Title)


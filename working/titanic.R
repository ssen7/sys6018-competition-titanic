library(tidyverse)
library(caret)

gender <- read_csv('../input/gender_submission.csv')
testing <- read_csv('../input/test.csv')
training <- read_csv('../input/train.csv')
head(training)
names(training)

# convert factor variables
training$Survived <- factor(training$Survived)
training$Pclass <- factor(training$Pclass)
training$Embarked <- factor(training$Embarked)
training$Sex <- factor(training$Sex)
# since PassengerId cannot be a factor in survival, we will drop that column
# for easier analysis

training$PassengerId <- NULL

head(training)

# starting with logistic regression
logit <- glm(Survived ~ . , data = modeltrainset, family = 'binomial')
# generates warning message

# finding the columns with most NA's
colSums(is.na(training))

# splitting into training and testing sets
set.seed(1345)
inTrain <- createDataPartition(y=training$Survived,
                               p=0.80, list=FALSE)
modeltrainset <- training[inTrain, ]
modeltestset <- training[-inTrain, ]

# removing Cabin from the model
logit2 <- glm(Survived ~ Pclass + Sex + Age + Embarked, data = modeltrainset, family = 'binomial')
summary(logit2)

pred <- predict(logit2, modeltestset)
pred[pred > 0.5 ] <- 1
pred[pred < 0.5 | is.na(pred) ] <- 0
pred <- as.factor(pred)
confusionMatrix(pred,modeltestset$Survived)$overall[1]



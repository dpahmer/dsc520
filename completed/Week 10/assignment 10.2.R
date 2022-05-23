# Logistic Regression
# Fit a Logistic Regression Model to Thoracic Surgery Binary Dataset
# For this problem, you will be working with the thoracic surgery data set from the University of California Irvine machine learning repository. This dataset contains information on life expectancy in lung cancer patients after surgery. The underlying thoracic surgery data is in ARFF format. This is a text-based format with information on each of the attributes. You can load this data using a package such as foreign or by cutting and pasting the data section into a CSV file.
# Assignment Instructions:
#   Fit a binary logistic regression model to the data set that predicts whether or not the patient survived for one year (the Risk1Y variable) after the surgery. Use the glm() function to perform the logistic regression. See Generalized Linear Models for an example. Include a summary using the summary() function in your results.
# According to the summary, which variables had the greatest effect on the survival rate?
#   To compute the accuracy of your model, use the dataset to predict the outcome variable. The percent of correct predictions is the accuracy of your model. What is the accuracy of your model?


library(caTools)
library(farff)
setwd("C:/users/pahme/onedrive/documents/github/dsc520")
surgery.full <- readARFF("./data/thoraricsurgery.arff")

str(surgery.full)

# Without understanding the various predictors we can't tell which value should be the baseline value...
# However, we can determine the baseline for the outcome variable, and we want to switch them.
str(surgery.full$Risk1Yr)
surgery.full$Risk1Yr <- relevel(surgery.full$Risk1Yr, "F")
str(surgery.full$Risk1Yr)

summary(surgery.full)

# Split into training dataset (surgery) and testing or validating dataset (surgery.test)
rnd <- sample.split(c(1:470), SplitRatio = .75)
surgery <- subset(surgery.full,rnd==TRUE)
surgery.test <- subset(surgery.full, rnd==FALSE)


model.1 <- glm(Risk1Yr ~ DGN , data = surgery, family = binomial())
summary(model.1)

model.2 <- glm(Risk1Yr ~ DGN + AGE , data = surgery, family = binomial())
summary(model.2)

model.3 <- glm(Risk1Yr ~ DGN + PRE4 + PRE5 , data = surgery, family = binomial())
summary(model.3)

model.all <- glm(Risk1Yr ~ DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 +
                   PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, data=surgery, family=binomial())
summary(model.all)

allpred <- predict(model.all, surgery, type="response")

(checkmodel <- table(actual=surgery$Risk1Yr, prediction= allpred > .5))

# Accuracy for the training set, sort of
((checkmodel[1,1]+checkmodel[2,2])/length(surgery[,1]))

allpred2 <- predict(model.all, surgery.test, type="response")

(checkmodel2 <- table(actual=surgery.test$Risk1Yr, prediction= allpred2 > .5))

# Accuracy for the training set, sort of
((checkmodel2[1,1]+checkmodel2[2,2])/length(surgery.test[,1]))


## Let's zero in on the best predictors!

model.opt1 <- glm(Risk1Yr ~ DGN + PRE9 + PRE14, data = surgery, family=binomial())
summary(model.opt1)

surgery$PRE9 <- relevel(surgery$PRE9, "F")

## Only the DGN5 matters, so let's extract that one; also OC14 in the PRE14
surgery$dgn5 <- c(surgery$DGN=="DGN5")
surgery$oc14 <- c(surgery$PRE14=="OC14")

model.opt2 <- glm(Risk1Yr ~ dgn5 + PRE9 + oc14, data=surgery, family=binomial())
summary(model.opt2)

surg.model <- data.frame(risk.predict=predict(model.opt2, 
                                              newdata=data.frame(dgn5=surgery$dgn5, 
                                                                 PRE9=surgery$PRE9, 
                                                                 oc14=surgery$oc14 ), type="response"))
# Alternate version of the same command
surg.model <- predict(model.opt2, surgery, type="response")
(opt2.conf <- table(actual=surgery$Risk1Yr, predicted=surg.model > .5))
((opt2.conf[1,1]+opt2.conf[2,2])/length(surgery[,1]))

## Now let's test this on the testing set:
surgery.test$PRE9 <- relevel(surgery.test$PRE9, "F")

## Only the DGN5 matters, so let's extract that one; also OC14 in the PRE14
surgery.test$dgn5 <- c(surgery.test$DGN=="DGN5")
surgery.test$oc14 <- c(surgery.test$PRE14=="OC14")

surg.model2 <- predict(model.opt2, surgery.test, type="response")
(opt2.conf <- table(actual=surgery.test$Risk1Yr, predicted=surg.model2 > .5))
((opt2.conf[1,1]+opt2.conf[2,2])/length(surgery.test[,1]))



## Part II


#   Fit a Logistic Regression Model
# Fit a logistic regression model to the binary-classifier-data.csv dataset
# The dataset (found in binary-classifier-data.csv) contains three variables; label, x, and y. The label variable is either 0 or 1 and is the output we want to predict using the x and y variables.
# What is the accuracy of the logistic regression classifier?

binclasdata <- read.csv("data/binary-classifier-data.csv")
str(binclasdata)
binclasdata$label <- as.factor(binclasdata$label)
str(binclasdata)

bcd.model1 <- glm(label~ x+y, data=binclasdata, family=binomial())
summary(bcd.model1)

bcd.model2 <- glm(label~y, data=binclasdata, family=binomial())
summary(bcd.model2)

# ok let's produce a training and testing set:
rnd <- sample.split(binclasdata[,1], SplitRatio = .75)
binclasdata.train <- subset(binclasdata, rnd==TRUE)
binclasdata.test <- subset(binclasdata, rnd==FALSE)

bcd.model2 <- glm(label~y, data=binclasdata.train, family=binomial())
summary(bcd.model2)


bcdpred <- predict(bcd.model2, binclasdata.train, type="response")
(model2.conf <- table(actual=binclasdata.train$label, predicted=bcdpred >= .5))

# Check accuracy of the model on the training set
((model2.conf[1,1]+model2.conf[2,2])/sum(model2.conf))


# it's pretty lousy. Let's check the testing set:
bcdpred <- predict(bcd.model2, binclasdata.test, type="response")
(model2.conf <- table(actual=binclasdata.test$label, predicted=bcdpred >= .5))

# Check accuracy on the testing set
((model2.conf[1,1]+model2.conf[2,2])/sum(model2.conf))


## What about the interaction between x and y?
bcd.model3 <- glm(label~y*x, data=binclasdata.train, family=binomial())
summary(bcd.model3)


bcdpred3 <- predict(bcd.model3, binclasdata.train, type="response")
(model3.conf <- table(actual=binclasdata.train$label, predicted=bcdpred3 >= .5))

# Check accuracy of the model on the training set
((model3.conf[1,1]+model3.conf[2,2])/sum(model3.conf))


# it's pretty lousy. Let's check the testing set:
bcdpred3 <- predict(bcd.model3, binclasdata.test, type="response")
(model3.conf <- table(actual=binclasdata.test$label, predicted=bcdpred3 >= .5))

# Check accuracy on the testing set
((model3.conf[1,1]+model3.conf[2,2])/sum(model3.conf))



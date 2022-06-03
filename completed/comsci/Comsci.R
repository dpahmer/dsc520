# DSC 520 Final project

setwd ("C:/Users/pahme/OneDrive/Documents")
library(readxl)
library(dplyr)
library(caTools)
library(useful)
library(ggplot2)
library(class)

lgpa <- read_excel("lgpa.xlsx")
grad.bach <- read_excel(("grad_bach.xlsx"))
hs_gpa <- read_excel("hs gpa.xlsx")
tgpa <- read_excel("tgpa.xlsx")
ftoc <- read_excel("ftoc.xlsx")
sat <- read_excel("sat act.xlsx")
ap <- read_excel("ap exam.xlsx")
firstcom <- read_excel("first com course.xlsx")
major <- read_excel("declared com major.xlsx")

grad.bach$inc <- (grad.bach$majr_code_1=="COM" | grad.bach$majr_code_1_2=="COM")
str (firstcom)
firstcom <- firstcom[,1:2]
# because we don't need the title.

str(ftoc)
profile <- subset(ftoc,term_code_ftoc > 201009 & term_code_ftoc <201809, select=c(pidm, term_code_ftoc))

# make sure pidm and term_code are unique!
tgpa <- tgpa[!duplicated(tgpa[,c("pidm","term_code")]),]
names(tgpa)[names(tgpa)=='term_code'] <- 'term_gpa'


# same for AP
ap <- ap[!duplicated(ap$pidm),]

#same for the lgpa
lgpa.u <- lgpa[do.call(order,lgpa),c("pidm","levl_code","lgpa")]
lgpa.u <- lgpa.u[!duplicated(lgpa.u$pidm),c("pidm","lgpa")]

## After checking that all the other tables are unique students, we can join them all

profile <- profile %>% left_join(major, by='pidm')
names(profile)[names(profile)=='term_code'] <- 'term_major'

names(firstcom)[names(firstcom)=='term_code'] <- 'term_com'

profile <- profile %>% left_join(firstcom, by='pidm')

profile <- profile %>% left_join(ap, by='pidm')
# convert this to T/F
profile$subj_code <- as.logical(!is.na(profile$subj_code))

profile <- profile %>% left_join(grad.bach, by='pidm')

profile <- profile %>% left_join(hs_gpa, by='pidm')

# It looks like gpa is a char; change to number
profile$gpa <- as.numeric(profile$gpa)

profile <- profile %>% left_join(lgpa.u, by='pidm')
profile <- profile %>% left_join(sat, by='pidm')

profile <- profile %>% left_join(tgpa, by='pidm')
# Only keep the records that the term gpa is same as the ftoc term
profile <- profile[profile$term_gpa==profile$term_code_ftoc,]

# Now let's filter down to the eligible population:

# first com course within two years of starting
interval <- profile$term_com - profile$term_code_ftoc

eligible <- interval<=200

# Declared COM major
eligible2 <- eligible==TRUE | !is.na(profile$term_major)

profile <- profile[which(eligible2==TRUE),]



# define success as graduated in COM with GPA 3.0 or better
success <- profile$inc==TRUE & profile$lgpa >=3
success[is.na(success)] <- FALSE

profile$success <- success==TRUE


profile.train <- profile[which(profile$term_code_ftoc<= 201509),]
profile.test <- profile[which(profile$term_code_ftoc > 201509),]



## Now the work: analysis

# First model- logistic regression with many variables:
model.1 <- glm(success ~ subj_code + gpa + tgpa + sat_math + act_math + sat_verbal , data = profile.train, 
               family = binomial())
summary(model.1)

# ok, nothing significant!

# Second model- logistic regression without the ACT, because too many missing values:
model.2 <- glm(success ~ subj_code + gpa + tgpa + sat_math +  sat_verbal , data = profile.train, 
               family = binomial())
summary(model.2)

# maybe something- if we focus on the SAT math score

model.3 <- glm(success ~ sat_math, data=profile.train, family=binomial())
summary(model.3)

pred.m3 <- predict(model.3, profile.test, type="response")

(checkmodel3 <- table(actual=profile.test$success, prediction= pred.m3 > .1))
# if we set the cutoff at .5 like one would expect- we get the prediction that all fail.

# Accuracy for the training set, sort of
((checkmodel3[1,1]+checkmodel3[2,2])/length(profile.test[,1]))


# Let's plot this logistic regression curve:

ggplot(profile.test, aes(x=sat_math, y=pred.m3)) + geom_point() +
  stat_smooth(method="glm",  method.args=list(family=binomial()))

# We see that everyone has less than 50% expectation of success, for some reason.


# let's try adding also AP credit to this:

model.4 <- glm(success ~ sat_math + subj_code, data=profile.train, family=binomial())
summary(model.4)

pred.m4 <- predict(model.4, profile.test, type="response")

(checkmodel4 <- table(actual=profile.test$success, prediction= pred.m4 > .1))

# Accuracy for the training set, sort of
((checkmodel4[1,1]+checkmodel4[2,2])/length(profile.test[,1]))

# So it didn't add anything.


model.5 <- glm(success ~ sat_math + tgpa, data=profile.train, family=binomial())
summary(model.5)

pred.m5 <- predict(model.5, profile.test, type="response")

(checkmodel5 <- table(actual=profile.test$success, prediction= pred.m5 > .1))

# Accuracy for the training set, sort of
((checkmodel5[1,1]+checkmodel5[2,2])/length(profile.test[,1]))


# Let's plot this logistic regression curve:

ggplot(profile.test, aes(x=sat_math, y=pred.m5)) + geom_point() +
  stat_smooth(method="glm",  method.args=list(family=binomial()))



# Let's try it with an interaction:

model.5 <- glm(success ~ sat_math * tgpa, data=profile.train, family=binomial())
summary(model.5)

pred.m5 <- predict(model.5, profile.test, type="response")

(checkmodel5 <- table(actual=profile.test$success, prediction= pred.m5 > .1))

# Accuracy for the training set, sort of
((checkmodel5[1,1]+checkmodel5[2,2])/length(profile.test[,1]))


# Let's plot this logistic regression curve:

# For the SAT cutoff:
ggplot(profile.test, aes(x=sat_math, y=pred.m5)) + geom_point() +
  stat_smooth(method="glm",  method.args=list(family=binomial()))

# For the first-term GPA cutoff:
ggplot(profile.test, aes(x=tgpa, y=pred.m5)) + geom_point() +
  stat_smooth(method="glm",  method.args=list(family=binomial()))


# So- so far we are seeing something promising with 
#   math SAT scores above 650 and possibly also the first term GPA above 3.3
 


# Miscellaneous tests

with (profile.train, table(subj_code,success))
with (profile.train, mean(subj_code==success))
# Interesting- about 69% success, but not enough maybe to be useful.

with (profile.train, table(round(gpa),success))
# Not clearly useful

with (profile.train, table(act_math,success))
# so not enough people took ACT

with (profile.train, table(round(tgpa,2),success))
# maybe interesting, as above


# Let's see if the combination of sat_math and first term gpa can work better
model.4 <- glm(success ~ sat_math + round(tgpa,1), data=profile.train, family=binomial())
summary(model.4)

# So that wasn't any better ostensibly than the sat math alone. Let's try a cluster analysis

# seems like we need to eliminate the NA records.
p2.train <- profile.train[!is.na(profile.train$sat_math) & !is.na(profile.train$tgpa),c("sat_math","tgpa","success")]

p2.test <- profile.test[!is.na(profile.test$sat_math) & !is.na(profile.test$tgpa),c("sat_math","tgpa","success")]


p.pred <- knn(p2.train[1:2]  ,p2.test[1:2]  , k=1, cl=p2.train$success)
(acctable <- table(p.pred, p2.test$success))

mean(p.pred==p2.test$success)


# looks like about 60% accuracy so far. Let's try larger k
p.pred <- knn(p2.train[1:2]  ,p2.test[1:2]  , k=4, cl=p2.train$success)
(acctable <- table(p.pred, p2.test$success))

mean(p.pred==p2.test$success)

# Although this looks like an improvement, it's not, since we 
# predicted only 5 people would be successful, and of those only one was; while
# of all the others whom we predicted would not be successful- about a third were.




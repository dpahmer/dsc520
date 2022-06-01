# Assignment 11.2
# David Pahmer
# Weeks 11 & 12
# Due June 3 2022


# Introduction to Machine Learning

# Include all of your answers in a R Markdown report. 

# In this problem, you will use the nearest neighbors algorithm to fit a model on two simplified datasets. The first dataset (found in binary-classifier-data.csv) contains three variables; label, x, and y. The label variable is either 0 or 1 and is the output we want to predict using the x and y variables (You worked with this dataset last week!). The second dataset (found in trinary-classifier-data.csv) is similar to the first dataset except that the label variable can be 0, 1, or 2.

library(useful)
library(ggplot2)



setwd("C:/users/pahme/onedrive/documents/github/dsc520/data")
binclasdata <- read.csv("binary-classifier-data.csv")
binclasdata$label <- as.factor((binclasdata$label))
bdiv <- sample.split(binclasdata[,1],SplitRatio = .75)
btrain <- binclasdata[bdiv,]
btest <- binclasdata[!bdiv,]




triclasdata <- read.csv("trinary-classifier-data.csv")
triclasdata$label <- as.factor(triclasdata$label)
tdiv <- sample.split(triclasdata[,1], SplitRatio = .75)
ttrain <- triclasdata[tdiv,]
ttest <- triclasdata[!tdiv,]


# Plot the data from each dataset using a scatter plot.
(bscatter <- ggplot(data=binclasdata, aes(x=x, y=y, color=label)) + geom_point())

(tscatter <- ggplot(data=triclasdata, aes(x=x, y=y, color=label)) + geom_point())


# The k nearest neighbors algorithm categorizes an input value by looking at the labels for the k nearest points and assigning a category based on the most common label. In this problem, you will determine which points are nearest by calculating the Euclidean distance between two points. 







#   Fitting a model is when you use the input data to create a predictive model. There are various metrics you can use to determine how well your model fits the data. For this problem, you will focus on a single metric, accuracy. Accuracy is simply the percentage of how often the model predicts the correct result. If the model always predicts the correct result, it is 100% accurate. If the model always predicts the incorrect result, it is 0% accurate.
# Fit a k nearest neighbors’ model for each dataset for k=3, k=5, k=10, k=15, k=20, and k=25. Compute the accuracy of the resulting models for each value of k. Plot the results in a graph where the x-axis is the different values of k and the y-axis is the accuracy of the model.


# I think no need to scale in this case
# btrain_scale <- scale(btrain[, 2:3])

bpred <- knn(btrain[2:3], btest[2:3], k=1, cl=btrain$label)
(acctable <- table(bpred, btest$label))
mean(bpred==btest$label)






# Let's try various k values, from 1 to 30 let's say.

accuracytable <- c()
kopts <- c(1:30)

for (kval in kopts){
  bpredx <- knn(btrain[2:3], btest[2:3], k=kval, cl=btrain$label)
  accuracytable[kval] <- round(mean(bpredx== btest$label)*100,2)
}
accuracytable

plot(accuracytable)



# Now let's do the same for the triclass dataset:
accuracytable <- c()
kopts <- c(1:30)

for (kval in kopts){
  tpredx <- knn(ttrain[2:3], ttest[2:3], k=kval, cl=ttrain$label)
  accuracytable[kval] <- round(mean(tpredx== ttest$label)*100,2)
}
accuracytable

plot(accuracytable)






# decision boundary
# Looking back at the plots of the data, do you think a linear classifier would work well on these datasets?
#   How does the accuracy of your logistic regression classifier from last week compare?  Why is the accuracy different between these two methods?






set.seed(43)
(bk <- kmeans(x=binclasdata, centers=2, nstart = 10))
plot(bk, data=binclasdata)

plot(bk, data=binclasdata, class="label")


bkn <- kmeans(x=binclasdata, centers=25, nstart = 10)

plot(bkn, data=binclasdata)

plot(bkn, data=binclasdata, class="label")



#   Clustering
# Labeled data is not always available. For these types of datasets, you can use unsupervised algorithms to extract structure. The k-means clustering algorithm and the k nearest neighbor algorithm both use the Euclidean distance between points to group data points. The difference is the k-means clustering algorithm does not use labeled data.

# In this problem, you will use the k-means clustering algorithm to look for patterns in an unlabeled dataset. The dataset for this problem is found at data/clustering-data.csv.

clusdata <- read.csv("clustering-data.csv")
ggplot(data=clusdata, aes(x=x, y=y)) + geom_point()
plot(clusdata)

# Plot the dataset using a scatter plot.


# Fit the dataset using the k-means algorithm from k=2 to k=12. Create a scatter plot of the resultant clusters for each value of k.



(clus.k <- kmeans(x=clusdata, centers=10, nstart=10))
plot(clus.k, data=clusdata)


sumofwss <- c()
meanwss <- c()
acc <- c()

kopts <- c(1:20)

for (kval in kopts){
  (clus.k <- kmeans(x=clusdata, centers=kval, nstart=10))
  sumofwss[kval] <- round(sum(clus.k$withinss))
  meanwss[kval] <- round(mean(clus.k$withinss))
  acc[kval] <- round(sum(clus.k$totss)/sum(clus.k$betweenss),2)
}

plot(sumofwss, type="b")
plot(meanwss, type="b")
plot(acc, type="b")



# As k-means is an unsupervised algorithm, you cannot compute the accuracy as there are no correct values to compare the output to. Instead, you will use the average distance from the center of each cluster as a measure of how well the model fits the data. To calculate this metric, simply compute the distance of each data point to the center of the cluster it is assigned to and take the average value of all of those distances.


# Calculate this average distance from the center of each cluster for each value of k and plot it as a line chart where k is the x-axis and the average distance is the y-axis.


# One way of determining the “right” number of clusters is to look at the graph of k versus average distance and finding the “elbow point”. Looking at the graph you generated in the previous example, what is the elbow point for this dataset?
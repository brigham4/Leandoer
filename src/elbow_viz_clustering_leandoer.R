library(readr)
library(cluster)
library(tidyverse)
library(factoextra)
shoppingdata <- read_csv("/Users/ericbrigham/Desktop/datamining/project/complete.csv")
View(shoppingdata)
lean_albums <- c("Crash Bandicoot & Ghostface / Shyguy", "Frost God", "Lavender EP", 
                 "Neal Yung 2003", "Poison Ivy", "Profound Sadness 2004", "Spider Feet - Single",
                 "Stranger", "Unknown Death 2002", "Unknown Memory", "Warlord")

thedata  = shoppingdata
thedata = as.data.frame(unclass(thedata))
summary(thedata)
dim(thedata)
cleandata = na.omit(thedata)
dim(cleandata)
summary(cleandata)
rownames(cleandata) <- cleandata$Album
cleandata$X1 <- NULL
only_yung <- subset(cleandata, Album %in% lean_albums)
only_yung$Album <- NULL
cleandata$Album <- NULL
only_yung$negative <- NULL
cleandata$negative <- NULL
only_yung$positive <- NULL
cleandata$positive <- NULL
only_yung$Artist <- NULL
cleandata$Artist <- NULL

#scaled_data = as.matrix(scale(cleandata))

##Elbow Method
set.seed(123)
#Compute and plot WSS for k=2 to k=15
k.max <- 15
data <- cleandata
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart = 50, iter.max = 15)$tot.withinss}) 
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clusGap(data, pam, k.max, B = 100, d.power = 1,
        spaceH0 = c("scaledPCA", "original"),
        verbose = interactive())

##Source code for k-means clustering
km <- kmeans(cleandata, centers = 4)
fviz_cluster(km, data = cleandata, labelsize = 8) 

##Source code for k-medoids clustering
clusplot(pam(cleandata, 5), main = "Clustering with K-Medoids")

##Source code for graphing unclustered data
plot(x = cleandata$Annual.Income..k.., y = cleandata$Spending.Score..1.100., xlab = "Annual Income in Thousands of $", ylab = "Spending Score")

##Source code for dendrogram
dd <- dist(scale(cleandata), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, labels = NULL, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")

plot(hc, hang = -1, cex = 0.6)
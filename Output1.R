

library(ggplot2)
library(corrplot)
library(RColorBrewer)
library( tmap )
library(sf)
library(psycho) #Standardize / Normalize / Z-score / Scale
library(tidyverse)
library(cluster)
library(GGally)
library(plotly)
library(reshape2)

setwd("C:\R")
data1<- read.csv("Leeds_Variables.csv")
names(data1)


#--------------- select k-means variables------------#

data2<- data1[,c("OA","k030","k037","k038", "k042", "k044", "k068" ,"k069")]
head(data2)





corr<-cor(data2[,-1])
corrplot(corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))




#------------ data standarization----------------#
data2.zd<-data.frame(scale(data2[,-1]))



## histograms


p1=ggplot(data2.zd, aes(x=k030)) + geom_histogram(binwidth=.1)
p2=ggplot(data2.zd, aes(x=k037)) + geom_histogram(binwidth=.1)
p3=ggplot(data2.zd, aes(x=k038)) + geom_histogram(binwidth=.1)
p4=ggplot(data2.zd, aes(x=k042)) + geom_histogram(binwidth=.1)
p5=ggplot(data2.zd, aes(x=k044)) + geom_histogram(binwidth=.1)
p6=ggplot(data2.zd, aes(x=k068)) + geom_histogram(binwidth=.1)
p7=ggplot(data2.zd, aes(x=k069)) + geom_histogram(binwidth=.1)





#----------- k means-------------#
res.kmeans <- lapply(1:10, function(i) {
  kmeans(data2.zd, centers = i)
})

## SS for each cluster (1 cluster to 10 clusters)
lapply(res.kmeans, function(x) x$withinss)


## Sum up SS
res.within.ss <- sapply(res.kmeans, function(x) sum(x$withinss))

plot(1:10, res.within.ss, type = "b", xlab = "Number of clusters", ylab = "Within SS")


ggplot(data.frame(cluster = 1:10, within.ss = res.within.ss), aes(cluster, within.ss)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = 0:10)


set.seed(2222)
pc_cluster_4<-kmeans(data2.zd, centers = 4)
pc_cluster_4$cluster
pc_cluster_4$centers
pc_cluster_4$size



data2.zd$cluster <- as.factor(pc_cluster_4$cluster)
p <- ggparcoord(data = data2.zd, columns = c(1:7), groupColumn = "cluster", scale = "std") + labs(x = "variables", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)




centers<-data.frame(cluster=1:4,pc_cluster_4$centers)
# collapse the data frame
centers<-melt(data = centers, id.vars = "cluster")
centers$cluster<- as.character(centers$cluster)
ggplot(data=centers, aes(x=variable, y=value, fill=cluster) ) + 
  geom_bar(stat="identity", 
           position=position_dodge())










#-----------ANOVA --------------##
res.anova <- lapply(1:7, function(i) {
  aov(data2.zd[,i] ~ cluster, data = data2.zd)
})

for(i in 1:7){
print(summary(res.anova[[i]])) }






#the 'distance' between each OA and the 'cluster centre' of 
#the cluster that it falls within
dist<- sqrt(rowSums(data2.zd[,1:7] - fitted(pc_cluster_4)) ^ 2)
data2.zd$dist<-dist
ggplot(data2.zd, aes(x=cluster, y=dist, color=cluster)) +
  geom_boxplot()



data.clust<-data.frame(code=data1$OA, cluster=pc_cluster_4$cluster)
write.csv(data.clust, "FinalClassification.csv")
LeedsOA<- read_sf("LeedsOA.shp")
head(LeedsOA)
names(LeedsOA)

leed.dat<-merge(LeedsOA, data.clust,by="code")
names(leed.dat)


tm_shape(leed.dat) +
  tm_fill(col="cluster", style="cont", size=0.2, id="code", palette="Blues", title="") +
  tm_borders(col="#bdbdbd", lwd=0.5) +
  tm_layout(
    title=" OA Clusters",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)

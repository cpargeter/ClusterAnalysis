#Cluster Analysis Example

#Libraries
library(fpc)
library(ggplot2)

#Example Data
df.c<-data.frame(Level = rep(c("A","B","C"),each = 30),
  Rock = rep(c("Rock1","Rock2","Rock3","Rock4","Rock5"),18),
  L.tech = rnorm(90,mean=5), W.mid = rnorm(90))

# Question
#Is there a break in the data sets that would separate the data by technical length and width 
#at mid point.  Show the data in terms of Rock type and/or Level
#Note:  Some code based off work from StackOverFlow - 
#http://stackoverflow.com/questions/15376075/
#cluster-analysis-in-r-determine-the-optimal-number-of-clusters

## Methodology
#Determine the number of clusters - plots the Sum of Square Erros Scree Plot for 1 to 20 clusters.  
#Change the maximum number of cluster depending on your data, or if the original plot is not 
#showing sufficient convergence towards minimal SSE

#Note, in this example, the clusters are based on columns 3 and 4
wss <- (nrow(df.c[,3:4])-1)*sum(apply(df.c[,3:4],2,var))
    for (i in 2:20) wss[i] <- sum(kmeans(df.c[,3:4],centers=i)$withinss)
    plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Partion around medoids to estimate the best number of clusters
        pamk.best <- pamk(df.c[,3:4])
        cat("Optimimum number of clusters:", pamk.best$nc, "\n")

#Plot Clusters - you can either use the number that Partioning returned (pamk.best$nc) 
#or enter the amount based on your visual inspection of the SSE plot.  
#The plot below shows the data separated by Level, using shape = Level, 
#and gives the color based on the assigned cluster.

n <- pamk.best$nc
cl <- kmeans(df.c[,c(3:4)], n)
p<- ggplot(data = df.c,aes(x = L.tech, y= W.mid))         
p1<- p+ geom_point(aes(shape = Level, col= factor(cl$cluster), size=3)) + 
        scale_shape(solid = FALSE) +
        scale_colour_manual(values = rainbow(n))
p1
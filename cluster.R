#Cluster Analysis Example with quantiles

#Libraries
library(fpc)
library(ggplot2)
library(dplyr)
library(broom)

#Create example Data
df.cluster<-data.frame(Level = rep(c("A","B","C"),each = 300),
  Rock = rep(c("Rock1","Rock2","Rock3","Rock4","Rock5"),180),
  Length = rnorm(900,mean=6,sd=1), Width = rnorm(900,mean=3,sd=0.5))

df.cluster=rbind(data.frame(Level="A",
                            Rock = rep(c("Rock1","Rock2","Rock3","Rock4","Rock5"),60),
                            Length=rnorm(300,mean=20,sd=2), 
                            Width = rnorm(300,mean=10,sd=0.5)),
                 data.frame(Level="B",
                            Rock = rep(c("Rock1","Rock2","Rock3","Rock4","Rock5"),60),
                            Length=rnorm(300,mean=15,sd=1), 
                            Width = rnorm(300,mean=8,sd=1)),
                 data.frame(Level="C",
                            Rock = rep(c("Rock1","Rock2","Rock3","Rock4","Rock5"),60),
                            Length=rnorm(300,mean=11,sd=3), 
                            Width = rnorm(300,mean=5,sd=0.5)))

summary(df.cluster)

# Question
#Is there a break in the data sets that would separate the data by length and width. 
#Show the data in terms of Rock type and/or Level
#Note:  Some code based off work from StackOverFlow - 
#http://stackoverflow.com/questions/15376075/
#cluster-analysis-in-r-determine-the-optimal-number-of-clusters

## Methodology for visualizing the clusters

#Determine the number of clusters - plots the Sum of Square Errors Scree Plot for 1 to 20 clusters.  
#Change the maximum number of cluster depending on your data, or if the original plot is not 
#showing sufficient convergence towards minimal SSE

#Note, in this example, the clusters are based on c('Length','Width')
wss <- (nrow(df.cluster[,c('Length','Width')])-1)*sum(apply(df.cluster[,c('Length','Width')],2,var))
    for (i in 2:20) wss[i] <- sum(kmeans(df.cluster[,c('Length','Width')],centers=i)$withinss)
    plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Partion around medoids to estimate the best number of clusters
        pamk.best <- pamk(df.cluster[,c('Length','Width')])
        cat("Optimimum number of clusters:", pamk.best$nc, "\n")


#Method for adding cluster identifications back to the original dataset

cluster.function<-function(x,colNames, n){    
  cl <- kmeans(x[,colNames], n)           #change here to length, width
  x$cluster <- factor(cl$cluster)
  return(x)
}

data_cluster<-cluster.function(df.cluster,c('Length','Width'),pamk.best$nc)

#Plot Clusters - you can either use the number that Partioning returned (pamk.best$nc) 
#or enter the amount based on your visual inspection of the SSE plot.  
#The plot below shows the data separated by Level, using shape = Level, 
#and gives the color based on the assigned cluster.


p<- ggplot(data = data_cluster,aes(x = Length, y= Width))
p1<- p+ geom_point(aes(shape = Level, col= factor(cluster))) + 
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = rainbow(6)) + 
  guides(col=guide_legend(title="Clusters"))

p1

#Method to check size boundaries between clusters using quantiles

quant.funct<-function(df, col){
  qt<-df %>%
    group_by(cluster) %>%
    do(tidy(t(quantile(.[[col]],c(0,0.01,0.1,0.5,0.9,0.99,1)))))
  colnames(qt) = c('cluster','0%','1%','10%','50%','90%','99%','100%')
  return(qt)
}

  quant.funct(data_cluster, "Length")
  quant.funct(data_cluster, 'Width')


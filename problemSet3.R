#                  ## Problem Set 3 ##

##############################################
##############################################

## Exercise 1

rm(list=ls())

## swiss data
s <- swiss 
s
attach(s)  # valutare se fare l'attach di questo o dello standardizzato

#PRELIMINARY ANALYSIS
dim(s)
summary(s)
head(s)

'We’ll use the swiss data as data set. 
We start by loading the dataset and standardizing the data to make variables comparable.'
df <- scale(s[1:6])
df <-df[, 2:6] #take only the 5 indicators
head(df)

pairs(df, gap=0, pch=16) #graphical representation
'We can see, from the graphical pairs representation that the data presents a grouping propensity,
conversely to a random dataset generated from the swiss dataset, presented below'
random_df <- apply(df, 2,
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
scaled.random_df = scale(random_df)
pairs(scaled.random_df, gap = 0, pch = 16)

library("factoextra")

# Plot the standardized df data 
fviz_pca_ind(prcomp(df), title = "PCA - Swiss dataset",
             palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# Plot the random df data
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())

'It is not clear how many clusters contain the real data; 
Instead, for sure it can be seen, in the PC space too, that the standardized randomly generated uniform data 
are more "isolated".'


'For evaluating the clustering tendency, we can use a statistical method called Hopkins statistic, that takes values in [0,1].
The value H of this method close to 0 indicates clustered data; H close to 0.5 indicates uniformly distributed data
(no meaningful clusters).'
library(clustertend)
# Compute Hopkins statistic for the iris dataset
set.seed(123)
hopkins(df, n = nrow(df)-1)
# Compute Hopkins statistic for a random dataset
set.seed(123)
hopkins(random_df, n = nrow(random_df)-1)
'It can be seen that the swiss data set (df) is clusterable because his H value (0.3165011) is close enough to 0. 
However, the random_df dataset is not clusterable (H = 0.5237217).
'

'There is also a visual method for confirming the cluster tendency of a dataset, 
and it is the visual assessment of cluster tendency (VAT). It computes the dissimilarity matrix (DM) 
between the units in the dataset using the Euclidean distance, and it reorders it (the DM) 
so that similar units are close to one another. 
This process creates an ordered dissimilarity matrix (ODM) that is displayed as an ordered dissimilarity image (ODI),
which is the visual output of the VAT algorithm. For the visual assessment of clustering tendency, 
we start by computing the dissimilarity matrix between observations using the function dist() 
and then using the function fviz_dist() to display the dissimilarity matrix.'
## VAT algorithm
fviz_dist(dist(df), show_labels = FALSE)+
  labs(title = "Swiss data")

fviz_dist(dist(random_df), show_labels = FALSE)+
  labs(title = "Random data")

'The color level is proportional to the value of the dissimilarity between observations: 
red denotes high similarity (i.e. low dissimilarity); blue denotes low similarity (i.e. high dissimilarity). 
The dissimilarity matrix image confirms that there is a cluster structure in the standardized swiss dataset but not in the random one.
The VAT algorithm detects the clustering tendency in a visual form by counting the number of a square shaped 
red blocks along the diagonal in a VAT image.
'

#POINT 1
#TEXT: 
'Obtain a 3 clusters solution via hierarchical clustering with Ward’s method. 
Plot the observations in the space of the first two principal components with colors determined by cluster memberships.
Add the projected cluster centroids. Give an interpretation to the clusters. What is the cluster assigned to province “ V. De Geneve ”?'

'The first clustering algorithm applied is the agglomerative hierarchical clustering, that is a “bottom-up” approach: 
each observation starts in its own cluster (leaf), and pairs of cluster are merged as one moves up the hierarchy. 
This process goes on until there is just one single big cluster (root). In this case the leaves are 47, and for every step, 
the algorithm merges pairs of cluster with the lowest dissimilarity according to a different given linkage method,
and then it moves up the hierarchy, until there is just one single big cluster, with all the observations within,
by building a sort of tree diagram called dendrogram. We use the Ward (minimum deviance) linkage method.'

'Ward’s linkage methods assume that a cluster is represented by its centroid (cluster center). 
So it doesn’t use a unit, but a point, and for this reason it is feasible for numeric dataset only;
Ward’s method minimizes iteratively the sum of the squared Euclidean distances of units within a cluster
for every cluster (WSS – Within deviance), and maximizes the sum of the squared average distances of units
between centroids (BSS – Between deviance). 
So, the Ward’s method accepts as input only a Euclidean distance matrix'

res.dist = dist(df, method = "euclidean")
res.wardd <- hclust(d = res.dist, method = "ward.D")
fviz_dend(res.wardd, cex = 0.5, main = "Ward D linkage method and euclidean distance")
cor(res.dist, cophenetic(res.wardd))

res.dist = dist(df, method = "euclidean")
res.wardd2 <- hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.wardd2, cex = 0.5, main = "Ward D2 linkage method and euclidean distance")
cor(res.dist, cophenetic(res.wardd2))

'The difference between the methods Ward.D and Ward.D2 is the distance matrix to be given as an input to hclust().
In Ward.D it is squared, in Ward.D2 it is not squared.'

'The cophenetic dissimilarity/distance of two units is a measure of how similar those two units have to be
in order to be grouped into the same cluster. From a practical point of view, 
the cophenetic distance between two units is the height of the dendrogram where the two branches that include the two units
merge into a single branch (height of the fusion).'
library(cluster)
fviz_dend(hclust(daisy(s[, 2:6]), method="ward.D"),cex=0.5, main="Ward linkage method and Gower distance")
cor(cophenetic(hclust(daisy(s[, 2:6]),method="ward.D")), daisy(s))
fviz_dend(hclust(daisy(s[, 2:6]), method="ward.D2"),cex=0.5, main="Ward D2 linkage method and Gower distance")
cor(cophenetic(hclust(daisy(s[, 2:6]),method="ward.D2")), daisy(s))
'One way to measure how well the cluster tree generated by the hclust() function reflects our data is to compute the 
correlation between the cophenetic distances and the original distances generated by the dist() function. 
If the clustering is valid, the linking of units in the cluster tree should have a strong correlation with the distances
between units in the original distance matrix.
The closer the value of the correlation coefficient is to 1, the more accurately the clustering solution reflects our data.
Values above 0.75 are felt to be good.
In this case the Ward D2 linkage method and the Gower’s distance produces the highest value of this statistic: 0.911548.'

res.dist = dist(df, method = "euclidean")
hc <- hclust(d=res.dist, method = "ward.D2")
plot(hc)
rect.hclust(hc, k=3, border="red")
hc_labels<-cutree(hc,k=3); 
hc_labels
table(hc_labels)

# grafici che confrontano le densità delle variabili nei cluster (manca legend)
# mettere il titolo della singola variabile
# modificando i limiti dei plot si possono adattare a tutte le variabili
plot(density(Fertility[hc_labels==1]),xlim=range(25,105),ylim=range(0,0.06),col="blue")
par(new=TRUE)
plot(density(Fertility[hc_labels==2]),,xlim=range(25,105),ylim=range(0,0.06),col="red")
par(new=TRUE)
plot(density(Fertility[hc_labels==3]),,xlim=range(25,105),ylim=range(0,0.06),col="green")

plot(density(Agriculture[hc_labels==1]),xlim=range(15,105),ylim=range(0,0.06),col="blue")
par(new=TRUE)
plot(density(Fertility[hc_labels==2]),,xlim=range(15,105),ylim=range(0,0.06),col="red")
par(new=TRUE)
plot(density(Fertility[hc_labels==3]),,xlim=range(15,105),ylim=range(0,0.06),col="green")
'INTEPRETAZIONE:
Mandato screen con tutti i cantoni segnati'

'Now that I know that Ward D2 is the best method I can Plot the observations in the space of the first
two principal components with colors determined by cluster memberships.'
df_pca<- prcomp(df)
summary(df_pca)
round(df_pca$rotation[,1:2],3)
#The first two components account for about 73% of the data variability
df_pca$rotation[,1:2]
aggregate(df,by=list(hc_labels),FUN=mean)[,-1]
centroids<-aggregate(df_pca$x[,1:2],by=list(hc_labels),FUN=mean)[,-1]

plot(df_pca$x[,1],df_pca$x[,2],type = "n",asp=1,   
     xlab = "PC1", ylab = "PC2", main="Swiss data, K=3")            
text(df_pca$x[,1],df_pca$x[,2],labels=hc_labels, 
     col = hc_labels) 
points(centroids,cex=1.5) # aggiungere cose grafiche 

axis(2)
axis(1,at=1:5,las=2,cex.axis=0.7,
     labels=names(s[, 2:6]))
box()
legend("bottomright",paste("Cluster",1:3),
       lty=c(1,1,1),col=c(1:3),bty="n")



#POINT 2
'Find a 3 clusters solution with k-means using the centroids found at previous point
as initial choice of seeds. Plot the observations in the space of the first two principal 
components as done in the previous point and compare the k-means cluster solution
with the hierarchical one.'
centroids<-aggregate(df,by=list(hc_labels),FUN=mean)[,-1]

km_swiss<-kmeans(df, iter.max=100, algorithm = "MacQueen",
                   centers= centroids)
km_label3<-km_swiss$cluster
WSS3<-round(km_swiss$tot.withinss,2); WSS3

k<-3
n<-nrow(swiss)
dd<-as.matrix(dist(rbind(km_swiss$centers,df)))[1:k,(k+1):(k+n)]
ind<-cbind(km_swiss$cluster,1:n)

out<-cbind(km_swiss$cluster,dd[ind])
colnames(out)<-c("cluster","dist_centr")
round(out[1:6,],2)

plot(PC2~PC1, data=df_pca$x,pch=16,asp=1, cex=0.8,xlab = "PC1", ylab = "PC2",main=paste("Swiss data, WSS=",WSS3, sep=""),col=km_swiss$cluster)            
text(PC2~PC1, data=df_pca$x, labels=km_label3,pos=4,cex=0.8,offset=0.1,col=km_swiss$cluster) 
legend("topleft",paste("clust",unique(km_swiss$cluster)),
       lty=rep(1,k),col=unique(km_swiss$cluster),bty="n",cex=0.8)

km_label3 == hc_labels # tutti true tranne Boudry
'THE 2 CLUSTERS (WARD AND KMEANS) PRESENT EXACTLY THE SAME RESULTS EXCEPT FOR THE OBSERVATION IN THE PERFECT MIDDLE:
WITH WARD IT IS CLUSTERED AS CLUSTER 3, WHILE WITH KMEANS IS CLUSTER 1. ALL THE OTHERS ARE EQUAL'

#POINT 3
'Compute the average Fertility in the three clusters found via k-means.
What is the cluster with the highest fertility? Can you make sense of this finding?'

s <- swiss
s
mean(s[km_swiss$cluster==1,]$Fertility)
mean(s[km_swiss$cluster==2,]$Fertility)
mean(s[km_swiss$cluster==3,]$Fertility)

'The cluster with the highest fertility is cluster 2, with 80.55'
#Let's see why
plot(Fertility ~ Catholic, swiss, xlab="Catholic", las=3)
#The higher degree of catholic the higher fertility
mean(s[km_swiss$cluster==1,]$Catholic)
mean(s[km_swiss$cluster==2,]$Catholic)
mean(s[km_swiss$cluster==3,]$Catholic)
#So, cluster 2 contains the highest percentage of catholic people. For this reason in cluster 2
#we have the highest fertility.


# POINT 4
"text consider now model-based clustering with gaussian mixtures. Use r library to select the best model
with 3 groups by BIC method. What model is chosen? Discuss"
s <- s[,2:6]

library(mclust)

#mclust already uses BIC to choose the best model
fit<-Mclust(s,G=3)
fit$modelName
# best one = EEE  (elliposidal, equal volume, shape and orientation)
# The above model fits a separate mean vector for each class, but the same ellipsoidal covariance
#matrix, which is essentially equivalent to linear discriminant analysis.

plot(fit, what="classification")

table(hc_labels,fit$classification)


# POINT 5
"COmpare the k-means cluster solution with the model based one. total freedom in this
can you make sense of the differences"

s[fit$classification==3,]
fit$classification == hc_labels # confronto hc
fit$classification == km_label3 # confronto km
table(km_label3,fit$classification)

"the only three that now form the third cluster are geneve, rive droite
and rive gauche that belong to canton geneve"
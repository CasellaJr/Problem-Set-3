
                                 ## Lecture_CA ##
library(mcclust)

############################################
#                                           #
## Financial performance data

rm(list=ls())
#                                           # clear workspace

financial<-read.table("data/financial.txt",header=FALSE) 

names(financial)<-c("Type", "Symbol", "obs_num", "ROR5", "DE", 
                  "SalesGR5", "EPS5", "NPM1", "PE", "PayoutR1")

financial[1:4,]

dim(financial)


financial<-financial[,4:10]
#                                           # variables considered for CA
summary(financial)
financial_s<-scale(financial)
#                                           # standardize obs
head(financial_s)
financial_s[,1]<--financial_s[,1]
financial_s[,7]<--financial_s[,7]
#                                           # change signs of variables
#                                           # ROR5 and PayoutR1 so to 
#                                           # prevent too much crossing
#                                           # in the profile diagram

plot(financial_s[1,],type="l",ylim=c(-2.5,2.5))
abline(h=0,lty=2,lwd=2,col="blue")
#                                           # profile diagram 1st obs

plot(financial_s[1,],type="l",ylim=c(-2.5,2.5),axes=F,
     ylab="Standardize values", xlab="variables",
     main="Financial performance data")
abline(h=0,lty=2,lwd=2,col="blue")
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=c("-ROR5","DE","SalesGR5", "EPS5", "NPM1", "PE", "-PayoutR1"))
box()

financial[15:21,]

# pdf("../graphics/chemical.pdf",paper="special")

matplot(t(financial_s)[,15:21],type="l",col=c(1,1,1,1,1,2,2),axes=F,
        ylab="Standardize values", xlab="variables")
#                                           # profile diagram of 7 obs
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=c("-ROR5","DE","SalesGR5", "EPS5", "NPM1", "PE", "-PayoutR1"))
box()
abline(h=0,lty=2,lwd=2,col="blue")
legend("bottomright",c("Health company","Supermarket company"),
       lty=1:1,col=c(1,2),bty="n")

# dev.off()

matplot(t(financial_s),type="l",col=c(rep(4,14),rep(1,5),rep(2,6)),axes=F,
        ylab="Standardize values", xlab="variables")
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=c("-ROR5","DE","SalesGR5", "EPS5", "NPM1", "PE", "-PayoutR1"))
box()
abline(h=0,lty=2,lwd=2,col="green")
legend("bottomright",c("Chemical company","Hospital company","Supermarket company"),
       lty=c(1,1,1),col=c(4,1,2),bty="n")

means.hat<-aggregate(financial_s[,1:7],by=list(financial$Type),mean)
means.hat
matplot(t(means.hat[,-1]),type="l",lwd=rep(2.5,3),col=c(4,2,1),
        lty=rep(1,3),add=T)

# round(financial_s[15:21,],2)
financial[15:21,-c(2,3)]

out<-as.matrix(dist(financial_s[15:21,]))
colnames(out)<-rownames(out)<-15:21
round(out,2)

out<-as.matrix(dist(financial_s[15:21,],method="manhattan"))
colnames(out)<-rownames(out)<-15:21

round(out,2)

############################################

## binary variables

i<-c(1,0,0,1,1); j<-c(1,1,0,1,0)

X<-rbind(i,j); colnames(X)<-1:5
X
sum((i-j)^2)
table(i,j)
?dist

as.matrix(dist(X,"binary"))
#                                           # "binary": distance is 
#                                           # proportion of (binary) 
#                                           # variables in which 
#                                           # only one is 1 amongst 
#                                           # those in which at least 
#                                           # one is 1
#                                           # i.e. Jaccard coef
1-2/(2+1+1)
#                                           # the same

sum((i-j)^2)/ncol(X)
#                                           # simple dissimilarity 
#                                           # coefficient
i<-c(1,1,1,1,0,1); j<-c(1,1,0,0,0,0)
X<-rbind(i,j); 
colnames(X)<-c("Father","Sons","Owned house",
               "Owned PC", "Animals", "Week holy")
X
table(i,j)
as.matrix(dist(X,"binary"))
1-2/(2+3+0)

sum((i-j)^2)/ncol(X)

############################################

## Example with single linkage

rm(list=ls())
#                                           # clear workspace

d<-matrix(c(0,9,3,6,11, 9,0,7,5,10, 3,7,0,9,2, 6,5,9,0,8,11,10,2,8,0), 
          nrow=5, ncol=5, byrow=TRUE)
dd <- as.dist(d)
# colnames(d)<-rownames(d)<-paste("obs",1:5,sep="")
as.matrix(d)
xs <- hclust(dd, method="single")

class(xs)

# pdf("../graphics/dendro_single.pdf",paper="special")
# pdf("../graphics/dendro_single2.pdf",paper="special")

plot(xs)

cutree(xs,h=5)
cutree(xs,k=2)
#                                           # the same

rect.hclust(xs, k = 2, border = "red")

# dev.off()

# pdf("../graphics/dendro_complete.pdf",paper="special")

xc <- hclust(dd, method="complete")
plot(xc)

# dev.off()

# pdf("../graphics/dendro_average.pdf",paper="special")

xm <- hclust(dd, method="average")
plot(xm)

# dev.off()

############################################

## Language data 

rm(list=ls())
#                                           # clear workspace

tab<-read.table("data/language.txt",header=F) 
names(tab)<-c("E","N","Da","Du","G","Fr","Sp","I","P","H","Fi")
rownames(tab)<-c("E","N","Da","Du","G","Fr","Sp","I","P","H","Fi")
tab

tab<-as.matrix(tab)
tab<-tab+t(tab)-diag(diag(tab),nrow=11)
tab

dd<-10-tab
dd<-as.dist(dd)
as.matrix(dd)

lang.s <- hclust(dd, method="single")

# pdf("../graphics/dendro_lang.pdf",paper="special")

plot(lang.s)

# dev.off()

lang.s$merge
      # [,1] [,2]
 # [1,]   -2   -3 N and Da are merged
 # [2,]   -6   -8 Fr and I are merged
 # [3,]   -7    2 Sp is merged with cluster formed at earlier stage 2, i.e. Fr-I
 # [4,]   -1    1 E is merged with cluster formed at earlier stage 1, i.e. N-Da
 # [5,]   -9    3 P is merged with cluster at earlier stage 3, i.e Fr-I-Sp
 # [6,]   -5    4 G is merged with cluster at earlier stage 4, i.e N-Da-E
 # [7,]   -4    6 Du is merged with cluster at earlier stage 6, i.e N-Da-E-G
 # [8,]    5    7 cluster at earlier stage 5, i.e. Fr-I-Sp-P, is merged with 
 #                cluster at earlier stage 7, i.e. N-Da-E-G-Du
 # [9,]  -10    8 H is merged with cluster at earlier stage 8, i.e. Fr-I-Sp-P-N-Da-E-G-Du
# [10,]  -11    9 Fi is merged with cluster at stage 9, i.e. Fr-I-Sp-P-N-Da-E-G-Du-H

lang.s$height                                  
#                                           # the value of the criterion
#                                           # associated with the 
#                                           # clustering method for 
#                                           # the particular 
#                                           # agglomeration taken place

lang.c <- hclust(dd, method="complete")

# pdf("../graphics/dendro_lang2.pdf",paper="special")

plot(lang.c)

# dev.off()

# lang.c$merge

lang.m <- hclust(dd, method="average")

# pdf("../graphics/dendro_lang3.pdf",paper="special")

plot(lang.m)

# dev.off()

############################################

## Utility data

rm(list=ls())
#                                           # clear workspace

utility<-read.table("data/utility.txt",header=FALSE) 


row.names(utility)<-utility[,9]
utility<-utility[,-9]

names(utility)<-paste("x",1:8,sep="")
utility

dd<-dist(utility, method="euclidean")
# dd
utility.a <- hclust(dd, method="average")

# pdf("../graphics/dendro_utility.pdf",paper="special")

plot(utility.a)
rect.hclust(utility.a, k = 5, border = "red")
# identify(utility.a,N=2)

# dev.off()

############################################

## USA arrest data
## Ward's hierarchical clustering

rm(list=ls())
#                                           # clear workspace

# ?USArrests
USArrests[1:4,]

hc<- hclust(dist(USArrests), method="ward.D2")
# hc<- hclust(dist(USArrests), method="ward.D")
# hc<- hclust(dist(USArrests)^2, method="ward.D")

# why ward.D2? see murtagh,legendre (2014 journal of classification)

# pdf("../graphics/dendro_arrest.pdf",paper="special")
plot(hc)
# dev.off()

# pdf("../graphics/dendro_arrest_3grp.pdf",paper="special")
plot(hc)
rect.hclust(hc, k=3, border="red")
# dev.off()

g<-cutree(hc,k=3)
n.i<-as.numeric(table(g))
means.hat<-aggregate(USArrests,by=list(g),mean)[,-1]
means.hat<-as.matrix(means.hat)
means.hat
dd<-dist(means.hat,method="euclidean")
dd

dd[1]^2*n.i[1]*n.i[2]/(n.i[1]+n.i[2])
dd[2]^2*n.i[1]*n.i[3]/(n.i[1]+n.i[3])
dd[3]^2*n.i[2]*n.i[3]/(n.i[2]+n.i[3])
# smallest is distance cluster 2 (Arkansas, 2nd from right) 
# and 3 (Cunnecticut, 1st from right)

out<-by(USArrests,g,var)
out
wss<-sum(diag(out[[1]])*(n.i[1]-1)+
           diag(out[[2]])*(n.i[2]-1)+
           diag(out[[3]])*(n.i[3]-1)
)
wss

out.2<-aggregate(USArrests,by=list(g),var)[,-1]
out.2
rbind( diag(out[[1]]),diag(out[[2]]),diag(out[[3]]) )

rowSums(out.2*(n.i-1))
sum((out.2)*(n.i-1))
wss
# the same
names(hc)
hc$height
hc$height[48]
sqrt(wss)
# no
sqrt(dd[3]^2*n.i[2]*n.i[3]/(n.i[2]+n.i[3]))
# no

g2<-cutree(hc,k=2)
n.i2<-as.numeric(table(g2))
out.2<-aggregate(USArrests,by=list(g2),var)[,-1]
wss2<-sum((out.2)*(n.i2-1))
wss2
wss
wss2-wss
dd[3]^2*n.i[2]*n.i[3]/(n.i[2]+n.i[3])
# the same
hc$height[48]
sqrt(wss2)
# no

tss<-sum(diag(var(USArrests)))*(dim(USArrests)[1]-1)
tss
hc$height[49]
sqrt(tss)
# no

# pdf("../graphics/dendro_arrest_6grp.pdf",paper="special")
plot(hc)
rect.hclust(hc, k=6, border="red")
# dev.off()

cutree(hc,k=6)

cbind(cutree(hc,k=3),cutree(hc,k=6))

############################################
############################################

## K-means clustering

rm(list=ls())
#                                           # clear workspace

X<-matrix(c(3,5,1,2,4,3,7,1,4,2,6,6,1,1,3,4,2,2),ncol=2,byrow=T)
X

clust<-c(2,1,2,1,2,2,1,2,1)
cbind(X,clust)

# pdf("../graphics/2means2_1.pdf",paper="special")

# plot(X,pch=16,col=clust,asp=1)
data.col<-ifelse(clust==1, "cyan", "orange")
plot(X,pch=16,col=data.col,asp=1)

means<-aggregate(X,by=list(clust),FUN=mean)[-1]
points(means,pch=21,bg=c("cyan","orange"),cex=1.2)
legend("topleft",c("Cluster 1","Cluster 2"),
       pch=16,col=c("cyan","orange"),bty="n")

# dev.off()

# out<-by(X,clust,var)
out<-aggregate(X,by=list(clust),var)[,-1]
n.i<-table(clust); n.i

# wss<-sum(diag(out[[1]])*(n.i[1]-1)+diag(out[[2]])*(n.i[2]-1))
wss<-sum((out)*(n.i-1))
wss
#                                           # within sum of squares
# wss<-n.i[1]*sum(diag(out[[1]]))*(n.i[1]-1)+
#   n.i[2]*sum(diag(out[[2]]))*(n.i[2]-1)
wss<-rowSums((out)*(n.i-1))
wss<-sum(n.i*wss)
wss
#                                           # weighted 
#                                           # within sum of squares

#                                           # (7,1) is 4th obs 
#                                           # it belongs to 
#                                           # cluster 1
dist.mat<-as.matrix(dist(rbind(X,means)))
rownames(dist.mat)[10:11]<-colnames(dist.mat)[10:11]<-paste("C",1:2,sep="")
round(dist.mat,2)

clust[4]
#                                           # distance from centroid 2 
#                                           # is smaller
clust[5]
#                                           # distance from centroid 1 
#                                           # is smaller

clust[4]<-2;clust[5]<-1
cbind(X,clust)
#                                           # (7,1) reassigned to 
#                                           # cluster 2
#                                           # (4,2) reassigned to 
#                                           # cluster 1

means<-aggregate(X,by=list(clust),FUN=mean)[,-1]

# pdf("../graphics/2means2_2.pdf",paper="special")

data.col<-ifelse(clust==1, "cyan", "orange")
plot(X,pch=16,col=data.col,asp=1)
points(means,pch=21,bg=c("cyan","orange"),cex=1.2)
legend("topleft",c("Cluster 1","Cluster 2"),
       pch=16,col=c("cyan","orange"),bty="n")

# dev.off()
out<-aggregate(X,by=list(clust),var)[,-1]
n.i<-table(clust); n.i

wss<-sum((out)*(n.i-1))
wss
#                                           # within sum of squares
wss<-rowSums((out)*(n.i-1))
wss<-sum(n.i*wss)
wss
#                                           # weighted 
#                                           # within sum of squares

#                                           # wss should be smaller

############################################

## USA arrest data

rm(list=ls())
#                                           # clear workspace

USArrests[1:4,]

# ?kmeans
set.seed(1)
km3<-kmeans(USArrests, centers=3, nstart=25, 
            iter.max=100, algorithm="MacQueen")
names(km3)
km3
km3$cluster
#                                           # returns vector of 
#                                           # cluster assignments
#                                           # label are assigned by 
#                                           # least element criterion

fitted(km3)
#                                           # returns cluster 
#                                           # assignment
#                                           # and cluster centers

out<-aggregate(USArrests,by=list(km3$cluster),var)[,-1]
n.i<-table(km3$cluster); n.i

(out)*(n.i-1)
rowSums((out)*(n.i-1))
km3$withinss
#                                           # the same
wss<-sum((out)*(n.i-1))
wss
km3$tot.withinss
#                                           # the same

set.seed(1)
n<-nrow(USArrests)
clust<-numeric(0)
size<-matrix(0,ncol=6,nrow=7)
wss<-numeric(0)
tss<-(n-1)*sum(apply(USArrests,2,var))

for(i in 2:7) {
	set.seed(2)
	km.arrest<- kmeans(USArrests, centers=i, nstart=25, 
	                   iter.max=100, algorithm="MacQueen")
	W<-sum(km.arrest$withinss)
	wss<-c(wss,W)
	clust<-cbind(clust,km.arrest$cluster)
	size[1:i,(i-1)]<-km.arrest$size
}
#

colnames(clust)<-paste(2:7, "groups", sep=" ")
clust

colnames(size)<-paste(2:7, "groups", sep=" ")
rownames(size)<-paste("Group", 1:7,  sep=" ")
size

wss<-c(tss,wss)
delta.wss<-c(0,-diff(wss))
out<-rbind(wss,diff=delta.wss)
colnames(out)<-paste(1:7, "groups", sep=" ")
out

bss<-tss-wss
out<-rbind(wss,bss)
colnames(out)<-paste(1:7, "groups", sep=" ")
rownames(out)<-c("WSS","BSS")
out

# pdf("../graphics/arrest_wss.pdf",paper="special")

plot(1:7,out[1,],type="b",xlab="Number of groups",
     ylab="Within sum of squares",lwd=2)
abline(v=3,lty=2,col="blue")

# dev.off()

calinski<-(out[2,-1]/((2:7)-1))/(out[1,-1]/(n-(2:7)))
out<-rbind(out,c(NA,calinski))
rownames(out)[3]<-"calinski"
out

# pdf("../graphics/calinski.pdf",paper="special")

plot(2:7,calinski,type="b",xlab="Number of groups",ylab="Calinski",lwd=2)
abline(v=3,lty=2,col="blue")
abline(v=6,lty=2,col="blue")

# dev.off()

# library(vegan)
# set.seed(2)
# km<- cascadeKM(USArrests, inf.gr=2, sup.gr=7, iter=100,criterion="calinski")
# km

# plot(2:7,km$results[2,],
#      type="l",xlab="Number of groups",ylab="Calinski",lwd=2)

# pdf("../graphics/calinski2.pdf",paper="special")

# plot(km)

# dev.off()

# km$results

# pdf("../graphics/calinski_SSE.pdf",paper="special")

plot(1:7,out[1,],type="b",xlab="Number of groups",
     ylab="Within sum of squares",lwd=2)
abline(v=3,lty=2,col="blue")
abline(v=6,lty=2,col="blue")

# dev.off()

set.seed(1)
km6<-kmeans(USArrests, centers=6, nstart=25, 
            iter.max=100, algorithm="MacQueen")
names(km6)

hc<- hclust(dist(USArrests), method="ward.D2")

# pdf("../graphics/dendro_arrest_6grp.pdf",paper="special")

plot(hc)
rect.hclust(hc, k=6, border="red")

# dev.off()

# comparison km vs ward


hc_labels<-cutree(hc,k=6); 
# hc_labels

km6_labels<-km6$cluster; 
#km6_labels

# cbind(hc_labels,km6_labels)

table(km6_labels)
table(hc_labels)

addmargins(table(hc_labels,km6_labels))
#                                           # perfect agreement if 
#                                           # all counts are 
#                                           # on the diagonal
                                                 
#                                           # labelling is different
#                                           # distance measure between
#                                           # cluster assignements
#                                           # should be invariant to
#                                           # permutations of 
#                                           # data point indices 
#                                           # and of cluster labels

#                                           # labelling assigned by 
#                                           # k-means
x<-km6_labels
names(x)<-NULL
x
unique(x)
order(unique(x))
order(unique(x))[unique(x)]
sort(unique(x))
#                                           # the same
order(unique(x))[x]
#                                           # assigning labels 
#                                           # according to
#                                           # least element 
km6_labels2<-order(unique(x))[x]
km6_labels2

#                                           # labelling assigned by 
#                                           # Ward's method
x<-hc_labels
names(x)<-NULL
x
unique(x)
# order(unique(x))
# order(unique(x))[x]
#                                           # labelling assigned 
#                                           # by hclust()
#                                           # corresponds to least 
#                                           # element criterion
cbind(hc_labels,km6_labels2)
#                                           # California and New Mexico
#                                           # have different cluster 
#                                           # assignements

out<-table(hc_labels,km6_labels2)
addmargins(out)
#                                           # now labelling is similar
#                                           # counts off diagonal 
#                                           # are number of missmatch
sum(out)-sum(diag(out))
n

error<-1-sum(diag(out))/sum(out)
error

library(mcclust)

# ?arandi
arandi(hc_labels,km6_labels)
arandi(hc_labels,km6_labels,adjust=F)
arandi(hc_labels,km6_labels2,adjust=F)
#                                           # the same

#                                           # Rand Index: measure 
#                                           # of similarity defined 
#                                           # as the number of 
#                                           # agreements in the 
#                                           # choose(n,2) possible 
#                                           # pairs of obs divided 
#                                           # by choose(n,2)
choose(n,2)
#                                           # num of possible 
#                                           # pairs of abs
arandi(hc_labels,km6_labels,adjust=F)*choose(n,2)
#                                           # tot num of agreements

(1-arandi(hc_labels,km6_labels,adjust=F))*choose(n,2)
#                                           # binder loss tot num of
#                                           # disagreements

(sum(addmargins(out)[7,1:6]^2)+
#                                           # sum squares of col sums
 sum(addmargins(out)[1:6,7]^2)-
#                                           # sum squares of row sums
 2*sum(out^2))/2
#                                           # sum of squared counts
#                                           # the same
#                                           # cf equation (1)

# (10^2+6^2+12^2+4^2-2*(10^2+2^2+4^2))/2
#                                           # the same
#                                           # i.e. equation (1) 
#                                           # restricted to first
#                                           # 2 rows and cols
# 2*(4+10)
#                                           # the same
#                                           # cfr centered equation 
#                                           # after equation (1)

#                                           # also see
#                                           # wade,gharamani (2017ba)
#                                           # binder (1978bmk)
(1-arandi(hc_labels,km6_labels,adjust=F))
error
#                                           # different measures of 
#                                           # disagreement

############################################

## crime profile data


# from http://flowingdata.com/2010/08/31/how-to-visualize-data-with-cartoonish-faces/

#crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")

rm(list=ls())
#                                           # clear workspace

crime<-read.table("data/crime.txt",header=TRUE,sep=",",as.is=T) 

crime[1:10,]
crime[10,1]<-"DC"
#                                           # change state name
crime<-crime[-1,]
#                                           # omit 1st obs "US"

state<-crime$state
#                                           # state names

crime<-crime[,-1]
#                                           # omit state variable
row.names(crime)<-state
#                                           # state names as row names
# names(crime)
names(crime)[c(2,4,6,7)]<-c("rape","assault","theft","vehicle")

crime[1:10,]
dim(crime)

# pdf("../graphics/scatter_crime.pdf",paper="special")

# pairs(crime)

pairs(crime, pch = ".", cex = 1.5)

# dev.off()

subset(crime, murder > 15)

crime[crime$murder>15,]
#                                           # the same

# pdf("../graphics/crux_scatter.pdf",paper="special")

pairs(crime, pch = c(".", "+")[(rownames(crime) == "DC") + 1],cex = 1.5)

# dev.off()

crime<-subset(crime, murder <= 15)
#                                           # remove obs 'DC'
dim(crime)
n<-nrow(crime);p<-ncol(crime)
crime[1:10,]

round(diag(var(crime)),2)
#                                           # variances are different
crime_s<-data.frame(scale(crime))
#                                           # standardize var
# rge<-rep(0,p)
# for(i in 1:p){
#   rge[i]<-range(crime[,i])[2]-range(crime[,i])[1]
# }
# crime_s<-as.matrix(crime)%*%diag(1/rge)
# crime_s<-data.frame(crime_s)
# names(crime_s)<-names(crime)

#                                           # rescaling 
#                                           # with respect to
#                                           # the range of the
#                                           # variables
# round(diag(var(crime_s)),2)

tss<- (n-1)*sum(diag(var(crime_s)))
# tss; sum(diag(cov(crime_s)))*(n-1)
# tss; ncol(crime_s)*(n-1)
#                                           # the same
wss<- rep(0, 6)
wss[1]<- tss

set.seed(1)
for (i in 2:6){
	wss[i]<- kmeans(crime_s, centers=i, nstart=25, 
                  iter.max=100, algorithm="MacQueen")$tot.withinss
	}

# pdf("../graphics/groups_k.pdf",paper="special")

plot(1:6, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares",lwd=2)
abline(v=2,lty=2,col="blue")

# dev.off()

calinski<-((tss-wss[-1])/((2:6)-1))/(wss[-1]/(n-(2:6)))
# calinski<-(wss[-1]/((2:6)-1))/((wss[1]-wss[-1])/(n-(2:6)))
out<-rbind(wss,c(NA,calinski))
rownames(out)[2]<-"calinski"
colnames(out)<-paste(1:6, "groups", sep=" ")
out


# pdf("../graphics/calinski_crime1.pdf",paper="special")

plot(2:6,calinski,type="b",
     xlab="Number of groups",ylab="Calinski",main="Crime data",lwd=2)
points(2,calinski[1],col="red",pch=16)

# dev.off()

# library(vegan)
# km1.crime<- cascadeKM(crime_s, inf.gr=2, sup.gr=6, iter=100,criterion="calinski")

# pdf("../graphics/calinski_crime2.pdf",paper="special")

# plot(km1.crime)

# dev.off()

set.seed(2)
km.crime<- kmeans(crime_s, nstart=25, centers=2, 
                  iter.max=100, algorithm="MacQueen")
# km.crime<- kmeans(crime_s, nstart=25, centers=3,
#                   iter.max=100, algorithm="MacQueen")
km.crime
# names(km.crime)

# centroids<-as.matrix(aggregate(crime_s,by=list(km.crime$cluster),
                               # FUN=mean)[,-1])
# centroids<-centroids%*%diag(rge)
# centroids<-km.crime$centers * rbind(rge,rge)

centroids<-aggregate(crime,by=list(km.crime$cluster),FUN=mean)[,-1]

round(centroids,2)

# pdf("../graphics/clusters_crime_profile.pdf",paper="special")

matplot(t(centroids[,-1]),type="l",col=c(1,2),axes=F,lty=c(1,1),
        ylab="crime rates", xlab="variables")
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=names(crime))
box()
legend("bottomright",c("Cluster 1","Cluster 2"),
       lty=1:1,col=c(1,2),bty="n")

# dev.off()

crime_pca<- prcomp(crime_s)
summary(crime_pca)
round(crime_pca$rotation[,1:2],3)

# crime_pca

names(km.crime)
km.crime$cluster

# pdf("../graphics/clusters_crime_pc.pdf",paper="special")
#                                           # plot of first two PCs
plot(crime_pca$x[,1],crime_pca$x[,2],type = "n",asp=1,   
         xlab = "PC1", ylab = "PC2", main="Crime data, K=2")            
text(crime_pca$x[,1],crime_pca$x[,2],labels=km.crime$cluster, 
     col = km.crime$cluster) 

# dev.off()

set.seed(5)
km.crime3<- kmeans(crime_s, nstart=25, centers=3, 
                   iter.max=100, algorithm="MacQueen")

# centroids<-km.crime3$centers * rbind(rge,rge)
centroids<-aggregate(crime,by=list(km.crime3$cluster),FUN=mean)[,-1]
centroids
# pdf("../graphics/clusters_crime_profile2.pdf",paper="special")

matplot(t(centroids),type="l",col=c(1:3),axes=F,lty=c(1,1,1),
        ylab="crime rates", xlab="variables")
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=names(crime))
box()
legend("bottomright",paste("Cluster",1:3),
       lty=c(1,1,1),col=c(1:3),bty="n")

# dev.off()

# pdf("../graphics/clusters_crime_pc2.pdf",paper="special")

plot(crime_pca$x[,1],crime_pca$x[,2],type = "n",asp=1,   
         xlab = "PC1", ylab = "PC2", main="Crime data, K=3")            
text(crime_pca$x[,1],crime_pca$x[,2],labels=km.crime3$cluster, 
     col = km.crime3$cluster) 

# dev.off()

round(crime_pca$rotation[,1:2],3)


############################################

## Protein data

rm(list=ls())
# clear workspace

protein<-read.table("data/protein.txt",header=TRUE) 

head(protein)
dim(protein)

diag(var(protein))

# protein_s<-data.frame(scale(protein))
protein_s<-protein
prot.names<-rownames(protein)

protein_pca<-prcomp(protein_s)

pdf("../graphics/protein_pca.pdf",paper="special")
#                                           # plot of first two PCs
plot(PC2~PC1, data=protein_pca$x,pch=16,asp=1, cex=0.5,   
     xlab = "PC1", ylab = "PC2", main="Protein data")            
text(PC2~PC1, data=protein_pca$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1) 

dev.off()

## k=4 

k<-4
n<-nrow(protein)

## initial seeds chosen at random

set.seed(1)
i<-sample(1:n,k)
prot.names[i]

km_protein<-kmeans(protein_s, iter.max=100, algorithm = "MacQueen",
                   centers= protein_s[i,])
km_label1<-km_protein$cluster
WSS1<-round(km_protein$tot.withinss,2); WSS1

dd<-as.matrix(dist(rbind(km_protein$centers,protein_s)))[1:k,(k+1):(k+n)]
ind<-cbind(km_protein$cluster,1:n)

out<-cbind(km_protein$cluster,dd[ind])
colnames(out)<-c("cluster","dist_centr")
round(out[1:6,],2)

pdf("../graphics/protein_km1.pdf",paper="special")

plot(PC2~PC1, data=protein_pca$x,pch=16,asp=1, cex=0.8,   
     xlab = "PC1", ylab = "PC2", 
     main=paste("Protein data, WSS=",WSS1, sep=""),
     col=km_protein$cluster)            
text(PC2~PC1, data=protein_pca$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1,
     col=km_protein$cluster) 
legend("topleft",paste("clust",unique(km_protein$cluster)),
       lty=rep(1,k),col=unique(km_protein$cluster),bty="n",cex=0.8)

dev.off()

protein_s$class<-km_protein$cluster
library(MASS)
protein.lda<-lda(class~.,data=protein_s)
lda.pred<-predict(protein.lda)

pdf("../graphics/protein_km1bis.pdf",paper="special")

plot(LD2~LD1,data=lda.pred$x,asp=1,cex=0.8,
     pch=16,main="Protein data, LD space",col=km_protein$cluster)         
text(LD2~LD1,data=lda.pred$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1,
     col=km_protein$cluster) 
legend("topleft",paste("clust",unique(km_protein$cluster)),
       lty=rep(1,k),col=unique(km_protein$cluster),bty="n",cex=0.8)

dev.off()


## initial seeds farthest apart

dd <- dist(protein_s, method="euclidean")
dmat<-as.matrix(dd)
diag(dmat)<-NA

set.seed(1)
i<-sample(1:n,1)
prot.names[i]

dmax<-max(dmat[,i],na.rm=TRUE)
ind<-which(dmat[,i] == dmax, arr.ind = TRUE)
i<-union(i,which(dmat[,i] == dmax, arr.ind = TRUE))
prot.names[i]

while(length(i)<k){
  dmax<-max(apply(dmat[-i,i],1,min,na.rm=TRUE))
  ind<-rownames(which(dmat[-i,i] == dmax, arr.ind = TRUE))
  ind<-which(prot.names==ind)
  i<-union(i,ind)
}

prot.names[i]

pdf("../graphics/protein_farthest.pdf",paper="special")

plot(PC2~PC1, data=protein_pca$x,pch=16,asp=1, cex=0.8,   
     xlab = "PC1", ylab = "PC2", main="Protein data")            
text(PC2~PC1, data=protein_pca$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1) 
points(PC2~PC1, data=protein_pca$x[i,],
       pch=16,cex=1,col="red") 

dev.off()

km_protein<-kmeans(protein_s, iter.max=100, algorithm = "MacQueen",
                   centers= protein_s[i,])
km_label2<-km_protein$cluster
WSS2<-round(km_protein$tot.withinss,2); WSS2

dd<-as.matrix(dist(rbind(km_protein$centers,protein_s)))[1:k,(k+1):(k+n)]
ind<-cbind(km_protein$cluster,1:n)

out<-cbind(km_protein$cluster,dd[ind])
colnames(out)<-c("cluster","dist_centr")
round(out[1:6,],2)

pdf("../graphics/protein_km2.pdf",paper="special")

plot(PC2~PC1, data=protein_pca$x,pch=16,asp=1, cex=0.8,   
     xlab = "PC1", ylab = "PC2",  
     main=paste("Protein data, WSS=",WSS2, sep=""),
     col=km_protein$cluster)            
text(PC2~PC1, data=protein_pca$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1,
     col=km_protein$cluster) 
legend("topleft",paste("clust",unique(km_protein$cluster)),
       lty=rep(1,k),col=unique(km_protein$cluster),bty="n",cex=0.8)

dev.off()

protein_s$class<-km_protein$cluster
# library(MASS)
protein.lda<-lda(class~.,data=protein_s)
lda.pred<-predict(protein.lda)

pdf("../graphics/protein_km2bis.pdf",paper="special")

plot(LD2~LD1,data=lda.pred$x,asp=1,cex=0.8,
     pch=16,main="Protein data, LD space",col=km_protein$cluster)         
text(LD2~LD1,data=lda.pred$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1,
     col=km_protein$cluster) 
legend("topleft",paste("clust",unique(km_protein$cluster)),
       lty=rep(1,k),col=unique(km_protein$cluster),bty="n",cex=0.8)

dev.off()


## initial seeds from Ward's method

hc<- hclust(dist(protein_s), method="ward.D2")
hc_labels<-cutree(hc,k)
centroids<-as.matrix(aggregate(protein_s,by=list(hc_labels),FUN=mean)[,-1])
km_protein<-kmeans(protein_s, iter.max=100, algorithm = "MacQueen",
                   centers= centroids)
km_label3<-km_protein$cluster
WSS3<-round(km_protein$tot.withinss,2); WSS3

dd<-as.matrix(dist(rbind(km_protein$centers,protein_s)))[1:k,(k+1):(k+n)]
ind<-cbind(km_protein$cluster,1:n)

out<-cbind(km_protein$cluster,dd[ind])
colnames(out)<-c("cluster","dist_centr")
round(out[1:6,],2)

pdf("../graphics/protein_km3.pdf",paper="special")

plot(PC2~PC1, data=protein_pca$x,pch=16,asp=1, cex=0.8,   
     xlab = "PC1", ylab = "PC2",   
     main=paste("Protein data, WSS=",WSS3, sep=""),
     col=km_protein$cluster)            
text(PC2~PC1, data=protein_pca$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1,
     col=km_protein$cluster) 
legend("topleft",paste("clust",unique(km_protein$cluster)),
       lty=rep(1,k),col=unique(km_protein$cluster),bty="n",cex=0.8)

dev.off()

protein_s$class<-km_protein$cluster
protein.lda<-lda(class~.,data=protein_s)
lda.pred<-predict(protein.lda)

pdf("../graphics/protein_km3bis.pdf",paper="special")

plot(LD2~LD1,data=lda.pred$x,asp=1,cex=0.8,
     pch=16,main="Protein data, LD space",col=km_protein$cluster)         
text(LD2~LD1,data=lda.pred$x,labels=prot.names,
     pos=4,cex=0.8,offset=0.1,
     col=km_protein$cluster) 
legend("topleft",paste("clust",unique(km_protein$cluster)),
       lty=rep(1,k),col=unique(km_protein$cluster),bty="n",cex=0.8)

dev.off()

library(mcclust)

arandi(km_label1,km_label2,adjust=F)
arandi(km_label1,km_label3,adjust=F)
arandi(km_label2,km_label3,adjust=F)

(1-arandi(km_label2,km_label3,adjust=F))*choose(n,2)
choose(n,2)


############################################

## Financial performance data

rm(list=ls())
#                                           # clear workspace

financial<-read.table("data/financial.txt",header=FALSE) 

names(financial)<-c("Type", "Symbol", "obs_num", "ROR5", "DE", "SalesGR5", "EPS5", "NPM1", "PE", "PayoutR1")

financial[1:4,]
company.names<-c(paste("C",1:14,sep=""),
                 paste("H",15:19,sep=""),
                 paste("G",20:25,sep=""))

financial_s<-data.frame(scale(financial[,4:10]))
financial_s[,1]<--financial_s[,1]
financial_s[,7]<--financial_s[,7]
rownames(financial_s)<-company.names
  
## Hierarchical agglomerative clusterings, average linkage

dd <- dist(financial_s, method="euclidean")
xm <- hclust(dd, method="average")

# pdf("../graphics/dendro_aver_clust.pdf",paper="special")

plot(xm)

# dev.off()

# pdf("../graphics/dendro_aver_clust7.pdf",paper="special")

plot(xm)
rect.hclust(xm, k=7, border="red")

# dev.off()

# pdf("../graphics/dendro_aver_clust9.pdf",paper="special")

plot(xm)
rect.hclust(xm, k=9, border="blue")

# dev.off()

# plot(xm)
# identify(xm,N=1)
 #                                           # identify group with 
 #                                           # companies 1,2,3
# rect.hclust(xm, k=20, border="red")

# plot(xm)
# identify(xm,N=1)
 #                                           # identify group with 
 #                                           # companies 15-18
# rect.hclust(xm, k=2, border="red")

plot(xm)
identify(xm,N=1)
 #                                           # identify group with 
 #                                           # companies 1-13
rect.hclust(xm, k=7, border="red")

plot(xm)
identify(xm,N=1)
 #                                           # identify group with 
 #                                           # companies 16-18
rect.hclust(xm, k=9, border="red")


# pdf("../graphics/clust_DE.pdf",paper="special")

col.index<-c(rep(1,14),rep(4,5),rep(2,6))
plot(financial_s$DE, type = "n", ylab="DE",main="Financial performance data")
text(1:25,financial_s$DE,labels=company.names,col=col.index) 

# dev.off()

# K-means clustering

set.seed(1)
km.clust<- kmeans(financial_s, iter.max=100, centers=3, algorithm="MacQueen")
km.clust

# kmeans(financial_s, iter.max=100, centers=3, nstart=1)$cluster
# kmeans(financial_s, iter.max=100, centers=3, nstart=1)$cluster
# kmeans(financial_s, iter.max=100, centers=3, nstart=1)$cluster

x<-km.clust$cluster
x
unique(x)
order(unique(x))
unique(x)[order(unique(x))]
order(unique(x))[x]
#                                           # account for label
#                                           # switching
#                                           # assign labels 
#                                           # according to
#                                           # least element in 
#                                           # the group


set.seed(1)
niter<-10
n<-dim(financial_s)[1]
# results<-matrix(0,ncol=niter,nrow=n)
results<-matrix(0,ncol=niter,nrow=n+1)
# row.names(results)<-company.names
row.names(results)<-c(company.names,"wss")
# results

for (a in 1:niter){
	set.seed(a)
  # km.clust<- kmeans(financial_s, iter.max=100, centers=3, 
  #                    nstart=1, algorithm = "MacQueen")
  km.clust<- kmeans(financial_s, iter.max=100, centers=3, 
                   nstart=25, algorithm = "MacQueen")
	x<-km.clust$cluster
	y<-order(unique(x))[x]
	# y<-km.clust$cluster
	# financial_s[x==x[1]]<--1
	# ind<-which(y>0)[1]
	# financial_s[x==x[ind]]<--2
	# financial_s[y>0]<--3
	# y<--y
	# results[,a]<-y
	results[1:n,a]<-y
	results[n+1,a]<-round(sum(km.clust$withinss),2)
}
results

summ<-rep(0,n)
for (r in 1:n){
	x<-table(results[r,])
	if (length(x)>1) {
	  y<-as.numeric(names(x)[which(x==max(x))])
	  } else y<-as.numeric(names(x))
	summ[r]<-y[1]
}

out<-by(financial_s,summ,var)
n.i<-table(summ); n.i

wss<-sum(diag(out[[1]])*(n.i[1]-1)+
           diag(out[[2]])*(n.i[2]-1)+
           diag(out[[3]])*(n.i[3]-1))
round(wss,2)

results<-cbind(results,c(summ,round(wss,2)))

colnames(results)<-c(paste("iter",1:10,sep=""),"summ")
results

km.clust3_labels<-order(unique(summ))[summ]
out<-table(cluster=km.clust3_labels,type=financial$Type)
addmargins(out)

library(mcclust)

# ?arandi
arandi(km.clust3_labels,financial$Type,adjust=F)
#                                           # Rand Index: measure
#                                           # of similarity defined 
#                                           # as the number of
#                                           # agreements
#                                           # in the choose(n,2)
#                                           # possible pairs of obs
#                                           # divided by choose(n,2)
n<-nrow(financial)
choose(n,2)
#                                           # num of possible 
#                                           # pairs of abs
arandi(km.clust3_labels,financial$Type,adjust=F)*choose(n,2)
#                                           # tot num of agreements

(1-arandi(km.clust3_labels,financial$Type,adjust=F))*choose(n,2)
#                                           # binder loss
#                                           # tot num of
#                                           # disagreements
(sum(addmargins(out)[4,1:3]^2)+
 sum(addmargins(out)[1:3,4]^2)-
 2*sum(out^2))/2
#                                           # the same

(sum(table(km.clust3_labels)^2)+
 sum(table(financial$Type)^2)-
 2*sum(table(km.clust3_labels,financial$Type)^2))/2
#                                           # the same

financial_pca<- prcomp(financial_s)
summary(financial_pca)

# pdf("../graphics/clusters_clust_pc.pdf",paper="special")
 #                                           # plot of first two PCs
plot(financial_pca$x[,1],financial_pca$x[,2],type = "n",asp=1,   
         xlab = "PC1", ylab = "PC2", main="Financial data, K=3")            
text(financial_pca$x[,1], financial_pca$x[,2],labels=company.names, 
     col = summ) 

# dev.off()

financial_pca$rotation[,1:2]

centroid<-aggregate(financial_s,by=list(summ),mean)[,-1]

# pdf("../graphics/chemical_kmean_3.pdf",paper="special")

matplot(t(centroid),type="l",col=c(1,2,4),lty=c(1,1,1),
        lwd=c(2,2,2),axes=F,
        ylab="Standardize values", xlab="variables")
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=c("-ROR5","DE","SalesGR5", "EPS5", "NPM1", "PE", "-PayoutR1"))
box()
legend("bottomright",c("clust 1","clust 2","clust 3"),
       lty=c(1,1,1),col=c(1,2,4),lwd=c(2,2,2),bty="n")

# dev.off()
#                                           # perhaps K=4?

set.seed(1)
km.clust4<- kmeans(financial_s, iter.max=100, centers=4, 
                   nstart=25, algorithm = "MacQueen")
km.clust4$cluster
x<-km.clust4$cluster
km.clust4_labels<-order(unique(x))[x]
km.clust4_labels

out<-table(cluster=km.clust4_labels,type=financial$Type)
addmargins(out)

arandi(km.clust4_labels,financial$Type,adjust=F)

n<-nrow(financial)
(1-arandi(km.clust4_labels,financial$Type,adjust=F))*choose(n,2)

km.clust4$centers
km.clust4$centers[order(unique(x)),]
centroid<-km.clust4$centers[order(unique(x)),]
centroid

# pdf("../graphics/chemical_kmean_4.pdf",paper="special")

matplot(t(centroid),type="l",col=c(1,2,3,4),lty=c(1,1,1,1),
        lwd=c(2,2,2,2),axes=F,
        ylab="Standardize values", xlab="variables",
        main="Financial data, K=4")
axis(2)
axis(1,at=1:7,las=2,cex.axis=0.7,
     labels=c("-ROR5","DE","SalesGR5", "EPS5", "NPM1", "PE", "-PayoutR1"))
box()
legend("bottomright",c("clust 1","clust 2","clust 3","clust 4"),
       lty=c(1,1,1,1),col=c(1,2,3,4),lwd=c(2,2,2,2),bty="n")

# dev.off()

# pdf("../graphics/clusters_clust_pc2.pdf",paper="special")

plot(financial_pca$x[,1],financial_pca$x[,2],type = "n",asp=1,   
         xlab = "PC1", ylab = "PC2", main="Financial data, K=4")            
text(financial_pca$x[,1],financial_pca$x[,2],labels=km.clust4_labels, 
     col = km.clust4_labels) 

# dev.off()

##############################################
##############################################

## Heart Disease data

rm(list=ls())
# clear workspace

library(mclust)

heart<-read.table("data/heartdisease.txt",
                  sep=",",head=T,row.names=1)
# data from Chp 4 ESL09

heart[1:5,]
heart<-heart[,-c(4,6)]
dim(heart)

out<-with(heart,table(chd))
out

lookup<-c("blue","orange")
names(lookup)<-as.character(0:1)
# lookup
# lookup[as.character(heart$chd)]
heart.col<-lookup[as.character(heart$chd)]

# pdf("../graphics/fig4_12.pdf",height=6,width=6)

pairs(heart[,-8],col=heart.col)

# dev.off()

hist(heart$age[heart$chd==0],main="No chd",ylab="counts",
     xlab="age",breaks = 20,col = "blue") 
 
hist(heart$age[heart$chd==1],main="No chd",ylab="counts",
     xlab="age",breaks = 20,col = "orange") 

hist(heart$age,main="Combined",ylab="counts",
     xlab="age",breaks = 20,col = "darkgreen") 

fit<-Mclust(heart$age,G=2)
summary(fit,parameters=TRUE)

plot(fit,what="classification")

table(chd=heart$chd, fit=fit$classification)

names(fit)
theta<-fit$parameters
names(theta)
theta$mean
theta$variance
sd.fit<-sqrt(theta$variance[[4]])
theta$pro

pdf("../graphics/fig6_17.pdf",height=12,width=18)

old.par <- par(no.readonly = TRUE)
par(mfrow=c(2,3),cex=1.2)

hist(heart$age[heart$chd==0],main="No chd",ylab="counts",
     xlab="age",breaks = 20,col = "blue") 

hist(heart$age[heart$chd==1],main="chd",ylab="counts",
     xlab="age",breaks = 20,col = "orange") 

hist(heart$age,main="Combined",ylab="counts",
     xlab="age",breaks = 20,col = "darkgreen") 

curve(dnorm(x,mean=theta$mean[1],sd=sd.fit[1]),
      from=min(heart$age), to=max(heart$age), ylab="mixture estimate",
      col="blue", ylim=c(0,0.10), lwd=2)

curve(dnorm(x,mean=theta$mean[2],sd=sd.fit[2]),
      from=min(heart$age), to=max(heart$age), ylab="mixture estimate",
      col="orange", ylim=c(0,0.10), lwd=2)

curve(dnorm(x,mean=theta$mean[1],sd=sd.fit[1]),
      from=min(heart$age), to=max(heart$age), ylab="mixture estimate",
      col="blue", ylim=c(0,0.10), lwd=2)
curve(dnorm(x,mean=theta$mean[2],sd=sd.fit[2]),
      add=T, col="orange", lwd=2)
f<-function(x) theta$pro[1]*dnorm(x,mean=theta$mean[1],sd=sd.fit[1])+
               theta$pro[2]*dnorm(x,mean=theta$mean[2],sd=sd.fit[2])
curve(f,add=T, col="darkgreen", lwd=2)

par(old.par)

dev.off()


BIC<-mclustBIC(heart$age)
BIC
summary(BIC)

pdf("../graphics/BIC.pdf",height=12,width=18)

old.par <- par(no.readonly = TRUE)
par(cex=2)

plot(BIC)

par(old.par)
dev.off()

fit<-Mclust(heart$age,x=BIC)


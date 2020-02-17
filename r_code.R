#import Data
world_happiness<- read.csv("Happiness_Report_Data.csv", row.names = "Country")
head(world_happiness)
#########################################################################################################################
#Data Cleaning & Visualization
# Correlation and Missing Values
#########################################################################################################################
happiness<- world_happiness[, c(-1,-2,-4, -11)] # Remove Region, Happiness Rank, Happiness Score and Standard Error for the purpose of analysis head(happiness)
happiness[happiness==0]<-NA
for(i in 1:ncol(happiness)){
  happiness[is.na(happiness[,i]), i] <- mean(happiness[,i], na.rm = TRUE) # fill with mean values of a column
}
library(corrplot)
newdata = cor(happiness[-1]) 
corrplot(newdata, method = "number")

#########################################################################################################################
#Multivariate normality test for Outlier Detection
#########################################################################################################################
xbar <- colMeans(happiness[-1])
S <- cov(happiness[-1])
d2 <- mahalanobis(happiness[-1], xbar, S) 
sd2 <-sort(d2)
quantiles <-qchisq((1:nrow(happiness[-1])-1/2)/nrow(happiness[-1]), df=ncol(happiness[-1]))
plot(quantiles, sd2,
     xlab=expression(paste(chi[3]^2,"Quantile")),
     ylab="Ordered squared distances", main="") 
abline(a=0, b=1)
text(quantiles, sd2, abbreviate(names(sd2)), col="red", pch=0.8)
#five outlier countries matched with the data-frame
outcountry <- match(lab<-c("Myanmar", "Botswana", "Rwanda", "Syria", "Qatar", "Somaliland region"), rownames(happiness))
clean_happiness_withscore <- happiness[-outcountry,]
clean_happiness <- clean_happiness_withscore[-1] #drop happiness score

#########################################################################################################################
#Multivariate normality test with clean data
#########################################################################################################################

xbar <- colMeans(clean_happiness) 
S <- cov(clean_happiness)
d2 <- mahalanobis(clean_happiness, xbar, S) 
sd2 <-sort(d2)
quantiles <-qchisq((1:nrow(clean_happiness)-1/2)/nrow(clean_happiness), df=ncol(clean_happiness))
plot(quantiles, sd2,
     xlab=expression(paste(chi[3]^2,"Quantile")),
     ylab="Ordered squared distances", main="") 
abline(a=0, b=1)
text(quantiles, sd2, abbreviate(names(sd2)), col="red", pch=0.8)

#Scatterplot of Clean Data
plot(clean_happiness) #scatterplot of clean data

#########################################################################################################################
#Dimension Reduction Analysis :PCA
#########################################################################################################################
happiness_pca<- princomp(clean_happiness, cor=T) 
summary(happiness_pca, loading=T)
# check correlation with the data
cor(clean_happiness$Economy..GDP.per.Capita., happiness_pca$scores[,1])
#PCA Biplot
biplot(happiness_pca, cex=0.6)

#########################################################################################################################
#Dimension Reduction Analysis :MDS
#########################################################################################################################
s_dist <-dist(scale(clean_happiness)) 
mydata.mds <-cmdscale(s_dist, k=2, eig=T) 
cumsum(mydata.mds$eig)/sum(mydata.mds$eig)
# MDS for observations
plot(mydata.mds$points, pch='.', xlab ="Coordinate 1", ylab="Coordinate 2") 
text(mydata.mds$points, labels=rownames(clean_happiness), cex=0.7)
# MDS for variables
dist_corr<-1-cor(clean_happiness)
mydata.mds2 <-cmdscale((dist_corr), k=3, eig=T) 
plot(mydata.mds2$points,xlim=c(-1.2, 1.2), ylim=c(-1,1), pch='.', xlab ="Coor dinate 1", ylab="Coordinate 2")
text(mydata.mds2$points, labels=colnames(clean_happiness), cex=0.7)

#########################################################################################################################
#Dimension Reduction Analysis :CCA
#########################################################################################################################
X <- scale(clean_happiness[, 1:3]) 
Y<- scale(clean_happiness[, 4:6])
library(CCA) 
cca <- cc(X, Y) 
a <- cca$xcoef
U<-cca$scores$xscores #U1 scores is the first column of xscores head(U)
V<-cca$scores$yscores #V1 scores is the first column of xscores head(V)
round(cca$cor, 3) 
a<-cca$xcoef 
a1<-a[,1]/min(a[,1])
b<-cca$ycoef 
b1<-b[,1]/min(b[,1])
#########################################################################################################################
#Cluster Analysis
#########################################################################################################################
#Hierchical-single
mydata.s <- scale(clean_happiness)
dist <- dist(mydata.s) #distance matrix
hc1 <- hclust(dist, "single")
plot(hc1, hang=-1, cex=0.5, main ="Single Linkage HC Dendogram") #dendogram
#scree plot
plot(rev(hc1$height), xlim=c(1,20)) 
ct<- cutree(hc1, 2)
table(ct)
pca <- princomp(mydata.s)
plot(pca$scores[, 1:2], col=ct) 
plot(pca$scores[, 2:3], col=ct) 
plot(pca$scores[, c(1,3)], col=ct)


#Hierchical-complete
dist <- dist(mydata.s) #distance matrix
hc1 <- hclust(dist, "complete")
plot(hc1, hang=-1, cex=0.5, main ="Complete Linkage HC Dendogram")#dendogram
#scree plot
plot(rev(hc1$height),xlim=c(1,20)) # 6 clusters, may be?
ct<- cutree(hc1, 3) 
table(ct)
pca <- princomp(mydata.s) 
plot(pca$scores[, 1:2], col=ct)
plot(pca$scores[, 2:3], col=ct) 
plot(pca$scores[, c(1,3)], col=ct)

#Hierchical-Average
dist <- dist(mydata.s) #distance matrix
hc1 <- hclust(dist, "average")
plot(hc1, hang=-1, cex=0.5, main ="Average Linkage HC Dendogram") #dendogram
#scree plot
plot(rev(hc1$height),xlim=c(1,20)) # 6 clusters, may be?
ct<- cutree(hc1, 3) 
table(ct)
pca <- princomp(mydata.s) 
plot(pca$scores[, 1:2], col=ct)
plot(pca$scores[, 2:3], col=ct) 
plot(pca$scores[, c(1,3)], col=ct)

#K-Means
km <- kmeans(mydata.s, centers=3, nstart = 10) 
plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc) 
  for (i in 1:maxc)
    wss[i] = kmeans(mydata,centers=i, nstart = 10)$tot.withinss 
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares", main="Scree Plot")
}

plot.wgss(mydata.s, 20)

pca <- princomp(mydata.s) 
plot(pca$scores[, 1:2], col=km$cluster)
plot(pca$scores[, 2:3], col=km$cluster) 
plot(pca$scores[, c(1,3)], col=km$cluster)

#Model-based
library(mclust) 
mc<-Mclust(mydata.s) 
summary(mc)
plot(mc, what="BIC") 
table(mc$classification)
clust.data = cbind(rownames(mydata.s), mc$classification, mc$uncertainty) 
clust.data[order(mc$uncertainty),]
plot(pca$scores[, 1:2], col=mc$classification) 
plot(pca$scores[, 2:3], col=mc$classification) 
plot(pca$scores[, c(1,3)], col=mc$classification)

library(maptools) 
data(wrld_simpl)
cluster1=subset(rownames(mydata.s), mc$classification==1) 
cluster2=subset(rownames(mydata.s), mc$classification==2)
cluster3=subset(rownames(mydata.s), mc$classification==3) 
cluster=c(cluster1, cluster2,cluster3)
myCountries = wrld_simpl@data$NAME %in% cluster1 
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
myCountries = wrld_simpl@data$NAME %in% cluster2 
plot(wrld_simpl, col = c(gray(.80), "green")[myCountries+1])
myCountries = wrld_simpl@data$NAME %in% cluster3 
plot(wrld_simpl, col = c(gray(.80), "yellow")[myCountries+1])


#Model-Based Discriminant Clustering
happiness1 <- clean_happiness_withscore 
happiness1$Group[happiness1$Happiness.Score< 4]<-"Low" 
happiness1$Group[happiness1$Happiness.Score>=4 & happiness1$Happiness.Score<6 ]<-"Medium"
happiness1$Group[happiness1$Happiness.Score>=6]<-"Higher"
happiness1 =happiness1[sample(1:152, 152),]
data.train = happiness1[1:120, c(-1,-8)] 
label.train = happiness1[1:120,8] 
data.test = happiness1[121:152,c(-1,-8)] 
label.test = happiness1[121:152,8]
DA <- MclustDA(data.train, label.train)
summary(DA, newdata = data.test, newclass = label.test)
#########################################################################################################################
#EFA & CFA
#########################################################################################################################EFA & CFA
#EFA
happiness_fa <- factanal(mydata.s, factors=2, scores="regression")
happiness_fa #for large dataset p value may not describe our null hypothesis.
corHat <- happiness_fa$loadings %*% t(happiness_fa$loadings) + diag(happiness_fa$uniquenesses)
corr <- cor(mydata.s)
rmse=sqrt(mean((corHat-corr)^2))

#CFA
library(lavaan)
happiness.model <- 'Public =~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy.
Personal=~ Freedom + Trust..Government.Corruption. +Generosity'
options(fit.indices = c("GFI", "AGFI", "SRMR")) # Some fit indices
fit.cfa <- cfa(happiness.model, sample.cov= cor(mydata.s), std.lv=T, sample.nobs = nrow(mydata.s))
options(fit.indices = c("GFI", "AGFI", "SRMR")) # Some fit indices summary(fit.cfa, fit.measures=TRUE)
fitMeasures(fit.cfa)[c("gfi", "agfi")]

library(semPlot)
semPaths(fit.cfa, rotation=2, 'std', 'est')

#########################################################################################################################
#END
#########################################################################################################################
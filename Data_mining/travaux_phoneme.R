library(FactoMineR)
library(DiscriMiner)
library(cluster)
library(FactoMineR)
library(NbClust)
library(HotDeckImputation)
doss<- read.csv(file.choose(), header=TRUE, sep=",",row.names=1)
data <- doss
dim(data)


library(fds)
data(aa)
plot(aa)
data(ao)
plot(ao)
data(dcl)
plot(dcl)
data(iy)
plot(iy)
data(sh)
plot(sh)



dim(table(data$speaker))
data<- order(data$speaker)

vect_train <- grep("train",data$speaker)
data_train <- data[vect_train,]
dim(data_train)


vect_test <- grep("test",data$speaker)
data_test <- data[vect_test,]
dim(data_test)

#---------------------------------------------------------
#presentation des variable 
table(data_train$g)




#---------------------------------------------------------
#traitement de données on utilise le train 
# Non supervisé , classification automatique  
y <- data_train[,257]
X <- data_train[,-c(258,257)]



dim(data_train)
table(data_train$speaker)
library(fastcluster)
cor(X)

#Mesure de ressemblance 
#Utilisation de la fonction distance : 
X.cr <- scale(X,center=TRUE,scale=TRUE)
X.d  <- dist(X.cr,method = "euc")
X.hca <- hclust(X.d,method = "ward.D2")

plot(X.hca,hang=-1,cex=0.8)

X.NbClust<-NbClust(data=X.cr,distance="euclidean",method="ward.D2")

#Classification Hierarchique 
# Ascendante hiérarchique (CAH)

#Package cluster 

X.hclust <- agnes(X,metric="euclidean",stand=TRUE,method ="ward")

#avec variation d'inertie 
X.hclust1 <- hclust(X.d,method="ward.D2")
plot(X.hclust1,main="Dendrogramme Phoneme" ,hang=-1)
abline(h=753.02198,col="green")
abline(h=326.08109,col="red")
abline(h=200.73122,col="blue")
plot(rev(X.hclust$height),type="h",ylab=" Variation inertie")

inertie <- sort(X.hclust$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3 ,5), inertie[c(2, 3,5)], col = c("green3", "red3", 
                                                "blue3"), cex = 2, lwd = 3)

#Choix du nombre de classes 
res <- NbClust(data= X.cr,distance="euclidean",method="ward.D2")
plot(res$All.index[,"index.CH"],type="b", xlab= "Number of Clusters", ylab = "Criterion")

#verification par histogramme
barplot(rev(X.hca$height)[1:50],xlab="Agg.",ylab="Delta I intra")
plot(X.hca,hang=-1,cex=0.8)

#choix de k-means 
#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(X.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss }
#graphique
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
abline(v=3,col="red")
abline(v=5,col="blue")
#(2) indice de Calinski Harabasz - utilisation du package fpc
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(X.cr,krange=2:10,criterion="ch")
#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")
abline(v=3,col="red")
abline(v=5,col="blue")

#Classification par partition avec 3 et 5 groupes
K=3
part2<-cutree(X.hca,K)
part2
table(part2)
table(data_train$g)
K=5
part3 <- cutree(X.hca,K)
part3

res.kmeans<-kmeans(X.cr,K,nstart=50,algorithm="MacQueen")
plot(res.kmeans)



source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") 
A2Rplot.hclust(X.hca, k=3, show.labels=TRUE)


#w/ Package cluster et NbClust 
tree <- X
X.agnes<-agnes(X.cr, metric = "euclidean")
si2 <- silhouette(cutree(X.agnes,k=3),daisy(X.cr), )
x11()
plot(si2, nmax=80, cex.names=0.5)

si2 <- silhouette(cutree(X.agnes,k=5),daisy(X.cr), )
plot(si2, nmax=80, cex.names=0.5)
table(si2)

HCPC(as.data.frame(scale(X),center=TRUE,scale=TRUE))

## first kmeans
iris.kmeans0 <- kmeans(X.cr,2)
## determination of the centers of the clusters
iris.centers <- aggregate(iriscr,by=list(iris.part),FUN=mean)[,-1] 
iris.kmeans <- kmeans(X.cr,iris.centers)
## comparison of the two solutions 
table(iris.kmeans0$cluster,iris.kmeans$cluster)
tree <- X
iris.kmeans$cluster
require(cluster)

X <- EuStockMarkets
kmm <- kmeans(X, 3)

D <- daisy(X)

x11()
plot(silhouette(kmm$cluster, D), col=1:3, border=NA)


iris.partition<-as.data.frame(factor(iris.kmeans$cluster)) 
colnames(iris.partition)<-"Classe" 
Species <- y 
iris.final<-data.frame(X.cr,Species) 
iris.final<-data.frame(iris.final,iris.partition) 
iris.catdes <- catdes(iris.final,num.var=257)
texte <- iris.catdes[1:40]
x11()
plot(texte)


#---------------------------------------------------------
table(y)

#Analyse supervisee 
#---- 

library(FactoMineR)
library(DiscriMiner)
library(ggplot2)

summary(data_train)

library(FactoMineR)
res.PCA <- PCA(data_train[,-258],scale.unit=TRUE,quali.sup=257,graph=FALSE)

X11()
barplot(res.PCA$eig[,2])
plot(res.PCA,choix="var",axes=c(1,2))
X11()
plot(res.PCA,choix="ind",habillage=257,cex=0.7)


y <- data_train[,257]
X <- data_train[,-c(258,257)]

library(MASS) 
library(caret)

library(AppliedPredictiveModeling)
library(lars)
library(elasticnet)

base <- data_train[,-258]

str(base)

plot(res.cv.modpls)

# computation of the Fratio and Correlation ratio for the PCA components
Z.acp <- res.PCA$ind$coord
FRatio(Z.acp[,1],y)
res.PCA$call

for (j in 1:ncol(X)) {
  print(FRatio(X[,j],y),y)
  print(corRatio(X[,j],y))
}

res.desDA <- desDA(X,y,covar="within")
summary(res.desDA)
res.desDA$power
res.desDA$discrivar
res.desDA$discor
res.desDA$values

#correlation ratio between y and the discriminant variable
#compare with PCA line 23 !
corRatio(res.desDA$scores[,1],y)

res.geoDA <- geoDA(X,y,validation="crossval")
res.geoDA$confusion #be careful mat confusion for calibration !
res.geoDA$error_rate #tx erreur en validation crois?e
predict.total <- classify(res.geoDA,X)
mat.confusion.app <- table(y,predict.total$pred_class)
round((mat.confusion.app/3340)*100,4)
tx.err.app <- 1- sum(diag(mat.confusion.app)/sum(mat.confusion.app))
tx.err.app

table(data_train$g)/3340
table(data_test$g)/1169

my_pls1 = plsDA(X, y, autosel=FALSE, cv="LKO",k=7)
#Nb composantes ? based on Q2
dim(my_pls1$components)
my_pls1$confusion
my_pls1$error_rate
### partie pls
my_pls1$Q2
my_pls1$VIP
table(y)

# test de la PLS-DA with cv to choose the number of components based on error.rate
err.rate <- vector("numeric",length=9)
for (h in 2:10) {
  my_pls1 = plsDA(X, y, autosel=FALSE, comps=h,cv="LKO",k=7)
  #my_pls1 = plsDA(X, y, autosel=FALSE, comps=k) 
  err.rate[h-1] <- my_pls1$error_rate
}
err.rate

# choix de 4 composantes
my_pls1 = plsDA(X, y, autosel=FALSE, comps=4,cv="LKO",k=7)
my_pls1$confusion
my_pls1$error_rate
my_pls1$R2
my_pls1$Q2
# plot circle of correlations
windows()
plot(my_pls1)
my_pls1$VIP


# plot factor coordinates with ggplot
data$f1 = my_pls1$components[,1]
data$f2 = my_pls1$components[,2]
data$row = substr(rownames(data),1,2)


ggplot(data=data_train, aes(x=1:257, y=258, colour=1:259)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(label=row), size=4) +
  ggtitle("Discriminant Phoneme")
########
# predire une nouvelle observation ?

learn.pred <- classify(my_pls1,X)
learn.confusion <- table(y,learn.pred$pred_class)
learn.error_rate <- 1- sum(diag(learn.confusion))/sum(learn.confusion)
learn.error_rate

round((table(data_test$g)/1169),4)*100
round((table(data_train$g)/3340),4)*100
sum(table(data_train$g))


# analyse plsda sur donnees completes avec mixOmics
library(mixOmics)
plsda_1 <- plsda(X,y, ncomp=10)
plotVar(plsda_1,comp=1:2)
plotIndiv(plsda_1, col=c("red","blue"), ellipse = TRUE, ellipse.level=0.95)
## ------------------------------------------------------------------------
# with background
background = background.predict(plsda_1, comp.predicted=2, dist = "max.dist") 
#optional: xlim = c(-40,40), ylim = c(-30,30))
plotIndiv(plsda_1, comp = 1:2,
          group = y, ind.names = FALSE, title = "Maximum distance",
          legend = TRUE,  background = background)

## ------------------------------------------------------------------------
# takes a couple of minutes to run
set.seed(2543) # for reproducibility, only when the `cpus' argument is not used
perf.plsda_1 <- perf(plsda_1, validation = "Mfold", folds = 7, 
                     progressBar = FALSE, auc = TRUE, nrepeat = 5) 

## ------------------------------------------------------------------------
# perf.plsda_1$error.rate  # error rates
windows()
plot(perf.plsda_1, ylim=c(0,1),col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")

list.keepX <- c(1:10,  seq(1, 12, 12))  # grid to change according to the number of variables and the number of components
tune.splsda_1 <- tune.splsda(X, y, ncomp = 5, validation = 'Mfold', folds = 5,
                             progressBar = FALSE, dist = 'max.dist', measure = "BER",
                             test.keepX = list.keepX, nrepeat = 10, cpus = 2)
error <- tune.splsda_1$error.rate  # error rate per component for the keepX grid
error  
## ------------------------------------------------------------------------
ncomp <- tune.splsda_1$choice.ncomp$ncomp # optimal number of components based on t-tests
ncomp

## ------------------------------------------------------------------------
select.keepX <- tune.splsda_1$choice.keepX[1:ncomp]  # optimal number of variables to select
select.keepX
# not so much variables discarded !

## ------------------------------------------------------------------------
plot(tune.splsda_1, col = color.jet(6))

## ------------------------------------------------------------------------
splsda_1 <- splsda(X, y, ncomp = ncomp, keepX = select.keepX) 

## ------------------------------------------------------------------------
plotIndiv(splsda_1, comp = c(1,2),
          group = y, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE,
          title = 'sPLS-DA on VISA, comp 1 & 2')



















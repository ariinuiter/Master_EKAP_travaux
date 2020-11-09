projet <- read.csv("SurveyS.csv", header=TRUE, sep=";", row.names = 1)

#ACP avec Y 
View(projet)
dim(projet)
str(projet)
library(FactoMineR)

save(projet)
save(projet, file = "projet.RData")
projet[,c(1:9)]=lapply(projet[,c(1:9)],as.numeric)

str(projet)


res.pca <- PCA(projet, quanti.sup = 8:9)

library("factoextra")
barplot(test$eig)


#Satif et usage se trouvent sur l'axe 2 
#On voit que l'axe 1 a une inertie de 36.08% et axe 2 une inertie 30.32% 

# correlation 
cor(projet)

#pour plus de visibilité 
library(corrplot)
m <- cor(projet)
corrplot(m, method="number")


#Choix de nombre de composantes 
library(pls)
res.pca <- PCA(projet, quanti.sup=8:9)

Y <- as.matrix(projet[,8:9])
X <- as.matrix(projet[,1:7])

res.pls<-plsr(Y~X)
summary(res.pls)

#choix des composantes
RMSEP(res.pls)
plot(RMSEP(res.pls))
#on prends les composantes les plus élevées dont 1 et 2 

scores <- res.pls$scores[,1:2]
corr <- cor(projet,scores)

plot(corr[,1],corr[,2],xlim=c(-1,1),ylim=c(-1,1), type="n")
abline(h=0,v=0)
text(corr[1:7,1],corr[1:7,2],labels=rownames(corr)[1:7],cex=1.1,col="blue")
text(corr[8:9,1],corr[8:9,2],labels=rownames(corr)[8:9],cex=1.1,col="red")
symbols(0,0,circles=1,inches=FALSE, add=TRUE)



#avec le graphique precedent, une symétrie par rapport à l'axe 1 

#qualite de prediction 
#modele 1 : avec usage
#ACP 
projet1 <- projet[,-9]
res.pca1 <- PCA(projet1, quanti.sup = 8,axes=c(1,2))
res.pca1 <- PCA(projet1, quanti.sup = 8,axes=c(1,3))

#MCO 
lm1 <- lm(Usage~Speeed+Price1+Price2+Image1+Service+Image2+Quality,data=projet)
summary(lm1)
#R^2=77.47 
prediction1 <- predict(lm1)
prediction1



#PLS2 : partial least Squares regression 
fit.pls1<- plsr(Usage~Speeed+Price1+Price2+Image1+Service+Image2+Quality,data=projet,scale=TRUE)
summary(fit.pls1)

round(explvar(fit.pls1),2)
plot(round(explvar(fit.pls1),2), type="l",main="% d'inertie de X restituée",
     xlab="composantes", ylab="% d'inertie", xaxt="n")
axis(1,1:7)
#Prendre la composante 1 et 3 
cumsum(explvar(fit.pls1))
RMSEP(fit.pls1)
plot(RMSEP(fit.pls1))

predictions1 <- predict(fit.pls1,ncomp=1)
predictions1

mse1 <- mean((projet$Usage-predictions1)^2)
mse1
rmse1 <- sqrt(mse1)
rmse1

#satifs
#PLS2 : partial least Squares regression 
fit.pls2<- plsr(Satisf~Speeed+Price1+Price2+Image1+Service+Image2+Quality,data=projet,scale=TRUE)
summary(fit.pls2)

round(explvar(fit.pls2),2)
plot(round(explvar(fit.pls2),2), type="l",main="% d'inertie de X restituée",
     xlab="composantes", ylab="% d'inertie", xaxt="n")
axis(1,1:7)
#Prendre la composante 1 et 3 
cumsum(explvar(fit.pls2))
RMSEP(fit.pls2)
plot(RMSEP(fit.pls2))

predictions2 <- predict(fit.pls2,ncomp=1)
predictions2

mse2 <- mean((projet$Satisf-predictions1)^2)
mse2
rmse2 <- sqrt(mse2)
rmse2



#ACP USAGE 
projet1 <- projet[,-9]
res.pca1 <- PCA(projet1, quanti.sup = 8,axes=c(1,2))
res.pca1$eig


#ACP satisf 
projet2 <- projet[,-8]
res.pca2 <- PCA(projet2, quanti.sup = 8,axes=c(1,2))
res.pca2$eig


#pls2 
res.pca <- PCA(projet, quanti.sup =8:9,axes=c(1,2))
res.pca$eig
pls2 <- pcr(Y~X)
summary(pls2)
RMSEP(pls2)
plot(RMSEP(pls2))

plsr2 <- plsr(Y~X)
summary(plsr2)
RMSEP(plsr2) #qualité de prédiction 
plot(RMSEP(plsr2))






#pls1 , satif
projet1 <- projet[,-8]
res.pca1 <- PCA(projet1, quanti.sup = 8,axes=c(1,2))
#satifs_acp <- PCA(projet1,quanti.sup = 8)
fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50))



plsr1 <- plsr(projet$Usage~X)
summary(plsr1)
RMSEP(plsr1) #qualité de prédiction 
plot(RMSEP(plsr1))

#pls11 

plsr11 <- plsr(projet$Satisf~X)
summary(plsr11)
RMSEP(plsr11) #qualité de prédiction 
plot(RMSEP(plsr11))





fviz_eig(res.pca11, addlabels = TRUE, ylim = c(0, 50))

res.pca1$eig



test <- PCA(projet[,-9], quanti.sup = 8,axes=c(1,2))
test1 <- PCA(projet[,-8], quanti.sup = 8,axes=c(1,2))



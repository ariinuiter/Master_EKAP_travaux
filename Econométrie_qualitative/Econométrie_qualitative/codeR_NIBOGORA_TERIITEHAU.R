########################################################################
########################################################################
########################################################################
############### Modele Binaire              ###########################
########################################################################
########################################################################
########################################################################


#choix des variables 
X <- X1
BD<- X
selection_var <- c("X..Abs.Ins","actifs","retraite",
                   "immi","X..Vot.Ins","txch","dipsup"
                   ,"agri","industrie","HLML", "cadres","CDI_pub"
                   ,"construction","artis_chef_dentrepri"
                   ,"ouvrier", "interim", "retrait"
                   , "vote", "frontiere", "depuiss","FN","vote","GGFN"
                   ,"GGEM","GGUMP","GGautre")

selection_cor <- c("X..Abs.Ins","actifs","retraite",
                   "tximmi","X..Vot.Ins","txch","dipsup"
                   ,"agri","industrie","HLML", "cadres"
                   ,"construction","artis_chef_dentrepri"
                   ,"ouvrier", "interim"
)

selection_pourcentage <- c("X..Abs.Ins","X..Vot.Ins","txch","tximmi")

selection  <- c("actifs","retraite",
                "dipsup"
                ,"agri","industrie","HLML", "cadres"
                ,"construction","artis_chef_dentrepri"
                ,"ouvrier", "interim")

boxplot(BD[,selection_pourcentage],
    horizontal=TRUE,col="brown")

boxplot(BD[,selection],
        horizontal=TRUE,col="red")

summary(BD[,selection_pourcentage])
summary(BD[,selection])



mcor <- cor(X[,selection_cor])
boxplot(X[,selection_cor])
library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


str(X)



#test selon les resultats de stepwise 
X$tximmi <- (X$immi/(BD$immi+BD$noimmi))*100

summary(X$tximmi)

#### stepwise 
modele<-glm((FN~1),data=X,family=binomial(logit)) 
modele.forward<- step(modele,scope=list(lower=~1,upper=~X..Abs.Ins+actifs+retraite
                                        +tximmi+X..Vot.Ins+txch+dipsup+agri+industrie+
                                          HLML+cadres+construction+artis_chef_dentrepri+
                                          ouvrier+interim+frontiere+depuiss+
                                          GGFN+GGEM+GGautre), 
                      data=X, direction ="forward")


modele.back1<-glm((FN~X..Abs.Ins+actifs+retraite
                   +tximmi+X..Vot.Ins+txch+dipsup+agri+industrie+
                     HLML+cadres+construction+artis_chef_dentrepri+
                     ouvrier+interim+frontiere+depuiss+
                     GGFN+GGEM+GGUMP+GGautre),data=X,family=binomial(logit)) 
modele.backward<- step(modele.back1,scope=list(lower=~1,upper=~X..Abs.Ins+actifs+retraite
                                               +tximmi+X..Vot.Ins+txch+dipsup+agri+industrie+
                                                 HLML+cadres+construction+artis_chef_dentrepri+
                                                 ouvrier+interim+frontiere+depuiss+
                                                 GGFN+GGEM+GGUMP+GGautre), 
                       data=X, direction ="backward")
summary(modele.backward)


modele.both<- step(modele,scope=list(lower=~1,upper=~X..Abs.Ins+actifs+retraite
                                     +tximmi+X..Vot.Ins+txch+dipsup+agri+industrie+
                                       HLML+cadres+construction+artis_chef_dentrepri+
                                       ouvrier+interim+frontiere+depuiss+
                                       GGFN+GGEM+GGUMP+GGautre), 
                   data=X, direction ="both")
summary(modele.both)

#resultat du stepwise : 
#modele retenu : txch + tximmi + frontiere + cadres + industrie + dipsup + 
#actifs + HLML + X..Vot.Ins + GGUMP + depuiss + interim + construction + agri + GGFN
#  construction + X..Abs.Ins

#matrice de correlation 

mcor <- cor(X[,c("txch","tximmi","ouvrier","dipsup","actifs","industrie"
                 ,"X..Vot.Ins","HLML","cadres","interim","X..Abs.Ins")])

library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)




########################################################################
########################################################################
########################################################################
############### Cluster par CAH              ###########################
########################################################################
########################################################################
########################################################################


#variable à cluster : "dipsup","actifs","HLML","cadres","interim", "retraite", "ouvrier","artis_chef_dentrepri"
#                     "X..Abs.Ins"
#on n'ajoute pas la variable txch et tximmi pour eviter d'avoir des variables dépendante 
#entre elle dans la suite de l'étude

selection_cluster <- c("dipsup","actifs","HLML","cadres",
                       "interim", "retraite", "ouvrier",
                       "artis_chef_dentrepri","X..Abs.Ins")

clust <- X[,selection_cluster]


#cluster
#----
#cluster avec HCA

library("FactoMineR") 
library("factoextra")

library(FactoMineR)
library(DiscriMiner)
library(cluster)
library(FactoMineR)
library(NbClust)
library(HotDeckImputation)


#regroupement des variables 
require(ClustOfVar)
tree<-hclustvar(X.quanti=clust)
plot(tree,main="Dendrogram of ClustOfVar")

stab<-stability(tree,B=100)
plot(stab,main="Stability of the partitions")

#stabilité a 6 classes 

P3 <- cutreevar(tree,3)
summary(P3)

cor(clust)

clust <- clust_quanti

#Mesure de ressemblance 
#Utilisation de la fonction distance : 

str(clust)
clust.cr <- scale(clust,center=TRUE,scale=TRUE)
clust.d  <- dist(clust.cr,method = "euc")
clust.hca <- hclust(clust.d,method = "ward.D2")

plot(clust.hca,hang=-1,cex=0.8)

clust.NbClust<-NbClust(data=clust.cr,distance="euclidean",method="ward.D2")
#l'algo propose 3 catégorie 
plot(clust.NbClust,main="Dendrogramme Phoneme" ,hang=-1)

#Classification Hierarchique 
# Ascendante hiérarchique (CAH)

#Package cluster 

clust.hclust <- agnes(clust,metric="euclidean",stand=TRUE,method ="ward")

#avec variation d'inertie 
clust.hclust1 <- hclust(clust.d,method="ward.D2")
plot(clust.hclust1,main="Dendrogramme Phoneme" ,hang=-1)
plot(rev(clust.hclust$height),type="h",ylab=" Variation inertie")

inertie <- sort(clust.hclust$height, decreasing = TRUE)


plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3 ,5), inertie[c(2, 3,5)], col = c("green3", "red3", 
                                               "blue3"), cex = 2, lwd = 3)

#Choix du nombre de classes 

plot(clust.NbClust$All.index[,"index.CH"],type="b", xlab= "Number of Clusters", ylab = "Criterion")

#verification par histogramme
barplot(rev(clust.hca$height)[1:50],xlab="Agg.",ylab="Delta I intra")
plot(clust.hca,hang=-1,cex=0.8)

#choix de k-means 
#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(clust.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss }
#graphique
X11()
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
abline(v=2,col="red")
abline(v=3,col="blue")
#(2) indice de Calinski Harabasz - utilisation du package fpc
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(clust.cr,krange=2:10,criterion="ch")
#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")
abline(v=2,col="red")
abline(v=3,col="blue")

#Classification par partition avec 2 groupes
K=2
part2<-cutree(clust.hca,K)
part2
table(part2)

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") 
A2Rplot.hclust(clust.hca, k=2, show.labels=TRUE)

str(X)
HCPC(as.data.frame(scale(clust),center=TRUE,scale=TRUE))

table(X1$cat1)
table(X1$cat2)

#----

#combiner les nouvelles catégorie avec la base initiale 
#----
X1 <- cbind(X,part2)


X1$cat1 <- 0 
X1$cat1[X1$part2 ==1] <- 1

X1$cat2 <- 0 
X1$cat2[X1$part2 ==2] <- 1


X1$cat1 <- as.factor(X1$cat1)
X1$cat2 <- as.factor(X1$cat2)

table(X1$cat1)
table(X1$cat2)
#-----


#catégoriser la variables txch et tximmi
#-----
quantile.cut <- function(x, n=3,...)
{
  cut(x,
      breaks=quantile(x, seq(from=0, to=1, by=1/n)),
      include.lowest = TRUE,...)
}

X1$cattxch <- quantile.cut(X1$txch)
X1$catimmi <- quantile.cut(X1$immi)
X1$cattximmi <- quantile.cut(X1$tximmi)

quantile.cut <- function(x, n=2,...)
{
  cut(x,
      breaks=quantile(x, seq(from=0, to=1, by=1/n)),
      include.lowest = TRUE,...)
}

X1$cattxch1 <- quantile.cut(X1$txch)
X1$cattximmi1 <- quantile.cut(X1$tximmi)

str(X1$cattxch3 )

#----
#estimation avec fonction glm 
modele1<-glm((FN~1),data=X1,family=binomial(logit)) 
modele.forward1<- step(modele1,scope=list(lower=~1,upper=~txch + tximmi + frontiere + 
                                            X..Vot.Ins + GGUMP + depuiss + GGFN+ cat1), 
                       data=X1, direction ="forward")

modele.both<- step(modele1,scope=list(lower=~1,upper=~txch + tximmi + frontiere + 
                                        X..Vot.Ins + GGUMP + depuiss + GGFN+ cat1), 
                   data=X, direction ="both")

modele1<-glm((FN~ txch + tximmi + frontiere + 
                X..Vot.Ins + GGFN + cat1 + depuiss),data=X1,family=binomial(logit))
summary(modele1)




#test de dépendance : FN ~ txch + tximmi + frontiere + X..Vot.Ins + GGFN + cat1 + depuiss
#----
cor(X1$txch,X1$tximmi) #ok
cor(X1$txch,X1$X..Vot.Ins) #ok 
cor(X1$tximmi,X1$X..Vot.Ins) #ok 

chisq.test(X1$frontiere,X1$depuiss)
chisq.test(X1$frontiere,X1$GGFN)
chisq.test(X1$frontiere,X1$cat1)#ok

chisq.test(X1$depuiss,X1$GGFN)
chisq.test(X1$depuiss,X1$cat1)

chisq.test(X1$GGFN,X1$cat1)


t.test(X1$txch~X1$cat1) 
t.test(X1$txch~X1$frontiere)
t.test(X1$txch~X1$depuiss) #ok
t.test(X1$txch~X1$GGFN)

t.test(X1$tximmi~X1$cat1) 
t.test(X1$tximmi~X1$frontiere)
t.test(X1$tximmi~X1$depuiss) #ok 
t.test(X1$tximmi~X1$GGFN)

t.test(X1$X..Vot.Ins~X1$cat1)
t.test(X1$X..Vot.Ins~X1$frontiere)
t.test(X1$X..Vot.Ins~X1$depuiss) #ok 
t.test(X1$X..Vot.Ins~X1$GGFN)
#attention lors de l'estimation 
#----


cor(X1$txch,X1$immi)
cor(X1$txch,X1$X..Vot.Ins)
cor(X1$immi,X1$X..Vot.Ins)

#modele1
#----
library(car)
vif(modele1)

chi2<- (modele1$null.deviance-modele1$deviance) 
ddl<-modele1$df.null-modele1$df.residual 
pvalue<-pchisq(chi2,ddl,lower.tail=F) 
print(pvalue)
#le modele a un interet car p < 5% 

exp(coef(modele1))
#on peut tout interpreter les variables car elles sont
#toutes significatives 


#calcul des effets marginaux 
mean(dlogis(predict(modele1,type="link")))*coef(modele1)


#tableau de prévision et pourcentage d'erreur du modeles estime 
pred.proba<-predict(modele1,type="response") 
pred.moda<-factor(ifelse(pred.proba>0.5,"1","0")) 
mc<-table(X1$FN,pred.moda)
print(mc)

err<-(mc[2,1]+mc[1,2])/sum(mc) 
print(err)
#erreur est 33% 

Sensibilite<-mc[2,2]/(mc[2,1]+mc[2,2]) 
print(Sensibilite)

Specificite<-mc[1,1]/(mc[1,1]+mc[1,2]) 
print(Specificite)

library(pscl) 
hitmiss(modele1)
#le modele predit correctement avec une valeur 58% et 74% 

R2_Mc_Fadden<-1-(modele1$deviance/modele1$null.deviance) 
R2_Mc_Fadden
#16% qualité faible 

X11()
plot(rstudent(modele1),type="p",cex=0.5,ylab="Residus") 
abline(h=c(-2,2))

#il ya des valeurs qui influence de manieres significative l'estimation realisé
X_iden <- X1
resid <- rstudent(modele1)

X_iden <- cbind(X_iden,resid)
X_iden$resid
base_resid2<- X_iden[ X_iden$resid<=2  & X_iden$resid>= -2 ,]
#----


#modele 1bis sans observation influencant 
#----

modele1bis<-glm((FN~ txch + tximmi + frontiere + 
                   X..Vot.Ins + GGFN + cat1 + depuiss),data=base_resid2,family=binomial(logit))
summary(modele1bis)


library(car)
vif(modele1bis)

chi2<- (modele1bis$null.deviance-modele1bis$deviance) 
ddl<-modele1bis$df.null-modele1bis$df.residual 
pvalue<-pchisq(chi2,ddl,lower.tail=F) 
print(pvalue)
#le modele a un interet car p < 5% 

exp(coef(modele1bis))
#on peut tout interpreter les variables car elles sont
#toutes significatives 


#calcul des effets marginaux 
mean(dlogis(predict(modele1bis,type="link")))*coef(modele1bis)


#tableau de prévision et pourcentage d'erreur du modeles estime 
pred.proba<-predict(modele1bis,type="response") 
pred.moda<-factor(ifelse(pred.proba>0.5,"1","0")) 
mc<-table(base_resid2$FN,pred.moda)
print(mc)

err<-(mc[2,1]+mc[1,2])/sum(mc) 
print(err)
#erreur est 33% 

Sensibilite<-mc[2,2]/(mc[2,1]+mc[2,2]) 
print(Sensibilite)

Specificite<-mc[1,1]/(mc[1,1]+mc[1,2]) 
print(Specificite)

library(pscl) 
hitmiss(modele1bis)
#le modele predit correctement avec une valeur 58% et 74% 

R2_Mc_Fadden<-1-(modele1bis$deviance/modele1bis$null.deviance) 
R2_Mc_Fadden
#1% qualité faible 

plot(rstudent(modele1bis),type="p",cex=0.5,ylab="Residus") 
abline(h=c(-2,2))
#----



#modele_test<-glm((FN~ FN~ txch + tximmi + frontiere + 
#               X..Vot.Ins + GGFN + cat1 + depuiss),data=X1,family=binomial(logit))

#modele 2 FN~txch + tximmi + X..Vot.Ins + Depuiss
#----
modele2<-glm((FN~txch + tximmi + X..Vot.Ins + depuiss),data=X1,family=binomial(logit))
summary(modele2)
library(car)
vif(modele2)

chi2<- (modele2$null.deviance-modele2$deviance) 
ddl<-modele2$df.null-modele2$df.residual 
pvalue<-pchisq(chi2,ddl,lower.tail=F) 
print(pvalue)
#le modele a un interet car p < 5% 

exp(coef(modele2))
#on peut tout interpreter les variables car elles sont
#toutes significatives 


#calcul des effets marginaux 
mean(dlogis(predict(modele2,type="link")))*coef(modele2)


#tableau de prévision et pourcentage d'erreur du modeles estime 
pred.proba<-predict(modele2,type="response") 
pred.moda<-factor(ifelse(pred.proba>0.5,"1","0")) 
mc<-table(X1$FN,pred.moda)
print(mc)

err<-(mc[2,1]+mc[1,2])/sum(mc) 
print(err)
#erreur est 33% 

Sensibilite<-mc[2,2]/(mc[2,1]+mc[2,2]) 
print(Sensibilite)

Specificite<-mc[1,1]/(mc[1,1]+mc[1,2]) 
print(Specificite)

library(pscl) 
hitmiss(modele2)
#le modele predit correctement avec une valeur 58% et 74% 

R2_Mc_Fadden<-1-(modele2$deviance/modele2$null.deviance) 
R2_Mc_Fadden
#16% qualité faible 

X11()
plot(rstudent(modele2),type="p",cex=0.5,ylab="Residus") 
abline(h=c(-2,2))
#il ya des valeurs qui influent de manieres significative l'estimation realisé
#------


#modele 3
#-----

#modele 3 FN~cat1 + frontiere

modele3<-glm((FN~cat1 + frontiere),data=X1,family=binomial(logit))
summary(modele3)
library(car)
vif(modele3)

chi2<- (modele3$null.deviance-modele3$deviance) 
ddl<-modele3$df.null-modele3$df.residual 
pvalue<-pchisq(chi2,ddl,lower.tail=F) 
print(pvalue)
#le modele a un interet car p < 5% 

exp(coef(modele3))
#on peut tout interpreter les variables car elles sont
#toutes significatives 


#calcul des effets marginaux 
mean(dlogis(predict(modele3,type="link")))*coef(modele3)


#tableau de prévision et pourcentage d'erreur du modeles estime 
pred.proba<-predict(modele3,type="response") 
pred.moda<-factor(ifelse(pred.proba>0.5,"1","0")) 
mc<-table(X1$FN,pred.moda)
print(mc)

err<-(mc[2,1]+mc[1,2])/sum(mc) 
print(err)
#erreur est 33% 

Sensibilite<-mc[2,2]/(mc[2,1]+mc[2,2]) 
print(Sensibilite)

Specificite<-mc[1,1]/(mc[1,1]+mc[1,2]) 
print(Specificite)

library(pscl) 
hitmiss(modele3)
#le modele predit correctement avec une valeur 58% et 74% 

R2_Mc_Fadden<-1-(modele3$deviance/modele3$null.deviance) 
R2_Mc_Fadden
#16% qualité faible 

X11()
plot(rstudent(modele3),type="p",cex=0.5,ylab="Residus") 
abline(h=c(-2,2))
#il n' ya pas des valeurs qui influencent de manieres significative l'estimation realisé
#----

#modele1bis



#Effet du taux de chomage en fonction de la 
#commune si proche d'une frontiere et departement ayant une grande ville sur la probabilite de
#voter FN 
#----
library(visreg)
visreg(modele3, "cat1", by="frontiere",scale="response", main="En fonction de la proximité de la commune 
       à une frontière")

#----

#visreg
#----
visreg(modele2, "txch", by="depuiss",scale="response", main="En fonction d'une commune se trouvant 
      dans un département ayant une grande ville")

visreg(modele2, "tximmi", by="depuiss",scale="response", main="En fonction d'une commune se trouvant 
      dans un département ayant une grande ville")

visreg(modele2, "X..Vot.Ins", by="depuiss",scale="response", main="En fonction d'une commune se trouvant 
      dans un département ayant une grande ville")


#----



#heterosce modele 1 
#----
library(glmx)

modeleh <- hetglm(FN~ txch + tximmi + frontiere + X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch + tximmi + frontiere + X..Vot.Ins + GGFN + cat1 + depuiss,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)


modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1   
                  ,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch + tximmi + frontiere +
                    X..Vot.Ins  + cat1   
                  ,data=X1,family=binomial(logit) )
summary(modeleh)
vif(modeleh)

modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch  + frontiere +
                    X..Vot.Ins  + cat1   
                  ,data=X1,family=binomial(logit) )
summary(modeleh)
vif(modeleh)

modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch  + frontiere
                  + cat1   
                  ,data=X1,family=binomial(logit) )
summary(modeleh)
vif(modeleh)

modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch  + frontiere
                  
                  ,data=X1,family=binomial(logit) )
summary(modeleh)
vif(modeleh)

#l'Hmscedasticite des erreurs doit être refuser car inférieure à 1% 


h1c <- hetglm(FN~1,data=X1,family=binomial(logit)) 
(R2McFAdden<-1-(modeleh$loglik/h1c$loglik))



h1h <- hetglm(FN~txch + tximmi + frontiere +
                X..Vot.Ins + GGFN + cat1 + depuiss| 1 
              ,data=X1,family=binomial(logit) )
summary(h1h)

library(lmtest) 
lrtest(h1h,modeleh)
# Au seuil de risque de 1% (et donc de 5%), le modèle modeleh (prise en compte de l’hé téroscédasticité des erre
#urs) est préféré au modèle h1h (modèle où les erreurs sont supp osées homoscédast
#iques). Il faut donc conserver le modèle

#----

#heterosce modele 2 
#----
library(glmx)

modeleh <- hetglm(FN~ txch + tximmi  + X..Vot.Ins + depuiss|
                    txch + tximmi  + X..Vot.Ins + depuiss,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

modeleh <- hetglm(FN~ txch + tximmi  + X..Vot.Ins + depuiss|
                    txch + tximmi  + X..Vot.Ins ,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

modeleh <- hetglm(FN~ txch + tximmi  + X..Vot.Ins + depuiss|
                    tximmi   ,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

#l'Hmscedasticite des erreurs doit être refuser car inférieure à 1% 


h1c <- hetglm(FN~1,data=X1,family=binomial(logit)) 
(R2McFAdden<-1-(modeleh$loglik/h1c$loglik))



h1h <- hetglm(FN~txch + tximmi + frontiere +
                X..Vot.Ins + GGFN + cat1 + depuiss| 1 
              ,data=X1,family=binomial(logit) )
summary(h1h)
library(lmtest) 
lrtest(h1h,modeleh)

#p_value < 5% 

#----

#heterosce modele 3
#----
library(glmx)

modeleh <- hetglm(FN~ frontiere + cat1 |
                    frontiere + cat1 ,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

library(glmx)

modeleh <- hetglm(FN~ frontiere + cat1 |
                    frontiere ,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

modeleh <- hetglm(FN~ frontiere + cat1 |
                    cat1 ,data=X1,family=binomial(logit) )
summary(modeleh)

vif(modeleh)


h1c <- hetglm(FN~1,data=X1,family=binomial(logit)) 
(R2McFAdden<-1-(modeleh$loglik/h1c$loglik))


library(lmtest) 
lrtest(h1h,modeleh)


#----

#heterosce modele1bis
#----

library(glmx)

modeleh <- hetglm(FN~ txch + tximmi + frontiere + X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch + tximmi + frontiere + X..Vot.Ins + GGFN + cat1 + depuiss,data=base_resid2,family=binomial(logit) )
summary(modeleh)

vif(modeleh)


modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    txch + tximmi + frontiere +
                    X..Vot.Ins  
                  ,data=base_resid2,family=binomial(logit) )
summary(modeleh)

vif(modeleh)

modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    tximmi + frontiere+ X..Vot.Ins
                  
                  ,data=base_resid2,family=binomial(logit) )
summary(modeleh)
vif(modeleh)


modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    X..Vot.Ins+frontiere
                  
                  ,data=base_resid2,family=binomial(logit) )
summary(modeleh)
vif(modeleh)

modeleh <- hetglm(FN~txch + tximmi + frontiere +
                    X..Vot.Ins + GGFN + cat1 + depuiss|
                    frontiere
                  
                  ,data=base_resid2,family=binomial(logit) )
summary(modeleh)
vif(modeleh)

#l'Hmscedasticite des erreurs doit être refuser car inférieure à 1% 


h1c <- hetglm(FN~1,data=base_resid2,family=binomial(logit)) 
(R2McFAdden<-1-(modeleh$loglik/h1c$loglik))



h1h <- hetglm(FN~txch + tximmi + frontiere +
                X..Vot.Ins + GGFN + cat1 + depuiss| 1 
              ,data=base_resid2,family=binomial(logit) )
summary(h1h)

library(lmtest) 
lrtest(h1h,modeleh)
# Au seuil de risque de 1% (et donc de 5%), le modèle modeleh (prise en compte de l’hé téroscédasticité des erre
#urs) est préféré au modèle h1h (modèle où les erreurs sont supp osées homoscédast
#iques). Il faut donc conserver le modèle

#----

#----



########################################################################
########################################################################
########################################################################
############### Modele multinomiaux ordonnée ###########################
########################################################################
########################################################################
########################################################################



#ordonnes la variable multi ordonnée
#----
library(tidyverse)
X1$vote <- fct_relevel(X1$vote, "0","UMP","EM","FN")
table(X1$vote)
X1$vote<-ordered(X1$vote)
#----


table(X1$vote)
library(MASS) 
modelord<-polr(vote~txch + tximmi + frontiere +
                 X..Vot.Ins + GGFN + cat1 + depuiss
               ,data=X1, method=c("logistic"))
ctable<-coef(summary(modelord))
p<-pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2
p2<-round(p,4)
(ctable<-cbind(ctable,pvalue=p2))

step(modelord, direction='forward', criterion='AIC') 
step(modelord, direction='backward', criterion='AIC') 
step(modelord, direction='both', criterion='AIC')


#voir quelle modele choisir pour le modele multinominal
#modele retenu : vote ~ txch + tximmi + frontiere


#modele 1
#----
modelord<-polr(vote ~ txch + tximmi + frontiere  
               ,data=X1, method=c("logistic"))
ctable<-coef(summary(modelord))
p<-pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2
p2<-round(p,4)
(ctable<-cbind(ctable,pvalue=p2))

modelord0<-polr(vote~1,data=X1,method=c("logistic")) 
summary(modelord0)
require(lmtest)

lrtest(modelord,modelord0)

exp(coef(modelord))

R2_Mc_Fadden<-1-(modelord$deviance/modelord0$deviance) 
R2_Mc_Fadden
#r^2 a 7% tres faible  

X1$predict.m <- predict(modelord) 
(mc<-table(X1$predict.m , X1$vote))

qualite<-((mc[1,1]+mc[2,2]+mc[3,3]+mc[4,4])/sum(mc))*100 
print(qualite)
#qualite a 55%

#----


library(effects) 
plot(Effect(c("frontiere"),modelord))
plot(Effect(c("tximmi"),modelord))
plot(Effect(c("txch"),modelord))
library(VGAM)

table(X1$vote)
X1$vote1[X1$vote == "FN"] <- 4
X1$vote1[X1$vote == "EM"] <- 3
X1$vote1[X1$vote == "UMP"] <- 2
X1$vote1[X1$vote == "0"] <- 1

X1$vote1 <- as.numeric(X1$vote1)

#modele1 
#----
fitmodel<-vglm(vote1~ txch + tximmi + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))


fitmodel2<-vglm(vote1~ txch + tximmi + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE,reverse=TRUE))
summary(fitmodel2)


1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))
#----

#verification des effet des variables sur Y 

table(X1$vote)
X1$vote3[X1$vote == "FN"] <- "FN"
X1$vote3[X1$vote == "EM"] <- "EM"
X1$vote3[X1$vote == "UMP"] <- "0"
X1$vote3[X1$vote == "0"] <- 1
table(X1$vote3)

X1$vote4[X1$vote == "FN"] <- 3
X1$vote4[X1$vote == "EM"] <- 2
X1$vote4[X1$vote == "UMP"] <- 1
X1$vote4[X1$vote == "0"] <- 1
table(X1$vote4)
X1$vote1 <- as.numeric(X1$vote1)


X1$vote4 <- as.numeric(X1$vote4)
str(X1$vote4)

X1$vote3 <- as.factor(X1$vote3)
model1<-polr(vote~ cattxch + cattximmi + frontiere,data=X1,method=c("logistic"))
X11()
plot(Effect(c("cattxch","cattximmi","frontiere"),model1))
table(X1$vote)

# ne fonctionne pas , il faut repenser l'ensemble des variables 
# on propose d'utiliser des variables 
table(X1$cattxch)
table(X1$cattximmi)

X1$cattximmi 
X1$
table(X1$cattxch0)
table(X1$cattxch1)

#verifie l'impact significative des variables 
modelord<-polr(vote3 ~ cattxch0+ cattxch1 + catximmi0+ catximmi1 + frontiere  
               ,data=X1, method=c("logistic"))
ctable<-coef(summary(modelord))
p<-pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2
p2<-round(p,4)
(ctable<-cbind(ctable,pvalue=p2))

modelord0<-polr(vote3~1,data=X1,method=c("logistic")) 
summary(modelord0)
exp(coef(modelord))

R2_Mc_Fadden<-1-(modelord$deviance/modelord0$deviance) 
R2_Mc_Fadden

X1$predict.m <- predict(modelord) 
(mc<-table(X1$predict.m , X1$vote3))
qualite<-((mc[1,1]+mc[2,2]+mc[3,3])/sum(mc))*100 
print(qualite)
require(lmtest)
lrtest(modelord,modelord0)


library(effects) 
plot(Effect(c("cattxch0"),modelord))
plot(Effect(c("cattxch1"),modelord))
plot(Effect(c("catximmi0"),modelord))
plot(Effect(c("catximmi1"),modelord))
plot(Effect(c("frontiere"),modelord))
X11()
plot(Effect(c("cattxch1","frontiere"),modelord))


summary(modelord0)
step(modelord, direction='forward', criterion='AIC') 
step(modelord, direction='backward', criterion='AIC') 
step(modelord, direction='both', criterion='AIC')

require(lmtest)

lrtest(modelord,modelord0)


library(VGAM)
X1$catximmi0 <- 0
X1$catximmi0[X1$cattximmi =="(1.39,2.76]"]<-1
X1$catximmi1 <- 0
X1$catximmi1[X1$cattximmi =="(2.76,20.5]"]<-1

X1$cattxch0 <- 0
X1$cattxch0[X1$cattxch =="(11.1,14.7]"]<-1
X1$cattxch1 <- 0
X1$cattxch1[X1$cattxch =="(14.7,41.9]"]<-1
table(X1$cattxch0)
table(X1$cattxch1)

#Fonction vglm avec les nouvelles variables 
#----
#modele1 avec une categorisation des variables txch et tximmi
fitmodel<-vglm(vote4~ cattxch0+cattxch1 + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))


fitmodel2<-vglm(vote4~ cattxch0+cattxch1  + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE,reverse=TRUE))

1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))

#verification des pentes selon les variables 
#pour la variable cattxch0 
fitmodel<-vglm(vote4~ cattxch0+cattxch1  + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))
fitmodel2<-vglm(vote4~ cattxch0+cattxch1  + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE~+1+cattxch0,reverse=TRUE))
1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))
#non

#pour la variable cattxch1 
fitmodel<-vglm(vote4~ cattxch0+cattxch1  + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))
fitmodel2<-vglm(vote4~ cattxch0+cattxch1  + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE~+1+cattxch1,reverse=TRUE))
1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))
#non


#pour la variable catimmi0
fitmodel<-vglm(vote4~ cattxch0+cattxch1 + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))
fitmodel2<-vglm(vote4~ cattxch0+cattxch1 + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE~1+catximmi0,reverse=TRUE))
1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))
#oui

#pour la variable catimmi1
fitmodel<-vglm(vote4~ cattxch0+cattxch1 + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))
fitmodel2<-vglm(vote4~ cattxch0+cattxch1 + catximmi0+ catximmi1 + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE~1+catximmi1,reverse=TRUE))
1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))
#oui

#pour la variable frontiere
fitmodel<-vglm(vote4~ cattxch0+cattxch1 + cattximmi + frontiere  ,data=X1,link="logit", 
               family=cumulative(parallel=TRUE,reverse=TRUE))
fitmodel2<-vglm(vote4~ cattxch0+cattxch1 + cattximmi + frontiere  ,data=X1,link="logit", 
                family=cumulative(parallel=FALSE~1+frontiere,reverse=TRUE))
1-pchisq(deviance(fitmodel)-deviance(fitmodel2),df=df.residual(fitmodel)- 
           df.residual(fitmodel2))
#non

#conlusion : il n'y a que les variables catximmi0+ catximmi1 qui vérifient l'égalité des pentes 

#re-vérifions l'egalite des pentes 

fitmodelbis<-vglm(vote4~   catximmi0+ catximmi1   ,data=X1,link="logit", family=cumulative(parallel=TRUE,reverse=TRUE))
summary(fitmodelbis)
fitmodel2bis<-vglm(vote4~ catximmi0+ catximmi1  ,data=X1,link="logit", family=cumulative(parallel=FALSE,reverse=TRUE))
summary(fitmodel2bis)
1-pchisq(deviance(fitmodelbis)-deviance(fitmodel2bis),df=df.residual(fitmodelbis)- df.residual(fitmodel2bis))
#alors elle vérifie  l'egalite des pentes au seuil de 5% 

fit0 <- vglm(vote4~ 1, data=X1, link="logit", family = cumulative(parallel=TRUE, reverse=TRUE))
print(pseudo_R2 <- 1 - deviance(fitmodelbis) / deviance(fit0))



table(X1$cattximmi)

ooo <- with(X1, order(catximmi0))
fitted(fitmodelbis)[ooo,]
with(X1, matplot(catximmi0[ooo], fitted(fitmodelbis)[ooo,], ylim = c(0,1),
                 xlab = "catximmi0", ylab = "Probabilité", las = 1,
                 main = "Effet de la catximmi0 sur l'appartenance aux catégories", type = "l", lwd = 1))
legend("topright",col = c("black","red","green"), lty=1:3, legend=colnames(fitted(fitmodelbis)))

ooo <- with(X1, order(catximmi1))
fitted(fitmodelbis)[ooo,]
with(X1, matplot(catximmi1[ooo], fitted(fitmodelbis)[ooo,], ylim = c(0,1),
                 xlab = "catximmi1", ylab = "Probabilité", las = 1,
                 main = "Effet de la catximmi1 sur l'appartenance aux catégories", type = "l", lwd = 1))
legend("topright",col = c("black","red","green"), lty=1:3, legend=colnames(fitted(fitmodelbis)))

#----


#----


#effet marginaux au niveau moyen 
library(oglmx)

results.oprob<-oglmx(vote4~catximmi0+ catximmi1 , data=X1
                     ,link="logit", constantMEAN=FALSE, constantSD=FALSE, delta=0)
summary(results.oprob)
margins.oglmx(results.oprob, atmeans=TRUE, ascontinuous=FALSE)
margins.oglmx(results.oprob, atmeans=TRUE)



results.oprobhet<-oglmx(vote4~catximmi0+ catximmi1 ,~ catximmi0+catximmi1, data=X1, link="logit", constantMEAN=FALSE, constantSD=FALSE) 
summary(results.oprobhet)



library(lmtest) 
lrtest(results.oprob,results.oprobhet)
#p_value >5%



#Au seuil de risque de 1%, le modèle avec prise en compte de l’hétéroscédastic
#ité des erreurs est meilleur que celui supposant l’homoscédasticité des erre




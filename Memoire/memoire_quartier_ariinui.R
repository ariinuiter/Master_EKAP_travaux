load("Base_quartier.RData")
#test de significativité des moyennes de groupes 
#----
#selection des bases uniquement pour les années 2009 et 2014
#inserer dans un tableau les test des égalités des moyennes 
BD9<- BD[BD$periode==0,]
BD14<- BD[BD$periode==1,]

#Egalite des moyennes avec t-stat 
str(BD$tram)
test <- t.test(BD$'TXCHOM1564'~BD$tram)
test$p.value
BD9$TXCHOM5564
t.test(BD$'TXCHOM2554'~BD$tram)

#vecteur des variables à tester 
vect_ttest <- c('TXCHOM1564','TXCHOM1524','TXCHOM2554','TXCHOM5564',
                'TXACT', 'TXFEM','TX1524','C14_MENFAMMONO','P14_NSCOL15P_DIPLMIN',
                'P14_NSCOL15P_SUP', 'C14_ACTOCC1564_CS1','C14_ACTOCC1564_CS2',
                'C14_ACTOCC1564_CS3','C14_ACTOCC1564_CS4','C14_ACTOCC1564_CS5',
                'C14_ACTOCC1564_CS6','P14_RP_LOCHLMV','P14_RP_PROP','P14_RP_LOC',
                'C14_ACTOCC15P_TCOM','C14_ACTOCC15P_VOIT','P14_ACTOCC15P_ILT1')

BD_ttest9 <- BD9[vect_ttest]

#initialisation des vecteurs 
vect_moy_tram <- rep(0,length(vect_ttest))
vect_moy_notram <- rep(0, length(vect_ttest))
vect_sd_tram <- rep(0, length(vect_ttest))
vect_sd_notram <- rep(0, length(vect_ttest))
vect_pvalue0 <- rep(0, length(vect_ttest))
vect_pvalue <- rep('', length(vect_ttest))


for (i in 1:length(vect_ttest)){
  vect_moy_tram[i]<- round(mean(BD9[BD9$tram==0,vect_ttest[i]]),3)*100
  vect_moy_notram[i] <- round(mean(BD9[BD9$tram==1,vect_ttest[i]]),3)*100
  vect_sd_tram[i] <- round(sd(BD9[BD9$tram==0,vect_ttest[i]]),4)
  vect_sd_notram[i] <- round(sd(BD9[BD9$tram==1,vect_ttest[i]]),4)
  test <- t.test(BD9[,vect_ttest[i]]~BD9$tram)
  vect_pvalue0[i] <- round(test$p.value,2)
  if(vect_pvalue0[i]<0.1){
    if(vect_pvalue0[i]<0.05){
      if(vect_pvalue0[i]<0.01)
      {vect_pvalue[i]='***'}
      else{vect_pvalue[i]='**'}
      
    }
    else{vect_pvalue[i]='*'}
  }
  
}

EGA09 <-data.frame(vect_ttest,vect_moy_tram,vect_sd_tram,vect_moy_notram
                   ,vect_sd_notram,vect_pvalue0,vect_pvalue)
EGA09

##2014
BD_ttest14 <- BD14[vect_ttest]

vect_moy_tram <- rep(0,length(vect_ttest))
vect_moy_notram <- rep(0, length(vect_ttest))
vect_sd_tram <- rep(0, length(vect_ttest))
vect_sd_notram <- rep(0, length(vect_ttest))
vect_pvalue0 <- rep(0, length(vect_ttest))
vect_pvalue <- rep('', length(vect_ttest))


for (i in 1:length(vect_ttest)){
  vect_moy_tram[i]<- round(mean(BD14[BD14$tram==0,vect_ttest[i]]),3)*100
  vect_moy_notram[i] <- round(mean(BD14[BD14$tram==1,vect_ttest[i]]),3)*100
  vect_sd_tram[i] <- round(sd(BD14[BD14$tram==0,vect_ttest[i]]),4)
  vect_sd_notram[i] <- round(sd(BD14[BD14$tram==1,vect_ttest[i]]),4)
  test <- t.test(BD14[,vect_ttest[i]]~BD14$tram)
  vect_pvalue0[i] <- round(test$p.value,2)
  if(vect_pvalue0[i]<0.1){
    if(vect_pvalue0[i]<0.05){
      if(vect_pvalue0[i]<0.01)
      {vect_pvalue[i]='***'}
      else{vect_pvalue[i]='**'}
      
    }
    else{vect_pvalue[i]='*'}
  }
  
}

EGA14 <-data.frame(vect_ttest,vect_moy_tram,vect_sd_tram,vect_moy_notram
                   ,vect_sd_notram,vect_pvalue0,vect_pvalue)
EGA14



#Moyenne pour l'échantillon globale 2014 et 2009 
vect_moy_tram <- rep(0,length(vect_ttest))
vect_sd_tram <- rep(0, length(vect_ttest))

for (i in 1:length(vect_ttest)){
  vect_moy_tram[i]<- round(mean(BD14[,vect_ttest[i]]),3)*100
  
  vect_sd_tram[i] <- round(sd(BD14[,vect_ttest[i]]),4)
  
}

ANGERS14 <-data.frame(vect_ttest,vect_moy_tram,vect_sd_tram)
ANGERS14


vect_moy_tram <- rep(0,length(vect_ttest))
vect_sd_tram <- rep(0, length(vect_ttest))

for (i in 1:length(vect_ttest)){
  vect_moy_tram[i]<- round(mean(BD9[,vect_ttest[i]]),3)*100
  
  vect_sd_tram[i] <- round(sd(BD9[,vect_ttest[i]]),4)
  
}

ANGERS9 <-data.frame(vect_ttest,vect_moy_tram,vect_sd_tram)
ANGERS9


library("xlsx")
write.xlsx(EGA14, file="tab_ega_14.xlsx", 
           sheetName="feuille1")
write.xlsx(EGA09, file="tab_ega_09.xlsx", 
           sheetName="USA feuille1")

write.xlsx(ANGERS9, file="tab_ega_ANGERS09.xlsx", 
           sheetName="USA feuille1")
write.xlsx(ANGERS14, file="tab_ega_ANGERS14.xlsx", 
           sheetName="USA feuille1")
#----




#choix des variables avec stepwise 

BD_stepwise <- BD[BD$TYP_IRIS=='H',c(13:55)]


m0 <- lm(TXCHOM1564~1,data=BD_stepwise[,-c(2:4)]) 
mf <- lm(TXCHOM1564~.,data=BD_stepwise[,-c(2:4)]) 

step(m0, scope=list(lower=m0, upper=mf),data=BD_stepwise[,-c(2:4)], direction="forward")
step(mf, data=BD_stepwise[,-c(2:4)],direction="backward")
step(m0, scope = list(upper=mf),data=BD_stepwise[,-c(2:4)],direction="both")

#variable par stepwise forward AIC : -1116.54
#C14_MENFAMMONO + P14_SAL15P_CDI + P14_NSCOL15P_DIPLMIN + 
#  TX1524 + C14_ACTOCC1564_CS5 + P14_NSAL15P_INDEP + P14_SAL15P_APPR + 
#  C14_ACTOCC1564_CS6 + C14_ACTOCC15P_TCOM + C14_ACTOCC15P_MAR + 
#  P14_RP_LOC + C14_ACTOCC15P_VOIT + P14_ACTOCC15P_ILT5 + P14_RP_LOCHLMV + 
#  P14_NSCOL15P_SUP

#variable par stepwise backward : -1107.21
#C14_ACTOCC1564_CS2 + C14_ACTOCC1564_CS3 + 
#  C14_ACTOCC1564_CS4 + C14_ACTOCC1564_CS6 + TX1524 + C14_MENCOUPSENF + 
#  C14_MENCOUPAENF + P14_NSCOL15P_DIPLMIN + P14_NSCOL15P_CAPBEP + 
#  P14_NSCOL15P_BAC + P14_NSCOL15P_SUP + P14_RP_PROP + P14_RP_LOCHLMV + 
#  C14_ACTOCC15P_PAS + C14_ACTOCC15P_DROU + C14_ACTOCC15P_VOIT + 
#  P14_SAL15P_CDD + P14_SAL15P_INTERIM + P14_NSAL15P_EMPLOY + 
#  P14_ACTOCC15P_ILT1 + P14_ACTOCC15P_ILT2 + P14_ACTOCC15P_ILT3 + 
# P14_ACTOCC15P_ILT4

#variable par stepwise both -1116.54
#C14_MENFAMMONO + P14_SAL15P_CDI + P14_NSCOL15P_DIPLMIN + 
#  TX1524 + C14_ACTOCC1564_CS5 + P14_NSAL15P_INDEP + P14_SAL15P_APPR + 
#  C14_ACTOCC1564_CS6 + C14_ACTOCC15P_TCOM + C14_ACTOCC15P_MAR + 
#  P14_RP_LOC + C14_ACTOCC15P_VOIT + P14_ACTOCC15P_ILT5 + P14_RP_LOCHLMV + 
#  P14_NSCOL15P_SUP



#backward seul : C14_ACTOCC1564_CS2 C14_ACTOCC1564_CS3 C14_ACTOCC1564_CS4 
#  C14_MENCOUPSENF C14_MENCOUPAENF P14_NSCOL15P_CAPBEP P14_NSCOL15P_BAC
#  P14_RP_PROP C14_ACTOCC15P_PAS C14_ACTOCC15P_DROU P14_SAL15P_CDD P14_SAL15P_INTERIM
#  P14_ACTOCC15P_ILT3 P14_NSAL15P_EMPLOY
# P14_ACTOCC15P_ILT1 + P14_ACTOCC15P_ILT2 + P14_ACTOCC15P_ILT3 + P14_ACTOCC15P_ILT4

#forward + both : C14_MENFAMMONO P14_SAL15P_CDI C14_ACTOCC1564_CS5 P14_NSAL15P_INDEP P14_SAL15P_APPR
#               C14_ACTOCC15P_TCOM C14_ACTOCC15P_MAR P14_RP_LOC
#trois : P14_NSCOL15P_DIPLMIN TX1524 C14_ACTOCC1564_CS6 C14_ACTOCC15P_VOIT P14_ACTOCC15P_ILT5 P14_RP_LOCHLMV
#         P14_NSCOL15P_SUP

#ACP
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(corrplot)

BD_ACP <- BD[BD$TYP_IRIS=='H',c('TXCHOM1564','C14_ACTOCC1564_CS2' ,'C14_ACTOCC1564_CS3' ,'C14_ACTOCC1564_CS4'
                                ,'C14_MENCOUPSENF', 'C14_MENCOUPAENF', 'P14_NSCOL15P_CAPBEP', 'P14_NSCOL15P_BAC'
                                ,'P14_RP_PROP', 'C14_ACTOCC15P_PAS', 'C14_ACTOCC15P_DROU', 'P14_SAL15P_CDD', 'P14_SAL15P_INTERIM'
                                ,'P14_ACTOCC15P_ILT3' ,'P14_NSAL15P_EMPLOY'
                                ,'P14_ACTOCC15P_ILT1' ,'P14_ACTOCC15P_ILT2',  'P14_ACTOCC15P_ILT3' , 'P14_ACTOCC15P_ILT4'
                                ,'C14_MENFAMMONO', 'P14_SAL15P_CDI', 'C14_ACTOCC1564_CS5', 'P14_NSAL15P_INDEP', 'P14_SAL15P_APPR'
                                ,'C14_ACTOCC15P_TCOM', 'C14_ACTOCC15P_MAR', 'P14_RP_LOC'
                                ,'P14_NSCOL15P_DIPLMIN' ,'TX1524', 'C14_ACTOCC1564_CS6', 'C14_ACTOCC15P_VOIT', 'P14_ACTOCC15P_ILT5', 'P14_RP_LOCHLMV'
                                ,'P14_NSCOL15P_SUP')]


#choix des variables l'ACP
library("corrplot")
corrplot(cor(BD_ACP),is.corr=FALSE)
res.pca <- PCA(BD_ACP,quanti.sup = c(1),axes=c(1,2))
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
res.pca <- PCA(BD_ACP,quanti.sup = c(1),axes=c(1,3))
res.pca$eig
#valeur propres 
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions des variables à PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

# cos des variables à PC1
fviz_cos2(res.pca, choice = "var", axes = 1, top = 10)
# cos des variables à PC2
fviz_cos2(res.pca, choice = "var", axes = 2, top = 10)
# cos des variables à PC3
fviz_cos2(res.pca, choice = "var", axes = 3, top = 10)

res.pca$var$cos2
res.pca$var$coord
res.pca$var$contrib
res.pca$var$cor

#cercle de contribution 
# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)
library(xlsx)

tab_cos <- res.pca$var$cos2
tab_coord <- res.pca$var$coord
tab_cont <- res.pca$var$contrib

write.xlsx(x = tab_cos, file = "tab_cos.xlsx")
write.xlsx(x = tab_coord, file = "tab_coord.xlsx")
write.xlsx(x = tab_cont, file = "tab_cont.xlsx")


#catégoriser variables et individ selon la méthode de k-means pour avoir une idée 
#du groupement de variable et simplifier le choix des variables 

# Créez une variable de regroupement en utilisant kmeans
# Créez 3 groupes de variables (centers = 3)
res.pca$var$coord
res.km <- kmeans(res.pca$var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Colorer les variables par groupes
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster",
             repel= T)
table(grp)
print(grp[grp==1])
print(grp[grp==2])
print(grp[grp==3])





#indi 
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

# Créez une variable continue aléatoire de longueur 23,
# Même longeur que le nombre d'individus actifs dans l'ACP

# Colorer les individus par la variable continue
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

# Créez une variable de regroupement en utilisant kmeans
# Créez 3 groupes de variables (centers = 3)
res.pca$ind$coord
res.km <- kmeans(res.pca$ind$coord, centers = 3, nstart = 25)
res.km$cluster
grp <- as.factor(res.km$cluster)
# Colorer les variables par groupes
fviz_pca_ind(res.pca, col.ind = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster",
             repel= T)

BD_ACP$grp <- grp
table(grp)



#ajout des variables tram, periode et les différentes 
#classe de taux de chomage 
BD_stepwise$tram <- BD[BD$TYP_IRIS=='H',]$tram
BD_stepwise$periode <- BD[BD$TYP_IRIS=='H',]$periode
BD_stepwise$cluster <- BD[BD$TYP_IRIS=='H',]$cluster
BD_stepwise$TXCHOM1524 <- BD[BD$TYP_IRIS=='H',]$TXCHOM1524
BD_stepwise$TXCHOM2554 <- BD[BD$TYP_IRIS=='H',]$TXCHOM2554
BD_stepwise$TXCHOM5564 <- BD[BD$TYP_IRIS=='H',]$TXCHOM5564

#variable retenu final  
#C14_MENFAMMONO+P14_NSCOL15P_DIPLMIN+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
#  P14_RP_LOC+C14_ACTOCC15P_VOIT+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524


#verification des egalites des test 
t.test(BD_stepwise$C14_MENFAMMONO ~ BD_stepwise$tram) #p-value = 0.28
t.test(BD_stepwise$P14_SAL15P_CDI ~ BD_stepwise$tram) #p-value = 0.00
t.test(BD_stepwise$C14_ACTOCC1564_CS5 ~ BD_stepwise$tram) #p-value = 0.79
t.test(BD_stepwise$P14_NSAL15P_INDEP ~ BD_stepwise$tram) #p-value = 0.30
t.test(BD_stepwise$P14_SAL15P_APPR ~ BD_stepwise$tram) #p-value = 0.08
t.test(BD_stepwise$C14_ACTOCC15P_TCOM ~ BD_stepwise$tram) #p-value = 0.12
t.test(BD_stepwise$C14_ACTOCC15P_MAR ~ BD_stepwise$tram) #p-value = 0.00
t.test(BD_stepwise$P14_RP_LOC ~ BD_stepwise$tram) #p-value = 0.00
t.test(BD_stepwise$TX1524 ~ BD_stepwise$tram) #p-value = 0.02
t.test(BD_stepwise$C14_ACTOCC1564_CS6 ~ BD_stepwise$tram) #p-value = 0.01
t.test(BD_stepwise$C14_ACTOCC15P_VOIT ~ BD_stepwise$tram) #p-value = 0.00
t.test(BD_stepwise$P14_ACTOCC15P_ILT5 ~ BD_stepwise$tram) #p-value = 0.40
t.test(BD_stepwise$P14_RP_LOCHLMV ~ BD_stepwise$tram) #p-value = 0.17
t.test(BD_stepwise$P14_NSCOL15P_SUP ~ BD_stepwise$tram) #p-value = 0.00


table(BD_stepwise$cluster, BD_stepwise$tram)


BD_stepwise$grp <- BD[BD$TYP_IRIS=='H',]$grp
BD_stepwise1 <-BD_stepwise
BD_stepwise1$LIBCOM <- BD[BD$TYP_IRIS=='H',]$LIBCOM
BD_stepwise1$LIBIRIS <- BD[BD$TYP_IRIS=='H',]$LIBIRIS
BD_stepwise1$QP <- BD[BD$TYP_IRIS=='H',]$QP

#C14_MENFAMMONO+P14_NSCOL15P_DIPLMIN+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
#  P14_RP_LOC+C14_ACTOCC15P_VOIT+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524
#premiere regression 
modele <- lm( TXCHOM1564 ~ tram +periode+tram*periode  + 
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+C14_ACTOCC15P_VOIT+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524
              
              , data = BD_stepwise1)
summary(modele)
library(lmtest)
bptest(modele)
# p-value =0



reset(modele)
#p-value = 0

library(car)
vif(modele)



#remarque: nous retirons quelque variables a cause de
# la forte multicolinearite 




modele <- lm( TXCHOM1564 ~ tram +periode+tram*periode   
              , data = BD_stepwise1)
summary(modele)

modele <- lm( TXCHOM1564 ~ tram +periode+tram*periode  + 
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+C14_ACTOCC15P_VOIT+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524
              , data = BD_stepwise1)
summary(modele)
library(lmtest)
bptest(modele)
# p-value =0



reset(modele)
#p-value = 0

library(car)
vif(modele)

plot(cooks.distance(modele), type="h")
plot(rstandard(modele), type="h")
residuals(modele)
BD_stepwise2 <- BD_stepwise1[rstandard(modele) <= 1,]
#on utilise le rstandard pour garder les quartiers les plus explicative
#dans notre modèle, et permettre d'ameliorer le modele par rapport 
#a sa forme fonctionnelle. 

#modele 2
modele <- lm( TXCHOM1564 ~  tram +periode+tram*periode  + 
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+C14_ACTOCC15P_VOIT+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524  
              , data = BD_stepwise2)
summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.00 


reset(modele)
#p-value = 0

library(car)
vif(modele)

plot(cooks.distance(modele), type="h")
BD_stepwise3 <- BD_stepwise2[rstandard(modele) <= 1,]


#modele 3
modele <- lm( TXCHOM1564 ~ tram +periode+tram*periode  + 
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+C14_ACTOCC15P_VOIT+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524    
              , data = BD_stepwise3)
summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.10 - Bon



reset(modele)
#p-value = 0

library(car)
vif(modele)

plot(cooks.distance(modele), type="h")
BD_stepwise4 <- BD_stepwise3[rstandard(modele) <= 1,]

## modele 3 
modele <- lm( TXCHOM1564 ~ tram +periode+tram*periode  + 
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524  
              , data = BD_stepwise4)
summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.18 - Bon



reset(modele)
#p-value = 0.05

library(car)
vif(modele)

#Nous pouvons retenir ce modele car le test de la forme fonctionnelle
#est validé, de même pour l'homoscédasticité des résidus 


#nom des quartiers retenu 
nom_quartier <- BD_stepwise4[,c('LIBIRIS', 'QP', 'tram', 'periode')]
library(xlsx)
write.xlsx(x = nom_quartier, file = "nom_quartier.xlsx")

#calcul des moyennes 
mean(BD_stepwise4[BD_stepwise4$periode == 0 & BD_stepwise4$tram==0,]$TXCHOM1564)
mean(BD_stepwise4[BD_stepwise4$periode == 1 & BD_stepwise4$tram==0,]$TXCHOM1564)

mean(BD_stepwise4[BD_stepwise4$periode == 0 & BD_stepwise4$tram==1,]$TXCHOM1564)
mean(BD_stepwise4[BD_stepwise4$periode == 1 & BD_stepwise4$tram==1,]$TXCHOM1564)

# Calcul manuel des 4 points nécessaires pour le calcul de l’effet :
(A = sapply(subset(BD_stepwise4, periode == 0 & tram == 1, select=TXCHOM1564), mean))

(B = sapply(subset(BD_stepwise4, periode == 0 & tram == 0, select=TXCHOM1564), mean))

(C = sapply(subset(BD_stepwise4, periode == 1 & tram == 1, select=TXCHOM1564), mean))

(D = sapply(subset(BD_stepwise4, periode == 1 & tram == 0, select=TXCHOM1564), mean))

# Calcul de l’effet de la mesure sur le taux de chomage par quartier 
((C-A)-(D-B))



library(lme4)
library(plm)
#test l'effet fixe 
#remarque , nous constatons aucune différence en indexant avec le tram 
# nous avons les memes coefficients que la regression normale . 
modele <- lm( TXCHOM1564 ~ tram +periode+tram*periode    
              , data = BD_stepwise4)
summary(modele)
modelecontinu<-plm(formula=TXCHOM1564~tram*periode+periode,index=c('tram'),
                   data=BD_stepwise4)
summary(modelecontinu)
library(xlsx)

modele <- data.frame(summary(modele)[4])
modelecontinu <- data.frame(summary(modelecontinu)[1])
write.xlsx(x = modele, file = "modele.xlsx")
write.xlsx(x = modelecontinu, file = "modelecontinu.xlsx")


#modele avec taux de chomage pour la population entre 15-24 ans 
modele <- lm( TXCHOM1524 ~ tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524 
              , data = BD_stepwise1)
modelecontinu<-plm(formula=TXCHOM1524~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD_stepwise)
summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.03 - non

reset(modele)
#p-value = 0 

library(car)
vif(modele)
BD1524<- BD_stepwise1[rstandard(modele) <= 1,]


modele <- lm( TXCHOM1524 ~ tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524 
              , data = BD1524)
modelecontinu<-plm(formula=TXCHOM1524~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD1524)
summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.00 - non

reset(modele)
#p-value = 0.02

library(car)
vif(modele)
BD1524_1<- BD1524[rstandard(modele) <= 1,]
modele <- lm( TXCHOM1524 ~ tram +periode+tram*periode
              , data = BD1524_1)
modelecontinu<-plm(formula=TXCHOM1524~tram*periode+periode,index=c('tram'),
                   data=BD1524_1)
summary(modele)
summary(modelecontinu)
modele <- lm( TXCHOM1524 ~ tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524 
              , data = BD1524_1)
modelecontinu<-plm(formula=TXCHOM1524~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD1524_1)
summary(modelecontinu)
summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.19 - Bon

reset(modele)
#p-value = 0.19

library(car)
vif(modele)

#cest le modele final pour le taux de chomage entre 15-24 ans 


# Calcul manuel des 4 points nécessaires pour le calcul de l’effet : avec le taux de chomage 15-24 ans 
(A = sapply(subset(BD1524_1, periode == 0 & tram == 1, select=TXCHOM1524), mean))

(B = sapply(subset(BD1524_1, periode == 0 & tram == 0, select=TXCHOM1524), mean))

(C = sapply(subset(BD1524_1, periode == 1 & tram == 1, select=TXCHOM1524), mean))

(D = sapply(subset(BD1524_1, periode == 1 & tram == 0, select=TXCHOM1524), mean))

# Calcul de l’effet de la mesure sur le taux de chomage par quartier 
((C-A)-(D-B))




#
#----
# regression DD avec le taux de chomage 24-54 ans
BD2554 <- BD_stepwise1

modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD2554)
modelecontinu<-plm(formula=TXCHOM2554~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD_stepwise4)
summary(modele)
summary(modelecontinu)

library(lmtest)
bptest(modele)
# p-value = 0.0 - Bon

reset(modele)
#p-value = 0.0

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD2554_1 <- BD2554[rstandard(modele) <= 1,]




modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD2554_1)

library(lmtest)
bptest(modele)
# p-value = 0.0 - non

reset(modele)
#p-value = 0.0

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD2554_2 <- BD2554_1[rstandard(modele) <= 1,]


modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD2554_2)

library(lmtest)
bptest(modele)
# p-value = 0.07 - Bon

reset(modele)
#p-value = 0.0

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD2554_3 <- BD2554_2[rstandard(modele) <= 1,]


#
modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD2554_3)

library(lmtest)
bptest(modele)
# p-value = O.22 - Bon

reset(modele)
#p-value = 0.00

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD2554_4 <- BD2554_3[rstandard(modele) <= 1,]


#
modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD2554_4)

library(lmtest)
bptest(modele)
# p-value = 0.42 - Bon

reset(modele)
#p-value = 0.0

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD2554_5 <- BD2554_4[rstandard(modele) <= 1,]


# avec 76 observations 
# Calcul manuel des 4 points nécessaires pour le calcul de l’effet :
(A = sapply(subset(BD2554_5, periode == 0 & tram == 1, select=TXCHOM2554), mean))

(B = sapply(subset(BD2554_5, periode == 0 & tram == 0, select=TXCHOM2554), mean))

(C = sapply(subset(BD2554_5, periode == 1 & tram == 1, select=TXCHOM2554), mean))

(D = sapply(subset(BD2554_5, periode == 1 & tram == 0, select=TXCHOM2554), mean))

# Calcul de l’effet de la mesure sur le taux de chomage par quartier 
((C-A)-(D-B))
modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode  
              , data = BD2554_5)
summary(modele)
summary(modelecontinu)

#modele retenu avec le taux de chomage entre 25-54 ans 
modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD2554_5)

modelecontinu<-plm(formula=TXCHOM2554~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD2554_5)

summary(modele)
summary(modelecontinu)
library(lmtest)
bptest(modele)
# p-value = 0.61 - Bon

reset(modele)
#p-value = 0.05

library(car)
vif(modele)

modele <- data.frame(summary(modele)[4])
modelecontinu <- data.frame(summary(modelecontinu)[1])
write.xlsx(x = modele, file = "modele.xlsx")
write.xlsx(x = modelecontinu, file = "modelecontinu.xlsx")


modele <- lm( TXCHOM2554 ~  tram +periode+tram*periode    
              , data = BD2554_5)

modelecontinu<-plm(formula=TXCHOM2554~tram*periode+periode,index=c('tram'),
                   data=BD2554_5)

summary(modele)
summary(modelecontinu)

#----



#----

BD5564 <- BD_stepwise1

modele <- lm( TXCHOM5564 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD5564)
modelecontinu<-plm(formula=TXCHOM5564~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD5564)
summary(modele)
summary(modelecontinu)

library(lmtest)
bptest(modele)
# p-value = 0.0 - non

reset(modele)
#p-value = 0.0

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD5564_1 <- BD5564[rstandard(modele) <= 1,]




modele <- lm( TXCHOM5564 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD5564_1)

library(lmtest)
bptest(modele)
# p-value = 0.02 - NON

reset(modele)
#p-value = 0.49

library(car)
vif(modele)


plot(cooks.distance(modele), type="h")
BD5564_2 <- BD5564_1[rstandard(modele) <= 1,]

#122 observation 
modele <- lm( TXCHOM5564 ~  tram +periode+tram*periode+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD5564_2)

library(lmtest)
bptest(modele)
# p-value = 0.06 - NON

reset(modele)
#p-value = 0.05

library(car)
vif(modele)
BD5564_2$effet <- BD5564_2$periode*BD5564_2$tram

modele <- lm( TXCHOM5564 ~  tram +periode+effet+
                C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524     
              , data = BD5564_2)
modelecontinu<-plm(formula=TXCHOM5564~tram*periode+periode+C14_MENFAMMONO+C14_ACTOCC15P_TCOM+C14_ACTOCC15P_MAR+
                     P14_RP_LOC+P14_RP_LOCHLMV+P14_NSCOL15P_SUP+TX1524,index=c('tram'),
                   data=BD5564_2)
summary(modele)
summary(modelecontinu)


#----

library(lmtest)
bptest(modele)
# p-value = 0.06 - Bon

reset(modele)
#p-value = 0.06

library(car)
vif(modele)




#covaraince residu et variables 
#nous avons verifier avec tout les modeles 
#et nous constatons que tout les variables sont 
# indépendantes avec les résidus des modeles 
cov(residuals(modele), BD5564_2$C14_MENFAMMONO)
cov(residuals(modele), BD5564_2$tram)
cov(residuals(modele), BD5564_2$periode)
cov(residuals(modele), BD5564_2$periode*BD5564_2$tram)
cov(residuals(modele), BD5564_2$P14_NSCOL15P_DIPLMIN)
cov(residuals(modele), BD5564_2$TX1524)
cov(residuals(modele), BD5564_2$C14_ACTOCC15P_TCOM)
cov(residuals(modele), BD5564_2$C14_ACTOCC15P_MAR)
cov(residuals(modele), BD5564_2$P14_RP_LOC)
cov(residuals(modele), BD5564_2$P14_RP_LOCHLMV)
cov(residuals(modele), BD5564_2$TXFEM)
cov(residuals(modele), residuals(modele))




### DOSSIER INTRODUCTION A R 


###############
#fonction 

#creation de la fonction recodage 
recodage <- function(varx, code){
  library(carData)
  library(car)
  varx <- recode(varx,code)
  varx <- as.factor(varx)
}


#creer graphique Histogramme
makehisto <- function(variable,x_titre,y_titre, main_titre, couleur1){
  hist(variable,col=couleur1,xlab=x_titre,ylab=y_titre,main=main_titre, 
       freq=FALSE)
  box()
  densite <- density(variable) 
  lines(densite, col = "red",lwd=3)
}


#creer des pie charts
makepie<-function(a,b,c){
  
  categoriesi = a
  pcti=round((b/sum(b)*100),2)
  lblsi=paste(a ,":",pcti,"%")
  couleurs=c("red","orange","green","grey","purple","white")
  pie(b,labels = lblsi,main=c, col=couleurs,cex=1.1, cex.main=1.5)
  box()
  
}

#creer barplot 
makebarplot <- function(a,b,c){
  tablecroise <- table(a,b)
  barplot(tablecroise, beside=T,col=c("black","gray"), main = c)
}



#Creer un Tableau significative
makesign <- function(coefficient,EcartType,P_signi){
  taille <- length(coefficient)
  Signi <- rep(NA, taille) 
  for (i in 1:taille){
    if (P_signi[i]<0.05){
      Signi[i] <- "Significative"
      
    }
    else{
      Signi[i] <- "Non significative"
    }
  }
  tableau <- data.frame(coefficient,EcartType,Signi)
  return (tableau)
  
}



#creer un PNG pour sortie de tableau 
sortie_tab <- function(x,y,z,w){
  library(gridExtra)
  png(x, height = z*nrow(y), width =
        w*ncol(y))
  grid.table(y)
  p1 <- tableGrob(head(y))
  p2 <- grid.arrange(p1)
  return(p2)
}


#Importe les donnees
library(foreign)
donnee<- read.dta("INDIV151.dta", convert.factors=FALSE)


#on garde que les individu avec un salaire et "actif occupe" selon la BIT
BD <- donnee
BD <- BD[BD$salmee!="      0",]
BD <- BD[BD$salmee!="9999999",]
BD <- BD[BD$salmee!="9999998",]
BD <- BD[BD$acteu6=="1",]
table(BD$acteu6)
dim(BD)

#convertir en numerique
BD$salmee<-as.numeric(BD$salmee)

#on selectionne les individu avec un salaire egale ou supérieur que le smic
BD = BD[BD$salmee>1136,]

rm(donnee)



#recodage 
BD$salmee <-as.numeric(BD$salmee)
BD$age <-as.integer(BD$age)
BD$ancentr <-as.integer(BD$ancentr)
BD$nbind <- as.integer(BD$nbind)

#fonction recodage créer 
table(BD$sexe)
BD$sexe <- recodage(BD$sexe,"'1'=0;'2'=1")
table(BD$sexe)

BD$immi <- recodage(BD$immi,"'1'=1;'2'=0")
table(BD$immi)

BD$nivp <- recodage(BD$nivp, "'73'=0;'72'=0;'71'=0;'60'=1;
                    '50'=1;'41'=1;'40'=2;'30'=2;'20'=3;'10'=4" )
table(BD$nivp)

BD$ccontr <- recodage(BD$ccontr,"''=0;'0'=0;'5'=1;'4'=2;'3'=3;'2'=4;'1'=5")
table(BD$ccontr)

BD$zus <- recodage(BD$zus,"''=0" )

BD$ccontr <- as.factor(BD$ccontr)
BD$tuu2010r <- as.factor(BD$tuu2010r)


#creation de la nouvelle variable nation
#avec un des parents ayant une nationnalité etrangere
BD$nat14 <- recode(BD$nat14,"''=99")


BD$nation[BD$nat14=="10"]="0"
BD$nation[BD$nat14=="11"]="1"
BD$nation[BD$nat14=="12"]="1"
BD$nation[BD$nat14=="13"]="1"
BD$nation[BD$nat14=="14"]="1"
BD$nation[BD$nat14=="21"]="2"
BD$nation[BD$nat14=="22"]="2"
BD$nation[BD$nat14=="23"]="2"
BD$nation[BD$nat14=="24"]="3"
BD$nation[BD$nat14 == "31"]="4"
BD$nation[BD$nat14 == "32"]="4"
BD$nation[BD$nat14 == "41"]="4"
BD$nation[BD$nat14 == "51"]="4"
BD$nation[BD$nat14 == "99"]="4"

BD$nation[BD$natper == "06" | BD$natmer == "06"]="2"
BD$nation[BD$natper == "07" | BD$natmer == "07"]="3"
BD$nation[BD$natper == "03" | BD$natmer == "03"]="1"
BD$nation[BD$natper == "04" | BD$natmer == "04"]="1"
BD$nation[BD$natper == "05" | BD$natmer == "05"]="1"
BD$nation[BD$natper == "08" | BD$natmer == "08"]="4"
BD$nation[BD$natper == "09" | BD$natmer == "09"]="4"
BD$nation[BD$natper == "10" | BD$natmer == "10"]="4"
BD$nation[BD$natper == "99" | BD$natmer == "99"]="4"

BD$nation <- as.factor(BD$nation)

#creation de la nouvelle variable ORIGINE 
BD$ORIGINE[BD$naimer!="1" | BD$naiper!="1" |BD$nat14!="10"]="1"
BD$ORIGINE[BD$naimer=="1" & BD$naiper=="1" &BD$nat14=="10"]="0"


#creation de la variable binaire type public ou prive 
BD$pp[BD$pub3fp=="1" ]="0"
BD$pp[BD$pub3fp=="2" ]="0"
BD$pp[BD$pub3fp=="3" ]="0"
BD$pp[BD$pub3fp=="4" ]="1"



#nouvelle base avec l'exploitation des variables pour le modele 
BD1 <- data.frame(BD$salmee,BD$age,BD$ancentr,BD$nbind, BD$sexe, BD$immi, BD$ORIGINE, BD$pp, BD$zus,
                  BD$cser , BD$ccontr, BD$nivp, BD$tuu2010r, BD$nation,row.names=row.names(BD))
vect_name <- c("salmee","age","ancentr","nbind","sexe","immi","origine",
               "pp","zus","cser","ccontr","nivp","tuu2010r","nation")
names(BD1) <- vect_name


#nettoyage de la base 
BD2<-BD1
BD2<-na.omit(BD2)


#nommer les variable qualitative
BD2$immi1[BD2$immi==0] <- "non immigrant" 
BD2$immi1[BD2$immi==1] <- "immigrant"
table(BD2$immi1)

BD2$sexe1[BD2$sexe==0] <- "Homme"
BD2$sexe1[BD2$sexe==1] <- "Femme"
table(BD2$sexe1)

BD2$nivp1[BD2$nivp==""] <- "NIV 1"
BD2$nivp1[BD2$nivp==0] <- "NIV 1"
BD2$nivp1[BD2$nivp==1] <- "NIV 2"
BD2$nivp1[BD2$nivp==2] <- "NIV 3"
BD2$nivp1[BD2$nivp==3] <- "NIV 4"
BD2$nivp1[BD2$nivp==4] <- "NIV 5"

table(BD2$nivp1)

BD2$zus1[BD2$zus == 0 ] <- "ZUS NON"
BD2$zus1[BD2$zus == 1 ] <- "ZUS OUI"

BD2$ccontr1[BD2$ccontr== 0]<- "Pas de contrat de travail ou pas renseigne"
BD2$ccontr1[BD2$ccontr== 1]<- "Contrat d'apprentissage"
BD2$ccontr1[BD2$ccontr== 2]<- "Contrat d'interim"
BD2$ccontr1[BD2$ccontr== 3]<- "Contrat saisonnier"
BD2$ccontr1[BD2$ccontr== 4]<- "CDD"
BD2$ccontr1[BD2$ccontr== 5]<- "CDI"

BD2$nation1[BD2$nation==0] <- "Francais"
BD2$nation1[BD2$nation==1] <- " europeen"
BD2$nation1[BD2$nation==2] <- " Maghreb"
BD2$nation1[BD2$nation==3] <- " Africaine"
BD2$nation1[BD2$nation==4] <- "Reste du monde"

BD2$origine1[BD2$origine==0] <- "Pas d'origine"
BD2$origine1[BD2$origine==1] <- "origines"

BD2$pp1[BD2$pp==0] <- "Public"
BD2$pp1[BD2$pp==1] <- "Privé"


BD2$cser1[BD2$cser ==0 ] <-"Non renseigne" 
BD2$cser1[BD2$cser ==1 ] <-"Agriculteurs exploitants" 
BD2$cser1[BD2$cser ==2 ] <-"Artisans, commercants et chefs d'entreprises" 
BD2$cser1[BD2$cser ==3 ] <-"Cadres et professions intellectuelles supérieures" 
BD2$cser1[BD2$cser ==4 ] <-"Professions intermédiaires" 
BD2$cser1[BD2$cser ==5 ] <-"Employés" 
BD2$cser1[BD2$cser ==6 ] <-"Ouvriers" 


BD2$tuu2010r1[BD2$tuu2010r ==1 ] <-"Communaute rurale"
BD2$tuu2010r1[BD2$tuu2010r ==2 ] <-"Unité urbaine de - de 20k habitants"
BD2$tuu2010r1[BD2$tuu2010r ==3 ] <-"Unité urbaine de 20k à moins de 200k habitants"
BD2$tuu2010r1[BD2$tuu2010r ==4 ] <-"Unité urbaine de 200k habitants ou plus"
BD2$tuu2010r1[BD2$tuu2010r ==5 ] <-"Agglomeration parisienne"


table(BD2$sexe)

#detection des valeurs aberrantes 


library(EnvStats)
str(BD2$salmee)

par(mfrow=c(2,2))
boxplot(BD2$salmee, main = "Salaire mensuel", color= "Blue")
boxplot(BD2$age, main ="Age",color= "Blue")
boxplot(BD2$ancentr, main = "Anciennete dans l'entreprise")
boxplot(BD2$nbind, main = "Nb de pers dans le logement")

rosnerTest(BD2$salmee, k = 20, alpha = 0.05) 
order(BD2$salmee)
sort(BD2$salmee)
BD3 = BD2[BD2$salmee<8000,]

#on utilise order pour trier par rapport au salaire dans Rstudio
#Puis on peut comparer par rapport au graphique de boxplot 
#quelle valeur doit être inferieur au valeur aberrante

rosnerTest(BD3$salmee, k = 20, alpha = 0.05) 
boxplot(BD3$salmee)
BD3 = BD3[BD3$salmee<3157,]
boxplot(BD3$age)

boxplot(BD3$ancentr)
rosnerTest(BD3$ancentr, k = 9, alpha = 0.05)
BD3 = BD3[BD3$ancentr<492,]
BD3 = BD3[BD3$ancentr>-1,]

boxplot(BD3$nbind)
rosnerTest(BD3$nbind, k = 9, alpha = 0.05)
BD3 = BD3[BD3$nbind<8,]


#save BD3
save(BD, file = "RData")
save(BD1, file = "BD1.RData")
save(BD2, file = "BD2.RData")
save(BD3, file = "BD3.RData")
dim(BD3)

rm(BD1)
rm(BD2)
rm(BD)

##########################
summary(BD3)
boxplot(BD3)

library(psych)
describeBy(BD3)

par(mfrow=c(2,2))
boxplot(BD3$salmee, main = "Salaire mensuel", color= "Blue")
boxplot(BD3$age, main ="Age",color= "Blue")
boxplot(BD3$ancentr, main = "Anciennete dans l'entreprise")
boxplot(BD3$nbind, main = "Nb de pers dans le logement")



#########################
#matrice de correlation 
library(corrplot)
m<- cor(BD3[,c(1:4)])
corrplot(m, method="number")


##########################
#Variable numerique continue histogramme
par(mfrow=c(2,2))
makehisto(BD3$salmee,"Salaire mensuel (euros)", "Frequence","Salaire mensuel","#F5D0A9")
makehisto(BD3$age,"Annee", "Frequence","Âge","green")
makehisto(BD3$ancentr,"Nombre de mois", "Frequence","Anciennete dans l'entreprise","Gray")
makehisto(BD3$nbind,"nombre de personne", "Frequence","Nombre de personne dans le logement","Yellow")


#############################
#tableau avec la moyenne et ecart-type
library(questionr)

library(psych)
describeBy(BD3)

library(gmodels)
CrossTable(BD3$nation,BD3$cser,prop.chisq=FALSE,chisq=FALSE,expected=FALSE)



#Creation des bases avec origine et aucune origine 
BD_NORI = BD3[BD3$origine==0,]
BD_ORIGINE = BD3[BD3$origine!=0,]

##############################################################################
#graphique histogramme 

#histogramme BD3 avec densité de base BD_NORI et BD_ORIGINE
makehisto(BD3$salmee,"Salaire mensuel (euros)", "Frequence","Salaire mensuel","#F5D0A9")
densite <- density(BD_NORI$salmee)
lines(densite, col = "red",lwd=3)
densite1 <- density(BD_ORIGINE$salmee)
lines(densite1, col = "green",lwd=3)
box()
legend("topright", legend = c("Non origine", "Origine"),
       col= c("red","green"),lty=1:1, cex=1.5,
       box.lty=2, box.lwd=2
       )


hist(BD_ORIGINE$salmee,col="#96FD13",
     xlab="Salaire mensuel (euros)",ylab="Fréquences",main="Salaire mensuel des ind ayant des origines", freq=FALSE)
densite1 <- density(BD_ORIGINE$salmee)
lines(densite1, col = "red",lwd=3)

som<- summary(BD_NORI$salmee)


library(psych)

describeBy(BD_NORI$salmee)
describeBy(BD_ORIGINE$salmee)



hist(BD_ORIGINE$salmee)
hist(BD_NORI$salmee)




##### graphique pie 
#fonction graphique 
library("questionr")

par(mfrow=c(2,1))

#nombre d'origine
t<- table(BD3$origine1)
t<- data.frame(t)
makepie(t$Var1,t$Freq,"Individu ayant des origines")
table(BD3$origine)

#nombre d'immigree
t11 <- table(BD3$immi1)
t11<- data.frame(t11)
makepie(t11$Var1,t11$Freq,"Immigré")
table(BD3$immi1)

#Nationalite avec base d'origine
t1 <- table(BD_ORIGINE$nation1)
t1 <- data.frame(t1)
titre1 <- "graphique : Nationalite selon l'origine"
makepie(t1$Var1,t1$Freq,titre1)

#Niveau de diplome
par(mfrow=c(3,1))
t2 <- table(BD3$nivp1)
t2 <- data.frame(t2)
titre2<- "Niveau de diplome"
makepie(t2$Var1,t2$Freq, titre2)

t2_2 <- table(BD_NORI$nivp1)
t2_2 <- data.frame(t2_2)
titre2_2 <- "Niveau de diplome pour ind sans origine"
makepie(t2_2$Var1,t2_2$Freq,titre2_2)

t2_1 <- table(BD_ORIGINE$nivp1)
t2_1 <- data.frame(t2_1)
titre2_1 <- "Niveau de diplome pour ind ayant des origines"
makepie(t2_1$Var1,t2_1$Freq,titre2_1)

#categorie socio-prof
par(mfrow=c(3,1))
t3 <- table(BD3$cser1)
t3 <- data.frame(t3)
titre3 <- "Categorie socio-professionnelles"
makepie(t3$Var1,t3$Freq,titre3)

t3_2 <- table(BD_NORI$cser1)
t3_2 <- data.frame(t3_2)
titre3_2 <- "Categorie socio-professionnelles pour ind sans origine"
makepie(t3_2$Var1,t3_2$Freq,titre3_2)

t3_1 <- table(BD_ORIGINE$cser1)
t3_1 <- data.frame(t3_1)
titre3_1 <- "Categorie socio-professionnelles pour ind ayant des origines"
makepie(t3_1$Var1,t3_1$Freq,titre3_1)

#contrat
par(mfrow=c(3,1))
t4 <- table(BD3$ccontr1)
t4 <- data.frame(t4)
titre4<-"Contrat de travail"
makepie(t4$Var1,t4$Freq,titre4)

t4_2 <- table(BD_NORI$ccontr1)
t4_2 <- data.frame(t4_2)
titre4_2<- "contrat de travail pour ind sans origine"
makepie(t4_2$Var1,t4_2$Freq,titre4_2)

t4_1 <- table(BD_ORIGINE$ccontr1)
t4_1 <- data.frame(t4_1)
titre4_1 <- "contrat de travail pour ind avec origine"
makepie(t4_1$Var1,t4_1$Freq,titre4_1)

#Unite urbaine 2010
par(mfrow=c(3,1))
t5 <- table(BD3$tuu2010r1)
t5 <- data.frame(t5)
titre5 <-"Unite urbaine du logement de residence"
makepie(t5$Var1,t5$Freq,titre5)

t5_2 <- table(BD_NORI$tuu2010r1)
t5_2 <- data.frame(t5_2)
titre5_2 <-"Unite urbaine du logement de residence pour ind sans origine"
makepie(t5_2$Var1,t5_2$Freq,titre5_2)

t5_1 <- table(BD_ORIGINE$tuu2010r1)
t5_1 <- data.frame(t5_1)
titre5_1 <-"Unite urbaine du logement de residence pour ind avec origine"
makepie(t5_1$Var1,t5_1$Freq,titre5_1)



### barplot 

#graphique barplot
library(descr)
tit1<- "contrat de travail selon le sexe avec des ind ayant des origine"
makebarplot(BD_ORIGINE$sexe1,BD_ORIGINE$ccontr1,tit1)
legend("topright", legend = c("Femme", "Homme"),
       fill = c("black","gray"),
       cex = 1.7)

tit1_1<- "contrat de travail selon le sexe avec des ind sans origine"
makebarplot(BD_NORI$sexe1,BD_NORI$ccontr1,tit1_1)
legend("topright", legend = c("Femme", "Homme"),
       fill = c("black","gray"),
       cex = 1.7)

tit2 <- "Type de travail"
makebarplot(BD3$sexe1,BD3$pp1,tit2)
legend("topright", legend = c("Femme", "Homme"),
       fill = c("black","gray"),
       cex = 1.7)


#type de travail 
tit2_1 <- "Type de travail pour ind ayant des origines"
makebarplot(BD_ORIGINE$sexe1,BD_ORIGINE$pp1,tit2_1)
legend("topright", legend = c("Femme", "Homme"),
       fill = c("black","gray"),
       cex = 1.7)

tit2_2 <- "Type de travail pour ind sans origines"
makebarplot(BD_NORI$sexe1,BD_NORI$pp1,tit2_2)
legend("topright", legend = c("Femme", "Homme"),
       fill = c("black","gray"),
       cex = 1.7)




rm(BD1)
rm(BD)
rm(BD2)
rm(donnee)

####
#regression lineaire multiple 
# categoriser les variable X continue pour l'anciennete en entreprise

BD3$age1[BD3$age<35]<- "moins de 35 ans"
BD3$age1[BD3$age>=35 & BD3$age<45]<- "35 et 45 ans"
BD3$age1[BD3$age>=45 & BD3$age<55]<- "45 et 55 ans"
BD3$age1[BD3$age>=55]<- "plus de 55"
table(BD3$age1)

BD3$ancentr1<-round(BD3$ancentr/12,0)

BD3$ancentr2[BD3$ancentr1<5]<- "- de 5 ans"
BD3$ancentr2[BD3$ancentr1>=5 & BD3$ancentr1<15]<- "5 et 14 ans"
BD3$ancentr2[BD3$ancentr1>=15 & BD3$ancentr1<25]<- "15 et 24 ans"
BD3$ancentr2[BD3$ancentr1>=25 ]<- "+ 25 ans"
table(BD3$ancentr2)


#### RLM 
library(MASS)
library(gridExtra)
str(BD3)

BD3$origine1[BD3$origine==0] <- "Pas d'origine"
BD3$origine1[BD3$origine==1] <- "origines"
table(BD3$origine1)

BD_ORIGINE <- BD3[BD3$origine1=="origines",]
BD_NORI <- BD3[BD3$origine1=="Pas d'origine",]

BD_IMMI <- BD3[BD3$immi1=="immigrant",]
BD_NOIMMI <- BD3[BD3$immi1=="non immigrant",]

table(BD3$immi1)
str(BD3$sexe1)

# changer l'ordre des variable quali multi fact
facteur <- function(x,y){
x <- factor(x ,levels = y)
return(x)
}

## permet d'avoir le sens des facteurs pour le referentiel
BD3$nivp1<-facteur(BD3$nivp1,c("NIV 2","NIV 5","NIV 4","NIV 3","NIV 1"))
BD3$origine1 <- facteur(BD3$origine1,c("Pas d'origine","origines"))
BD3$nation1 <- facteur(BD3$nation1 ,c("Francais"," europeen"," Maghreb"," Africaine","Reste du monde"))
BD3$immi1 <- facteur(BD3$immi1,c("non immigrant","immigrant"))
BD3$cser1 <- facteur(BD3$cser1, c("Employés","Cadres et professions intellectuelles supérieures",
                                  "Ouvriers","Professions intermédiaires","Non renseigne"))
BD3$sexe1 <- facteur(BD3$sexe1,c("Homme","Femme"))
BD3$tuu2010r1 <- facteur(BD3$tuu2010r1,c("Unité urbaine de 200k habitants ou plus"
                                       ,"Agglomeration parisienne",
                                       "Unité urbaine de 20k à moins de 200k habitants"
                                       ,"Unité urbaine de - de 20k habitants",
                                       "Communaute rurale"))
table(BD3$ancentr2)
BD3$age1 <- facteur(BD3$age1,c("35 et 45 ans","moins de 35 ans","45 et 55 ans","plus de 55"))


#####################################
rego1 <- lm(log(salmee) ~  age+ ancentr+ nbind+ sexe1
            + nivp1+nation1+ zus1+ immi1+ origine1+ cser1+ tuu2010r1+pp1,data= BD3)
sommaire<- summary(rego1)

tabrego1 <- round(coef(rego1),6)
Ecart_type1 <- round(sommaire$coefficients[,2],6)
P_value1<- round(sommaire$coefficients[,4],3)
tableau_reg <- makesign(tabrego1,Ecart_type1,P_value1)

#sortie tableau 
#Creer un Tableau significative
sortie_tab("modele1.png", tableau_reg, 50,310)




table(BD3$origine1)
hist(BD_NORI$salmee, freq=F)
hist(BD_ORIGINE$salmee, freq=F)


#Modele 2 en retirant origine
str(BD3)
rego2 <- lm(log(salmee) ~  age+ ancentr+ nbind+ sexe1
            + nivp1+nation1+ zus1+ immi1+ cser1+ tuu2010r1+pp1,data= BD3)
sommaire2<- summary(rego2)


tabrego2 <- round(coef(rego2),6)
Ecart_type2 <- round(sommaire2$coefficients[,2],6)
P_value2<- round(sommaire2$coefficients[,4],3)
tableau_reg2 <- makesign(tabrego2,Ecart_type2,P_value2)
sortie_tab("modele2.png", tableau_reg2, 50,310)


table(BD3$cser1)

### analyse du modele 
#Existence de valeurs influençant les estimations via le graphique de la distance de Cook
plot(cooks.distance(rego2),type="h")
# pas d'observation influençant l'estimation du modele 


#Forme linéaire  
library(zoo)
library(lmtest) 
reset(rego2)
#On accepte pas la forme linéaire car p_value < 0.05

#test de fisher , il existe au moins une des variables qui est significative 


#test des residus 
residu <- residuals(rego2)
hist(residu)
#Test de Kolmogorov pour les grands nombres.
ks.test(residu,"pnorm",mean(residu),sd(residu))
#On valide l'hypothese que les residu suit une loi normale

#H d'HMS des residus 
#on refuse l'Homoscedasticite 
bptest(rego2)

#La probabilité critique du test étant inférieure
# à 0,05, l’hypothèse d’homoscédascité des résidus 
#du modèle n’est pas acceptée au seuil de risque de 5 %.

BD3$ancentr2

#identifier la variable explicative 
ncvTest(rego2,~BD3$ancentr2) #p_value= 0.14
ncvTest(rego2,~BD3$sexe1) #P_Value= 0.054
ncvTest(rego2,~BD3$nivp1) #P_Value= 0.066
ncvTest(rego2,~BD3$zus1) #P_Value= 0.26
ncvTest(rego2,~BD3$immi1) #P_Value= 5.1e^-6 , refuse H0
ncvTest(rego2,~BD3$cser1) #P_Value= 2.5e^-5 , refuse H0
ncvTest(rego2,~BD3$tuu2010r1) #P_Value= 0.00016 , refuse H0
ncvTest(rego2,~BD3$origine1) #P_Value= 0.00051 , refuse H0



#source d'Heterosc ne fonctionne pas car variabl qualiti a plusieurs facteur 
#revoir 
library(car)
residualPlots(rego2)
## NBIND pas significative


str(BD3)

#correction de la matrice de white 
library(sandwich)
library(lmtest)
coeftest(rego2,vcov=vcovHC(rego2,type="HC0"))
waldtest(rego2, vcov = vcovHC(rego2,type="HC0"))

vif(rego2)
#comme Y quantitative et Xqualitative a plusieurs facteur : analyse de la covariance avec
# ANCOVA 
plot(rego2)
anova(rego2)
summary.aov(rego2)




#modele 3 avec base immi
rego3 <- lm(log( salmee) ~  age+ ancentr+
              nbind+ sexe1
            + nivp1+ zus1+pp1+ccontr1
            + cser1+ tuu2010r1, data=BD_IMMI)
sommaire3<-summary(rego3)
tabrego3 <- round(coef(rego3),6)

Ecart_type3 <- round(sommaire3$coefficients[,2],6)
P_value3<- round(sommaire3$coefficients[,4],3)


tableau_reg3 <- makesign(tabrego3,Ecart_type3,P_value3)
tit_modele3="modele3.png"
sortie_tab(tit_modele3, tableau_reg3, 50,310)

summary(BD_ORIGINE$salmee)
summary(BD_NORI$salmee)
summary(BD3$salmee)
str(BD3)

#modele 4 avec base sans immi
rego4 <- lm(log( salmee) ~  age+ ancentr+
              nbind+ sexe1
            + nivp1+ zus1+pp1+ccontr1
            + cser1+ tuu2010r1,data= BD_NOIMMI)
summary(rego4)
sommaire4<-summary(rego4)
tabrego4 <- round(coef(rego4),6)
dim(tabrego4)
Ecart_type4 <- round(sommaire4$coefficients[,2],6)
P_value4<- round(sommaire4$coefficients[,4],3)
signi_4 <- rep(NA, 22) 


summary(exp(predict(rego3)))
summary(exp(predict(rego4)))
tableau_reg4 <- makesign(tabrego4,Ecart_type4,P_value4)
tit_modele4="modele4.png"
sortie_tab(tit_modele4, tableau_reg4, 50,310)


#illustrer les coefficient des deux groupes selon immi 
test1 <- data.frame(round(coef(rego3),4),round(coef(rego4),4))
sortie_tab("uni.png",test1,70,290)



#Magrebh 

table(BD3$nation1)
BD_ARABE <- BD3[BD3$nation==2,]
dim(BD_ARABE)

rego5 <- lm(log( salmee) ~  age+ ancentr+
              nbind+ sexe1
            + nivp1+ zus1+pp1+ccontr1
            + cser1+ tuu2010r1+pp1,data= BD_ARABE)
summary(rego5)

sommaire5<-summary(rego5)
tabrego5 <- round(coef(rego5),6)

Ecart_type5 <- round(sommaire5$coefficients[,2],6)
P_value5<- round(sommaire5$coefficients[,4],3)


tableau_reg5 <- makesign(tabrego5,Ecart_type5,P_value5)
tit_modele5="modele5.png"
sortie_tab(tit_modele5, tableau_reg5, 50,310)



#modele de décomposition des salaires 
str(BD4)

BD4[,c(28:47)]=lapply(BD4[,c(28:47)],as.factor)
table(BD4$origine)

rego5 <- lm(log(salmee)~ 
            sexe+NIV1+NIV2+NIV3+NIV4+ zus+pp+CDD+
              CDI+CA+CI+CS+EMPL+OUV
            +K200KH+APARIS+K20K200K+UM20K,
            data = BD4[BD4$origine==1,])

summary(rego5)
rego6 <- lm(log(salmee)~ 
              sexe+NIV1+NIV2+NIV3+NIV4+ zus+pp+CDD+
              CDI+CA+CI+CS+EMPL+OUV
            +K200KH+APARIS+K20K200K+UM20K,
            data = BD4[BD4$origine==0,])
summary(rego6)


coeffs.A <- rego5$coefficients
coeffs.B <- rego6$coefficients
tablecoef <- round(cbind(coeffs.A,coeffs.B),3)
sortie_tab("tablecoef.png", tablecoef, 50,310)
mean(predict(rego5))
mean(predict(rego6))
X.A <- model.matrix(~  sexe+NIV1+NIV2+NIV3+NIV4+ zus+pp+CDD+
                      CDI+CA+CI+CS+EMPL+OUV
                    +K200KH+APARIS+K20K200K+UM20K,
                    data = BD4[BD4$origine==1,])
#on applique la fonction moyenne pour chaque variable
X.moy.A<-apply(X.A,2,mean)

X.B <- model.matrix(~  sexe+NIV1+NIV2+NIV3+NIV4+ zus+pp+CDD+
                      CDI+CA+CI+CS+EMPL+OUV
                    +K200KH+APARIS+K20K200K+UM20K,
                    data = BD4[BD4$origine==0,])
X.moy.B<-apply(X.B,2,mean)
round(cbind(X.moy.A,X.moy.B),3)

sum((X.moy.B- X.moy.A)*coeffs.B)
sum(X.moy.A*(coeffs.B-coeffs.A))
somme <- sum((X.moy.B- X.moy.A)*coeffs.B)+sum(X.moy.A*(coeffs.B-coeffs.A))

mean(predict(rego6))-mean(predict(rego5))


plot(predict(rego5)~BD_ORIGINE$salmee)
abline(lm(predict(rego5)~BD_ORIGINE$salmee),col="red")
abline(lm(predict(rego6)~BD_NORI$salmee),col="green")

#methode de OAXACA
#Hlavac, Marek (2018). oaxaca: Blinder-Oaxaca Decomposition in R.
#R package version 0.1.4. https://CRAN.R-project.org/package=oaxaca

BD4<-BD3
table(BD4$nivp1)
table(BD4$nivp)
binary <- function(facteur, nom_va, var,base_salaire){
  base_salaire$nom_va[base_salaire$var==facteur]<- 1
  base_salaire$nom_va[base_salaire$var!=facteur]<- 0
  table(base_salaire$nom_va)

}


BD4$NIV2[BD4$nivp1=="NIV 2"] <- 1
BD4$NIV2[BD4$nivp1!="NIV 2"] <- 0
table(BD4$NIV2)

BD4$NIV3[BD4$nivp==2] <- 1
BD4$NIV3[BD4$nivp!=2] <- 0
table(BD4$NIV3)

BD4$NIV4[BD4$nivp==3] <- 1
BD4$NIV4[BD4$nivp!=3] <- 0
table(BD4$NIV4)

BD4$NIV5[BD4$nivp==4] <- 1
BD4$NIV5[BD4$nivp!=4] <- 0
table(BD4$NIV5)

BD4$NIV1[BD4$nivp1 == "NIV 1"] <- 1
BD4$NIV1[BD4$nivp1 != "NIV 1"] <- 0
table(BD4$NIV1)

table(BD3$ccontr)
table(BD3$ccontr1)
BD4$CDD[BD4$ccontr==4 ] <- 1
BD4$CDD[BD4$ccontr!=4] <- 0
table(BD4$CDD)

BD4$CDI[BD4$ccontr==5] <- 1
BD4$CDI[BD4$ccontr!=5] <- 0
table(BD4$CDI)

BD4$CA[BD4$ccontr==1] <- 1
BD4$CA[BD4$ccontr!=1] <- 0
table(BD4$CA)

BD4$CI[BD4$ccontr==2] <- 1
BD4$CI[BD4$ccontr!=2] <- 0
table(BD4$CI)

BD4$CS[BD4$ccontr==3] <- 1
BD4$CS[BD4$ccontr!=3] <- 0
table(BD4$CS)

BD4$PCT[BD4$ccontr==0] <- 1
BD4$PCT[BD4$ccontr!=0] <- 0
table(BD4$PCT)

table(BD4$cser1)
table(BD4$cser)
BD4$EMPL[BD4$cser==5] <- 1
BD4$EMPL[BD4$cser!=5] <- 0
table(BD4$EMPL)

BD4$OUV[BD4$cser==6] <- 1
BD4$OUV[BD4$cser!=6] <- 0
table(BD4$OUV)


BD4$CPI[BD4$cser==3] <- 1
BD4$CPI[BD4$cser!=3] <- 0
table(BD4$CPI)

BD4$CPI[BD4$cser==3] <- 1
BD4$CPI[BD4$cser!=3] <- 0
table(BD4$CPI)

BD4$PI[BD4$cser==4] <- 1
BD4$PI[BD4$cser!=4] <- 0
table(BD4$PI)

table(BD4$tuu2010r1)
table(BD4$tuu2010r)
BD4$K200KH[BD4$tuu2010r1=="Unité urbaine de 200k habitants ou plus"] <- 1
BD4$K200KH[BD4$tuu2010r1!="Unité urbaine de 200k habitants ou plus"] <- 0
table(BD4$K200KH)

BD4$APARIS[BD4$tuu2010r1=="Agglomeration parisienne"] <- 1
BD4$APARIS[BD4$tuu2010r1!="Agglomeration parisienne"] <- 0
table(BD4$APARIS)

BD4$K20K200K[BD4$tuu2010r1=="Unité urbaine de 20k à moins de 200k habitants"] <- 1
BD4$K20K200K[BD4$tuu2010r1!="Unité urbaine de 20k à moins de 200k habitants"] <- 0
table(BD4$K20K200K)

BD4$UM20K[BD4$tuu2010r1=="Unité urbaine de - de 20k habitants"] <- 1
BD4$UM20K[BD4$tuu2010r1!="Unité urbaine de - de 20k habitants"] <- 0
table(BD4$UM20K)

BD4$COMRU[BD4$tuu2010r1=="Communaute rurale"] <- 1
BD4$COMRU[BD4$tuu2010r1!="Communaute rurale"] <- 0
table(BD4$COMRU)


library("oaxaca")
table(BD4$origine)
results <- oaxaca(formula=log(salmee)~  sexe+NIV1+NIV2+NIV3+NIV4+ zus+pp+CDD+
                    CDI+CA+CI+CS+EMPL+OUV
                  +K200KH+APARIS+K20K200K+UM20K|origine,
                    data = BD4, R=100)
summary(results)

round(results$twofold$overall[,1:5], 3)
plot(results, decomposition = "twofold", group.weight = 1)
round(results$twofold$variables[[2]][,2:5] ,3)

sortie_tab("oxaca1.png", round(results$twofold$overall[,1:5], 3), 50,310)
sortie_tab("oxaca.png", round(results$twofold$variables[[2]][,2:5] ,6), 50,310)


# selon une nationalité pour les personnnes originaires de magrehb 
BD4$magh[BD4$nation==2] <- 1
BD4$magh[BD4$nation!=2] <- 0
table(BD4$nation)

mean(BD3[BD3$origine==0,]$salmee)-mean(BD3[BD3$origine==1,]$salmee)

logit<-glm(magh ~ sexe+NIV1+NIV2+NIV3+NIV4+ zus+pp+CDD+
             CDI+CA+CI+CS+EMPL+OUV
           +K200KH+APARIS+K20K200K+UM20K, family=binomial (link='logit'),
           data=BD4)
summary(logit)$coefficients





p<-predict(logit,type='response')
w1<-ifelse(BD4$magh==0,
           p/(1-p)*(1-mean(BD4$magh))/mean(BD4$magh), 1)



library(Hmisc)
grid<-seq(0.1,0.9,0.1)
ref<-BD4$magh==0
BD4$logsal <- log(BD4$salmee)
dfl.Fc<-wtd.quantile(BD4$logsal[ref], weights=w1[ref], probs=grid)


ref1<-BD4$magh==1
dfl.Fmagh<-wtd.quantile(BD4$logsal[ref1], weights=w1[ref1], probs=grid)


test <- rbind(dfl.Fc,dfl.Fmagh,dfl.FAFRI)

library(Hmisc)
#On calcule les d ́eciles 1 `a 9 de la distribution contrefactuelle #(la distribution dans le groupe B repond ́er ́ee)
dfl.Fc<-wtd.quantile(BD4$logsal[BD4$magh==0],
weights=w1[BD4$magh==0], probs=seq(0.1,0.9,0.1))
#A comparer aux d ́eciles de la distribution des salaires des B
dfl.FB<-wtd.quantile(BD4$logsal[BD4$magh==0],
                     probs=seq(0.1,0.9,0.1))
#Et `a ceux de la distribution des salaires des A
dfl.FA<-wtd.quantile(BD4$logsal[BD4$magh==1],
                     probs=seq(0.1,0.9,0.1))

plot(dfl.Fc, cex = 1, pch = 1, col = "red", main ="salaire selon l'origine")
lines(dfl.FB, col = "red")
lines(dfl.FA, col = "orange")
lines(dfl.Fc, col = "blue")
legend("topleft", legend = c("Pop non typé", "Magrhebe","Non typé repondé"), fill = c("red", "orange","blue"))



#ecart total 
round(dfl.FB-dfl.FA,3)

#dont effet de composition 
round(dfl.FB-dfl.Fc,3)


# ecart inexplique 
round(dfl.Fc-dfl.FA,3)

#### Répartition des migrants dans la France 

BD= BD[BD$reg!="01",]
BD= BD[BD$reg!="02",]
BD= BD[BD$reg!="03",]
BD= BD[BD$reg!="04",]
table(BD$reg)
table(FranceFormes$NAME_1)

BD$departement[BD$regio=="94"]="Corse" 
BD$departement[BD$regio=="52"]="Pays de la Loire" 
BD$departement[BD$regio=="11"]="Île-de-France" 
BD$departement[BD$regio=="53"]="Bretagne" 
BD$departement[BD$regio=="28"]="Normandie"
BD$departement[BD$regio=="93"]="Provence-Alpes-Côte d'Azur" 
BD$departement[BD$regio=="75"]="Nouvelle-Aquitaine"
BD$departement[BD$regio=="24"]="Centre-Val de Loire"
BD$departement[BD$regio=="27"]="Bourgogne-Franche-Comté"
BD$departement[BD$regio=="76"]="Occitanie"
BD$departement[BD$regio=="32"]="Hauts-de-France"
BD$departement[BD$regio=="44"]="Grand Est"
BD$departement[BD$regio=="84"]="Auvergne-Rhône-Alpes"
table(BD$departement)

table(BD3$immi1,BD3$origine1)
rm(BD)
#### carte de la france 
str(FranceFormes)
#Importation du package
library(raster)
table(BD$ORIGINE)
sum(table(BD$departement))
table <- table(BD$departement,BD$ORIGINE)
addmargins(table)
table<-data.frame(addmargins(table)) 
BD_TABLE <- data.frame(table$Var1,table$Freq)
BD_TABLE1<- BD_TABLE[-c(14:42),]
BD_TABLE2<- BD_TABLE[-c(1:14,28:42),-1]
BD_TABLE3 <- BD_TABLE[-c(1:28,42),-1]
BD_TABLET<- data.frame(BD_TABLE1,BD_TABLE2)
length(BD_TABLE3)
BD_TABLET$table.Freq <-100*BD_TABLET$table.Freq/9187
str(BD_TABLET)
BD_TABLET$table.Var1 <- as.character(BD_TABLET$table.Var1)

BD_TABLET


table(BD_TABLET$table.Var1)

#Découpage des régions avant 2015
FranceFormes <- getData(name="GADM", country="FRA", level=1)
plot(FranceFormes, main="Carte de la France, régions (avant 2015)")
idx <- match(FranceFormes$NAME_1, BD_TABLE1$table.Var1)
concordance <- BD[idx, "table.Freq"]
FranceFormes$immi1 <- BD_TABLE1$table.Freq/(sum(BD_TABLE1$table.Freq))*100



concordance <- BD[idx, "BD_TABLE2"]
FranceFormes$immi2 <- concordance

#établissemment de la charte des coupeurs puis tracage de la carte en utilisant
couleurs <- colorRampPalette(c('white', 'red'))
spplot(FranceFormes,"immi1",col.regions=couleurs(30), 
       main=list(label="Répartition des migrants en % dans la FRANCE",cex=.8))




table(BD4$origine1,BD4$tuu2010r1)

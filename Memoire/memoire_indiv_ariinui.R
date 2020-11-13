load("Base_individuel.RData")
save(X1, file = "Base_individuel.RData")

library(MASS)
library(ade4)
library(plotly)
table(X1$QP)

#########@
BD1 <- dichot1 
BD1$tram <- X1$tram
BD1$periode <- X1$periode
BD1$dcomiris <-X1$dcomiris
BD1$dcomiris <- as.factor(BD1$dcomiris)
BD1$QP <- X1$QP
BD1$ilt <- X1$ilt 
BD1$rech <- X1$rech 
BD1$tact <- X1$tact 
BD1$lib <- X1$libiris
BD1$cluster <- X1$cluster
BD1$cluster <- as.factor(BD1$cluster )
BD1$lib <- as.factor(BD1$lib)
BD1$effect <- BD1$periode * BD1$tram
try1 <- table(BD1$dcomiris)
try2 <- table(BD1$lib)
try2
#selectionner les individus ages entre 15-64 ans et chomeurs 
BDTEST <- BD1[BD1$agerev>=15 & BD1$agerev<=64 & BD1$tact=='12' ,]

#nouvelle variable si la personne est au chomage depuis plus d'un an.
BD1$searchm1 <- 0
BD1[BD1$rech=='2',]$searchm1 <-1
table(BDTEST$searchm1)
BD1$searchm2 <- 0
BD1[BD1$rech=='1',]$searchm2 <-1
table(BDTEST$searchm2)


#nombre de piece dans le logement nbpi
# NE18FR
# NE24FR
# NE3FR
# NE6FR  
# NENFR
# NPERR

var_cbin <- data.frame(X1[,c('job','nbpi',"nperr","nenfr","agerev")])
dichot <- acm.disjonctif(subset(X1,select=c("couple","cs1","dipl_15"
                                            ,"hlml","immi","inatc",'lprf'
                                            ,'sfm','surf','modv',"sexe","stat_conj", "stocd"
                                            ,'typmc','typmr','typfc'
                                            ,'voit')))
dichot1 <- cbind(var_cbin, dichot)
#dichot1 <- data.frame(dichot1[,-c(14,98:102)])


str(dichot1)
####
library(ggplot2)
dichot1[1:99] <- lapply(dichot1[1:99], as.factor)
dichot1[2:5] <- lapply(dichot1[2:5], as.numeric)

base1.illust=dichot1[,c(1)]
base1.actif=dichot1[,-c(1)]

dichot1$cdi <- X1$empl
dichot1$cdii <- 0
dichot1[dichot1$cdi=='16',]$cdii <- 1
table(dichot1$cdii)
#Glm 
modele<-glm((searchm1~1),data=BDTEST,family=binomial(logit)) 
modele.forward<- step(modele,scope=list(lower=~1,upper=~tram+periode+ effect+nbpi+nperr+ nenfr+agerev+
                                          couple.1+dipl_15.D+dipl_15.C+dipl_15.B+hlml.1
                                        +inatc.1+lprf.0+lprf.1+lprf.2+lprf.3+sexe.1+stat_conj.A+
                                          stocd.10+stocd.21 +stocd.22+stocd.23+typmc.1+typmc.2+typmc.3
                                        +typmr.11+typmr.12+typmr.20+typmr.31+typfc.1+typfc.2+voit.0
), data=BDTEST, direction ="forward")


#Nous appliquons la methode forward et nous trouvons ces variables
#searchm1 ~ agerev + voit.0 + lprf.3 + stocd.22 + lprf.1 + typfc.1 + 
#dipl_15.C + dipl_15.D + dipl_15.B + sexe.1 + stocd.10 + stocd.21 + 
#  stocd.23 + periode + inatc.1 + stat_conj.A

library(sandwich)
modele <- lm(searchm1~  tram + periode + effect +agerev + voit.0 + lprf.3 + stocd.22  + typfc.1 + 
               dipl_15.C + dipl_15.D + dipl_15.B + sexe.1 + stocd.10 + stocd.21 + 
               stocd.23  + inatc.1 + stat_conj.A
             ,data = BDTEST)



plot(y=modele$residuals^2, x=modele$fitted.values,
     ylab="Squared Residuals", xlab="Predicted probabilities")

vv <- vcovHC(modele, type="HC1")

coeftest(modele, vcov = vv)
summary(modele)

summary(modele)
table(BDTEST$tram,BDTEST$QP)

#test de chisq test
chisq.test(BDTEST$searchm1, BDTEST$tram)
chisq.test(BDTEST$searchm1, BDTEST$periode)
chisq.test(BDTEST$searchm1, BDTEST$effect)
t.test(BDTEST$agerev ~ BDTEST$tram)
chisq.test(BDTEST$tram, BDTEST$voit.0)
chisq.test(BDTEST$tram, BDTEST$lprf.3)
chisq.test(BDTEST$tram, BDTEST$stocd.22)
chisq.test(BDTEST$tram, BDTEST$typfc.1)
chisq.test(BDTEST$tram, BDTEST$dipl_15.C)
chisq.test(BDTEST$tram, BDTEST$dipl_15.D)
chisq.test(BDTEST$tram, BDTEST$dipl_15.B)
chisq.test(BDTEST$tram, BDTEST$sexe.1)
chisq.test(BDTEST$tram, BDTEST$stocd.10)
chisq.test(BDTEST$tram, BDTEST$stocd.21)
chisq.test(BDTEST$tram, BDTEST$stocd.23)
chisq.test(BDTEST$tram, BDTEST$inatc.1)
chisq.test(BDTEST$tram, BDTEST$stat_conj.A)





tab_coef <- summary(modele)[4]
library(xlsx)
write.xlsx(x = tab_coef, file = "tab_coef.xlsx")


library(lmtest)
bptest(modele)
# p-value = 0.0 - Non
#probleme attendu a cause de l'hétéroscédasticité des residus 
#en expliquant la variable à expliquer qui est binaire 


library(lmtest)
reset(modele)
#p-value = 0.74 
#forme fonctionnelle accepte

library(car)
vif(modele)




modele <- lm(searchm1~  tram + periode + effect 
             ,data = BDTEST)



summary(modele)
library(lmtest)
bptest(modele)
# p-value = 0.89 - Bon


library(lmtest)
reset(modele)
#p-value = 1

library(car)
vif(modele)



modele <- lm(searchm1~  tram + periode + effect +agerev      + 
               dipl_15.C + dipl_15.D + dipl_15.B + sexe.1  + inatc.1 +stat_conj.A+lprf.3+typfc.1
             ,data = BDTEST)

plot(y=modele$residuals^2, x=modele$fitted.values,
     ylab="Squared Residuals", xlab="Predicted probabilities")

vv <- vcovHC(modele, type="HC1")
coeftest(modele, vcov = vv)
summary(modele)

tab_coef <- summary(modele)[4]
library(xlsx)
write.xlsx(x = tab_coef, file = "tab_coef.xlsx")
library(lmtest)
bptest(modele)
#Non 


library(lmtest)
reset(modele)
#Bon

library(car)
vif(modele)


modele <- lm(searchm1~  tram + periode + effect +agerev      + 
               dipl_15.C + dipl_15.D + dipl_15.B + sexe.1  + inatc.1 +lprf.3+typfc.1+stat_conj.A
             ,data = BDTEST)

plot(y=modele$residuals^2, x=modele$fitted.values,
     ylab="Squared Residuals", xlab="Predicted probabilities")

vv <- vcovHC(modele, type="HC1")
coeftest(modele, vcov = vv)
summary(modele)

library(lmtest)
bptest(modele)
#Non


library(lmtest)
reset(modele)
#Bon

library(car)
vif(modele)


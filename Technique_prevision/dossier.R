#Importation de la base 
touriste <- read.csv("~/Desktop/Final_prevision/touriste.csv", sep="")
base <- touriste

#1) Serie saisonniere et analyse cyclique 
#-----
library(RJDemetra)
library(BCDating)
library(seasonal)
myseries <- base
myseries <- ts(data = myseries, start=c(2007,01),frequency=12)
plot(myseries, col = 'orange')
#detection de valeur atypique par boxplot 
boxplot(myseries)
library(outliers)
grubbs.test(base[,1])
#le test montre une valeur atypique pour 
#le mois de juillet 2018

#detection de saisonnalité avec seastests
library(seastests)
teststat <- ocsb(myseries, nrun=200)
check_residuals(teststat)
isSeasonal(myseries, test = "seasdum", freq = NA)
#By default, the WO-test is used to assess the seasonality of a time series and returns a boolean. 
#Alternatively, the QS test (test=’qs’), Friedman test (test=’fried’), Kruskall-Wallis (test=’kw’),
#F- test on seasonal dummies (test=’seasdum’) or the Welch test (test=’welch’) can be used.
#

#periodogramme 
library(TSA)


x13_model <- x13(myseries) # X-13ARIMA method  usrdef.outliersType = c("LS","AO")
(x13_model$regarima)
ts_model <- tramoseats(myseries) # TRAMO-SEATS method
(ts_model$regarima)
t <- regarima(myseries,ts_model)
resid <- ts_model$regarima$residuals
lynx.periodogram=periodogram(resid,ylab="Periodogramme")
layout(matrix(1:6,3,2));plot(t,ask=F)
plot(x13_model)
plot(ts_model)   
# Basic plot with the original series, the trend and the SA series
plot(x13_model, type_chart = "sa-trend")

#decomposition X13
plot(x13_model$regarima)

#tendance tramo
plot(ts_model)

#outliers 
# Automatic Procedure for Detection of Outliers
library(tsoutliers)
tso(myseries)

fit <- tso(myseries)
plot(fit)
show(fit)
fit

# outlier-adjusted series
adj <- fit$yadj
write(t(adj),file="ipi1984_TC.out",ncolumn=1,append=FALSE)

#regarima 
library(RJDemetra)
regarima_model <- regarima_def_x13(myseries, spec="RG4c")

#fonction jdemetra library("seasonal")
library("seasonal")
#TRAMO-SEATS
#montre une valeur atypique : Juillet 2010
te<-seas(
  x = myseries,
  outlier = NULL,
  outlier.critical = 3
)
te$data


#view avec interface jdemetra 
#library seasonal
m <- seas(myseries)
view(m)

#modele X13 avec seasonal 
test <- seas(
  x = myseries,
  na.action = na.x13,
  outlier = NULL,
  outlier.critical = 3
)

test$data
seasDATA <- m$data

#sauvegarde dans une fichier excel 
#les donnees de tendance de seat et X13
library(xlsx)
write.xlsx(x = seasDATA, file = "seasDATA.xlsx")

X13DATA <- x13_model$final$series
library(xlsx)
write.xlsx(x = X13DATA, file = "X13DATA.xlsx")

summary(m)
plot(m)
summary(x13_model$decomposition)


#modele X11 --> X13
test1<- seas(
  x = myseries,
  x11 = "",
  outlier.critical = 3,
  forecast.save = "fct"
)
test1$data
plot(test1)
test1$series$fct


#forecast
X13arima <- data.frame(test1$series$fct)
X13arima1 <- ts(data = X13arima$forecast[1:12], start=c(2018,10),frequency=12)
summary(test1)

#graphique X13 et SEAT
plot(final(test), col='blue')
lines(final(test1), col="red")
legend("bottomright", legend=c("SEAT", "X13-ARIMA"),
       col=c("blue","red"), lty=1:1, cex=0.8)

#graphique seat
plot(test)

#graphique X13
plot(test1)

#-----

#Désaisonnalisation et Analyse cyclique
#####
#Graphique série CVS et Trend.
#Dater les points de retournement du cycle des affaires de la série 
plot(x13_model)
df_x13<-data.frame(x13_model$final$series)

#serie avec tendance pour X13
x13_trendcycle <- df_x13$t

#serie cvs
lines(ts_model$final$series[,2])
lines(x13_model$final$series[,2])
plot(myseries, col='orange', main= 'Désaisonnalisation de la série brute avec X13-TS')
lines(ts_model$final$series[,2], col ='red')
lines(x13_model$final$series[,2], col='blue')
legend("bottomright", legend=c('Série brute',"SEAT", "X13-ARIMA"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)

plot(myseries, col='orange', main= 'Désaisonnalisation de la série brute avec X13-TS',
     xlim=c(2008,2011))
lines(ts_model$final$series[,2], col ='red')
lines(x13_model$final$series[,2], col='blue')
legend("topright", legend=c('Série brute',"SEAT", "X13-ARIMA"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)

plot(myseries, col='orange', main= 'Désaisonnalisation de la série brute avec X13-TS',
     xlim=c(2014,2018))
lines(ts_model$final$series[,2], col ='red')
lines(x13_model$final$series[,2], col='blue')
legend("topright", legend=c('Série brute',"SEAT", "X13-ARIMA"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)


x13_sa <- df_x13$sa

#mettre en serie temporelle
x13_trendcycle<-ts(data = x13_trendcycle, start=c(2007,01),frequency=12)
x13_sa<-ts(data = x13_sa, start=c(2007,01),frequency=12)
plot(x13_trendcycle)


#de même pour TRAMO-SEATS
df_ts<-data.frame(ts_model$final$series)
ts_trendcycle <- df_ts$t
ts_trendcycle<-ts(data = ts_trendcycle, start=c(2007,01),frequency=12)
ts_sa <- df_ts$sa
ts_sa<-ts(data = ts_sa, start=c(2007,01),frequency=12)
plot(ts_trendcycle)


plot(myseries, col='orange', main= 'CVS et trend - X13')
lines(x13_model$final$series[,2], col ='red')
lines(x13_model$final$series[,3], col='blue')
legend("bottomright", legend=c('Série brute',"CVS", "trend"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)

plot(myseries, col='orange', main= 'Tendance X13 - TS')
lines(x13_model$final$series[,3], col ='red')
lines(ts_model$final$series[,3], col='blue')
legend("bottomright", legend=c('Série brute',"X13", "TS"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)

plot(myseries, col='orange', main= 'trend - X13 et TS')
lines(x13_model$final$series[,3], col ='red')
lines(ts_model$final$series[,3], col='blue')
legend("bottomright", legend=c('Série brute',"trend X13", "trend TS"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)


plot(x13_model$final$series[,2], xlim=c((2010+3/12),(2011+1/12)),ylim=c(11000,15000))

min(x13_model$final$series[,2])
# Bry-Boschan algorithm sans les paramètres 
library(BCDating)
#graphique BB + TRAMO-SEAT CVS
par(mfrow = c(2,1))
datts_sa <- BBQ(ts_sa, name="Dating Business Cycles")
show(datts_sa)
summary(datts_sa)
p1 <- plot(ts_sa, col = 'red')
p2 <- plot(datts_sa)


#graphique BB + X13 CVS
datx13_sa<- BBQ(x13_sa, name="Dating Business Cycles")
show(datx13_sa)
summary(datx13_sa)
p3 <- plot(x13_sa, col ='blue')
p4 <- plot(datx13_sa)


#graphique BB + X13 tendance
datx13_trendcycle <- BBQ(x13_model$final$series[,3], name="Dating Business Cycles")
show(datx13_trendcycle)
summary(datx13_trendcycle)
p1 <- plot(x13_model$final$series[,3], col = 'pink')
p2 <- plot(datx13_trendcycle)

datx13_trendcycle
#graphique BB + TRAMO SEAT tendance
datts_trendcycle <- BBQ(ts_model$final$series[,3], name="Dating Business Cycles")
show(datts_trendcycle)
summary(datts_trendcycle)
p3 <- plot(ts_model$final$series[,3], col ='green')
p4 <- plot(datts_trendcycle)



#X13 + TREND
pyi <- BBQ(x13_model$final$series[,3], mincycle = 15, minphase = 6)
show(pyi)
summary(pyi)

plot(x13_model$final$series[,3],col='orange')
p4 <- plot(pyi, main='X13-TREND')


#graphique BB + X13 + TS sur trend et CVS 
# avec les parametre 
par(mfrow = c(4,1))
pyi <- BBQ(x13_model$final$series[,2], mincycle = 15, minphase = 6)
show(pyi)
summary(pyi)
p4 <- plot(pyi, main='X13-CVS')

pyi <- BBQ(x13_model$final$series[,3], mincycle = 15, minphase = 6)
show(pyi)
summary(pyi)
p4 <- plot(pyi, main='X13-TREND')

pyi <- BBQ(ts_model$final$series[,2], mincycle = 15, minphase = 6)
summary(pyi)
show(pyi)
p4 <- plot(pyi, main='TS-CVS')

pyi <- BBQ(ts_model$final$series[,3], mincycle = 15, minphase = 6)
summary(pyi)
show(pyi)
p4 <- plot(pyi, main='TS-TREND')



library(mFilter)
yy<- myseries

#fonction HP w/ X13 CVS

filterCVSX13 <- hpfilter(x13_model$final$series[,2], 13.9)
plot(filterCVSX13)
par(mfrow = c(2,1))
#graphique HP+BBQ X13
dat<- BBQ(filterCVSX13$trend,mincycle = 15, minphase = 6)
show(dat)
summary(dat)
plot(filterCVSX13$trend, col='green')
p5 <- plot(dat)

# HP filter with lambda = 13.9
library(timeSeries)

#fonction HP w/ X13 CVS
diff_x13_sa <- diff(x13_model$final$series[,2],lag=T,difference=1) 
plot(diff_x13_sa, col= 'green', main= 'Première différenciantion de la série CVS X13')
filterX13 <- hpfilter(diff_x13_sa,13.9)
plot(filterX13)
summary(filterX13)

#fonction HP w/ X13 CVS
diff_ts_sa <- diff(ts_model$final$series[,2], lag=T, difference=1)
plot(diff_ts_sa)
filterts <- hpfilter(diff_ts_sa, 13.9)
plot(filterts)

par(mfrow = c(2,1))
#graphique HP+BBQ X13
datHPX13<- BBQ(filterX13$trend,mincycle = 15, minphase = 6)
show(datHPX13)
summary(datHPX13)
plot(filterX13$trend, col='orange')
p5 <- plot(datHPX13)

#graphique HP+BBQ TRAMO-S
datHPTS<- BBQ(filterts$trend,mincycle = 15, minphase = 6)
show(datHPTS)
summary(datHPTS)
plot(filterts$trend, col='blue',main = '')
p5 <- plot(datHPTS)

library(seasonal)
library(BCDating)
library(mFilter)
#cycle acceleration avec lambda = 14400
#CVS provenant de la fonction seas
#serie normale 
seasX <- seas(yy)
trendcycle <- trend(seasX)
# Acceleration cycle
cvs <- final(seasX)
plot(cvs)
diffcvs <- diff(cvs, difference=1)

filter <- hpfilter(diffcvs,13.9)
plot(filter)
summary(filter)
datHP<- BBQ(filter$trend,mincycle = 18, minphase = 9)
show(datHP)
summary(datHP)



filter <- hpfilter(diffcvs,14400)
plot(filter)
summary(filter)
datHP<- BBQ(filter$trend,mincycle = 18, minphase = 9)
show(datHP)
summary(datHP)
#plot(filter$trend, col='red')
p5 <- plot(datHPX13, main ='cycle accélération lambda = 13.9')
p5 <- plot(datHP, main ='cycle accélération lambda = 14400')

cvs <- final(seasX)
plot(cvs)
diffcvs <- diff(cvs, difference=1)
filter1 <- hpfilter(diffcvs,129600)
plot(filter1)
summary(filter1)
datHP1<- BBQ(filter1$trend,mincycle = 18, minphase = 9)
show(datHP1)
summary(datHP1)
plot(filter1$trend, col='orange')
p5 <- plot(datHP)

par(mfrow = c(1,1))

#graphique avec different lambda diffcvs
plot(filterX13$trend, col='orange', main= '')
lines(filter$trend, col ='red')
lines(filter1$trend, col='blue')

legend("bottomright", legend=c('lambda = 13.9',"lambda = 144 00", 
                               "lambda = 129 600"),
       col=c('orange',"red","blue"), lty=1:1, cex=0.8)
#####

plot(filter$trend, col ='red')


#2)Prevision 
#------
library(forecast)
training <- window(myseries, end = c(2017+(8/12), 1))
test <- window(myseries, start = c(2017+9/12, 1))

#avoir une idée de la modelisation sans prevision 
mod_X13 = x13(training)
print(mod_X13)
mod_arima <- Arima(training,order=c(0,1,1),
                   seasonal=list(order=c(0,1,1),period=12),lambda=0)
mod_autoarima = auto.arima(training, ic='aicc', stepwise=FALSE)
summary(mod_naive)
mod_nnetar = nnetar(training, p=12, size=25)
mod_tbats = tbats(training, ic='aicc', seasonal.periods=12)
mod_bats = bats(training, ic='aicc', seasonal.periods=12)
mod_stlm = stlm(training, s.window=12, ic='aicc', robust=TRUE, method='ets')
mod_hw<-HoltWinters(training, seasonal='mul')
mod_naive = snaive(training)


#Prevision pas à pas 
#on fait une boucle et on incompare la nouvelle valeur 
#au fur et a mesure dans la serie pendant 12 mois 

#PREVISION X13
y_X13<-training
for (i in 1:12) {
  y_X13 <- y_X13[1:(128+i)]
  y_X13<-ts(data = y_X13, start=c(2007,01),frequency=12)
  model <-  x13(y_X13)  	# one-step ahead forecast
  w <-  model$final$forecasts[1,1]
  y_X13<-y_X13[1:(129+i)]
  y_X13[(129+i)] <- w
}
FCX13 <- y_X13[130:141]
FCX13<-ts(data = FCX13, start=c(2017,10),frequency=12)

#PREVISION TRAMO-SEATS
y_ts<-training
for (i in 1:12) {
  y_ts <- y_ts[1:(128+i)]
  y_ts<-ts(data = y_ts, start=c(2007,01),frequency=12)
  model <-  tramoseats(y_ts)  	# one-step ahead forecast
  w <-  model$final$forecasts[1,1]
  y_ts<-y_ts[1:(129+i)]
  y_ts[(129+i)] <- w
}
FCts <- y_ts[130:141]
FCts<-ts(data = FCts, start=c(2017,10),frequency=12)



#PREVISION ARIMA
y_arima<-training
for (i in 1:12) {
  y_arima <- y_arima[1:(128+i)]
  model <-  Arima(y_arima,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),lambda=0)  	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_arima[(129+i)] <- w$mean
}
FCarima <- y_arima[130:141]
FCarima<-ts(data = FCarima, start=c(2017,10),frequency=12)


#PREVISION AUTO-ARIMA
y_autoarima<-training
for (i in 1:12) {
  y_autoarima <- y_autoarima[1:(128+i)]
  model <-  auto.arima(y_autoarima, ic='aicc', stepwise=FALSE)  	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_autoarima[(129+i)] <- w$mean
}
FCauto<- y_autoarima[130:141]
FCauto<-ts(data = FCauto, start=c(2017,10),frequency=12)


#PREVISION ETS
y_ets<-training
for (i in 1:12) {
  y_ets <- y_ets[1:(128+i)]
  model <-  ets(y_ets)  	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_ets[(129+i)] <- w$mean
}
FCets <-  y_ets[130:141]
FCets<-ts(data = FCets, start=c(2017,10),frequency=12)


#PREVISION NNETAR
y_nnetar<-training
for (i in 1:12) {
  y_nnetar <- y_nnetar[1:(128+i)]
  y_nnetar<-ts(data = y_nnetar, start=c(2007,01),frequency=12)
  model <-  nnetar(y_nnetar, p=12, size=25)  	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_nnetar<-y_nnetar[1:(129+i)]
  y_nnetar[(129+i)] <- w$mean
}
FCnnetar <-  y_nnetar[130:141]
FCnnetar<-ts(data = FCnnetar, start=c(2017,10),frequency=12)


#PREVISION TBATS
y_tbats<-training
for (i in 1:12) {
  y_tbats <- y_tbats[1:(128+i)]
  y_tbats<-ts(data = y_tbats, start=c(2007,01),frequency=12)
  model <-  tbats(y_tbats, ic='aicc', seasonal.periods=12) 	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_tbats<-y_tbats[1:(129+i)]
  y_tbats[(129+i)] <- w$mean
}
FCtbats <-  y_tbats[130:141]
FCtbats<-ts(data = FCtbats, start=c(2017,10),frequency=12)


#PREVISION BATS
y_bats<-training
for (i in 1:12) {
  y_bats <- y_bats[1:(128+i)]
  y_bats<-ts(data = y_bats, start=c(2007,01),frequency=12)
  model <-  bats(y_bats, ic='aicc', seasonal.periods=12) 	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_bats<-y_bats[1:(129+i)]
  y_bats[(129+i)] <- w$mean
}
FCbats <-  y_bats[130:141]
FCbats<-ts(data = FCbats, start=c(2017,10),frequency=12)

#PREVISION STLM 
y_stlm<-training
for (i in 1:12) {
  y_stlm <- y_stlm[1:(128+i)]
  y_stlm<-ts(data = y_stlm, start=c(2007,01),frequency=12)
  model <-  stlm(y_stlm, s.window=12, ic='aicc', robust=TRUE) 	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_stlm<-y_stlm[1:(129+i)]
  y_stlm[(129+i)] <- w$mean[1]
}
FCstlm <-  y_stlm[130:141]
FCstlm<-ts(data = FCstlm, start=c(2017,10),frequency=12)

#PREVISION HOLT-WINTERS
y_HW<-training
for (i in 1:12) {
  y_HW <- y_HW[1:(128+i)]
  y_HW<-ts(data = y_HW, start=c(2007,01),frequency=12)
  model <-  HoltWinters(y_HW, seasonal='mul')	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_HW<-y_HW[1:(129+i)]
  y_HW[(129+i)] <- w$mean
}
FCHW <-  y_HW[130:141]
FCHW<-ts(data = FCHW, start=c(2017,10),frequency=12)

#PREVISION HOLT-WINTERS
y_HWad<-training
for (i in 1:12) {
  y_HWad <- y_HWad[1:(128+i)]
  y_HWad<-ts(data = y_HWad, start=c(2007,01),frequency=12)
  model <-  HoltWinters(y_HWad, seasonal='add')	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_HWad<-y_HWad[1:(129+i)]
  y_HWad[(129+i)] <- w$mean
}
FCadHW <-  y_HWad[130:141]
FCadHW<-ts(data = FCadHW, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(FCHW, col='red')
lines(FCadHW, col='blue')

#PREVISION NAIVE
y_na<-training
for (i in 1:12) {
  y_na <- y_na[1:(128+i)]
  y_na<-ts(data = y_na, start=c(2007,01),frequency=12)
  model <-  snaive(y_na)	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_na<-y_na[1:(129+i)]
  y_na[(129+i)] <- w$mean[1]
}
FCna <-  y_na[130:141]
FCna<-ts(data = FCna, start=c(2017,10),frequency=12)


#METTRE DANS UN GRAPHIQUE LES NOUVELLES PREVISION 
plot(myseries, col="orange", xlim=c(2017,2019),
     main = 'Prevision')
lines(FCX13,col=1)
lines(FCts, col=2)

legend("bottomright", legend=c('Série brute',"X13", "TRAMO-SEATS"),
       col=c("orange", 1:2), lty=1:1, cex=0.8)


plot(myseries, col="orange", xlim=c(2017,2019),
     main = 'Prevision')
lines(FCarima,col=1)
lines(FCauto, col=2)
lines(FCets, col=3)
legend("bottomright", legend=c('Série brute',"SARIMA", "AUTO-SARIMA",
                               "ETS"),
       col=c("orange", 1:3), lty=1:1, cex=0.8)

plot(myseries, col="orange", xlim=c(2017,2019),
     main = 'Prevision')
lines(FCnnetar, col=4)
lines(FCtbats, col=5)
lines(FCbats, col=6)
legend("bottomright", legend=c("Série Brute", "NNETAR",
                               "TBATS", "BATS"),
       col=c("orange", 4:6), lty=1:1, cex=0.8)

plot(myseries, col="orange", xlim=c(2017,2019),
     main = 'Prevision')
lines(FCstlm, col=7)
lines(FCHW, col=8)
lines(FCna, col=10)

legend("bottomright", legend=c("Série brute", "STLM",
                               "Holt-Winters", "Naive"),
       col=c("orange", 7:8,10), lty=1:1, cex=0.8)

plot(myseries, col="orange", xlim=c(2017,2019),
     main = 'Prevision')
lines(FCarima, col='red')
lines(FCna, col='pink')


FCarima

#CALCUL DES ERREURS DE PREVISIONS 
erreur <-list(
  erARIMA <- FCarima-test,
  erAUTO <- FCauto -test,
  erETS <- FCets -test,
  erNNETAR <- FCnnetar - test,
  ertbats <- FCtbats -test,
  erbats <- FCbats -test,
  erstlm <- FCstlm -test,
  erHW <- FCHW-test ,
  erna <- FCna-test,
  erX13 <- FCX13-test)

FC <- cbind(FCarima, FCauto,FCets,
            FCnnetar, FCtbats ,FCbats,
            FCstlm,FCHW, FCna, 
            FCX13)
write.xlsx(x = FC, file = "FC.xlsx")
library(Metrics)
rmse((farima$mean), test)

#CREATION D'UNE FONCTION 
#DE CALCUL DES TROIS MESURES DE PREVISION 
ERROR <- function(x,y){
  me<-mean(x)
  mae<-mean(abs(x))
  mse <-rmse(x+test, test)
  
  return(data.frame(rbind(me,mae,mse)))
}

tab_err <- lapply(erreur, ERROR)
tab_err <- lapply(tab_err, round,2)
write.xlsx(x = tab_err, file = "er_pas.xlsx")


#dotchart pour identifier clairement les valeurs minimum et 
#maximum des trois mesures de prevision
tab_erreur <- read.csv("~/Desktop/Final_prevision/tab_erreur.csv", row.names=1, sep=";")
tab_erreur<-t(tab_erreur)
dotchart(tab_erreur[1,], pch = 17, main = "Erreur moyenne")
dotchart(tab_erreur[2,], pch = 19, main = "Erreur quadratique moyenne")
dotchart(tab_erreur[3,], pch = 4, main = "Erreur absolue moyenne")


#test dm 
#TEST DM

#DM TEST 
#----
erreur <-list(
  erARIMA <- FCarima-test,
  erAUTO <- FCauto -test,
  erETS <- FCets -test,
  erNNETAR <- FCnnetar - test,
  ertbats <- FCtbats -test,
  erbats <- FCbats -test,
  erstlm <- FCstlm -test,
  erHW <- FCHW-test ,
  erna <- FCna-test,
  erX13 <- FCX13-test)

#DM TEST 
dm.test(erARIMA,erna,h=1)
dm.test(erAUTO,erna,h=1)
dm.test(erETS,erna,h=1)
dm.test(erNNETAR,erna,h=1)
dm.test(ertbats,erna,h=1)
dm.test(erbats,erna,h=1)
dm.test(erstlm,erna,h=1)
dm.test(erHW,erna,h=1)
dm.test(erX13,erna,h=1)

dm.test(erARIMA,erX13,h=1)
dm.test(erAUTO,erX13,h=1)
dm.test(erETS,erX13,h=1)
dm.test(erNNETAR,erX13,h=1)
dm.test(ertbats,erX13,h=1)
dm.test(erbats,erX13,h=1)
dm.test(erstlm,erX13,h=1)
dm.test(erHW,erX13,h=1)

dm.test(erARIMA,erHW,h=1)
dm.test(erAUTO,erHW,h=1)
dm.test(erETS,erHW,h=1)
dm.test(erNNETAR,erHW,h=1)
dm.test(ertbats,erHW,h=1)
dm.test(erbats,erHW,h=1)
dm.test(erstlm,erHW,h=1)

dm.test(erARIMA,erstlm,h=1)
dm.test(erAUTO,erstlm,h=1)
dm.test(erETS,erstlm,h=1)
dm.test(erNNETAR,erstlm,h=1)
dm.test(ertbats,erstlm,h=1)
dm.test(erbats,erstlm,h=1)

dm.test(erARIMA,erbats,h=1)
dm.test(erAUTO,erbats,h=1)
dm.test(erETS,erbats,h=1)
dm.test(erNNETAR,erbats,h=1)
dm.test(ertbats,erbats,h=1)

dm.test(erARIMA,ertbats,h=1)
dm.test(erAUTO,ertbats,h=1)
dm.test(erETS,ertbats,h=1)
dm.test(erNNETAR,ertbats,h=1)

dm.test(erARIMA,erNNETAR,h=1)
dm.test(erAUTO,erNNETAR,h=1)
dm.test(erETS,erNNETAR,h=1)

dm.test(erARIMA,erETS,h=1)
dm.test(erAUTO,erETS,h=1)

dm.test(erARIMA,erAUTO,h=1)

erreur <-list(
  erARIMA <- FCarima-test,
  erAUTO <- FCauto -test,
  erETS <- FCets -test,
  erNNETAR <- FCnnetar - test,
  ertbats <- FCtbats -test,
  erbats <- FCbats -test,
  erstlm <- FCstlm -test,
  erHW <- FCHW-test ,
  erna <- FCna-test,
  erX13 <- FCX13-test)
FCX13 <- erX13+test

#faire une matrice avec toutes les previsions 
fore<- data.frame(cbind(FCarima,FCauto,FCets,FCnnetar,FCtbats,FCbats
                        ,FCstlm,FCHW,FCna,FCX13))
fore <- t(fore) #transposer le dataframe 
fore1 <- as.matrix(fore, rownames.force = TRUE) #creation de la matrice 


library(multDM)

ts <- test[1:12] #mettre en vecteur 
forecasts <- fore1
l <- loss(realized=ts,evaluated=forecasts,loss.type="SE")
d <- d_t(l)
q <- TB_MA(d=d,q.max=1)
MDM.selection(realized=ts,evaluated=forecasts,q=q,alpha=0.1,statistic="Sc",loss.type="SE")


#-----

#importation de la base 
#----
serie_expli <- read.csv("~/Desktop/Final_prevision/serie_expli.csv", sep=";")
df <- serie_expli
#differenciation pour toutes les series dans la base
df_diff <- lapply(df[,1:9],function(x){diff(x, differences = 1)})
df_diff <- data.frame(df_diff)
plot(df_diff$Txch,col='orange', main ="Série brute avec différenciation de degrès 1")
plot(df_diff$CVT,col='grey')
plot(df_diff$GOOGLEUSA,col='grey')
#----



#PARTIE 3 AVEC SERIE NON SAISONNIERE 
#SERIE BRUTE ETUDIER, LE TAUX DE CHANGE 

#etude de la serie 
#----
#A etB 
#----
txchange <- read.csv("~/Desktop/Final_prevision/txchange.csv", sep=";")
library(RJDemetra)
library(BCDating)
library(seasonal)
myseries <- txchange$Txch
myseries <- ts(data = myseries, start=c(2007,01),frequency=12)
#boxplot et test Grubbs 
#detection de valeur atypique par boxplot 
boxplot(myseries)
library(outliers)
grubbs.test(txchange$Txch)

shapiro.test(txchange$Txch)

x13_model <- x13(myseries) # X-13ARIMA method  usrdef.outliersType = c("LS","AO")
ts_model <- tramoseats(myseries) # TRAMO-SEATS method
t <- regarima(myseries,x13_model)
plot(myseries)
plot(myseries, col= 2)


# Basic plot with the original series, the trend and the SA series
plot(x13_model, type_chart = "sa-trend")

# S-I ratio
plot(x13_model$regarima)
plot(x13_model$regarima)
plot(x13_model)
plot(ts_model)


#outliers 
# Automatic Procedure for Detection of Outliers
library(tsoutliers)

fit <- tso(myseries)
plot(fit$outliers)
show(fit)

#pas de valeurs atypique en utilisant tso

# outlier-adjusted series
adj <- fit$yadj
plot(adj)
write(t(adj),file="ipi1984_TC.out",ncolumn=1,append=FALSE)

library("seasonal")
te<-seas(
  x = myseries,
  outlier = NULL,
  outlier.critical = 3
)
te

m <- seas(myseries)
view(m)
#detection des valeurs atypique avec TRAMO 
# il y a 4 valeurs atypique avec un seuil de outliers à 4 
#decembre 2009 , Mai 2011, octobre 2011 et Mai 2016
#-----


#prevision 
#-----
#library de la prevision 
library(FitARMA)
library(lgarch) 
library(gets)
library(forecast)
#training entre oct 2017 et Sep 2018
myseries <- df_diff$Txch
myseries <- myseries[1:140]
myseries<-ts(data = myseries, start=c(2007,02),frequency=12)
plot(myseries,col='orange',main='TC première différenciation')
training <- window(myseries, end = c(2017+(8/12), 1))
test <- window(myseries, start = c(2017+9/12, 1))

library(normtest)
# ARX model with AR(1)
Mod_arx <- arx(training, mc = T, ar = 1, vcov.type = "ordinary",normality.JarqueB
=T) 
(Mod_arx)
plot(Mod_arx)

t <- residuals(Mod_arx)
plot(training, col='orange')
lines(t,col='red')
legend("bottomright", legend=c("Série I(1)", "rédisus"),
                         col=c('orange','red'), lty=1:1, cex=0.8)
#correlogramme
par(mfrow = c(2,1))
fac  <- acf(residuals(Mod_arx))
facp <- pacf(residuals(Mod_arx))
par(mfrow = c(1,1))
plot(t)
par(mfrow = c(1,2))
hist(t)
JarqueBeraTest(t)
qqnorm(t,datax=TRUE)
qqline(t,datax=TRUE, col ='red')



mod_ar=arima(training,order=c(1,0,0))
plot(mod_ar)
t <- residuals(mod_ar)
plot(training, col='orange')
lines(t,col='red')
legend("bottomright", legend=c("Série I(1)", "rédisus"),
       col=c('orange','red'), lty=1:1, cex=0.8)
#correlogramme
par(mfrow = c(2,1))
fac  <- acf(t)
facp <- pacf(t)
par(mfrow = c(1,1))
plot(t)
par(mfrow = c(1,2))
hist(t)
JarqueBeraTest(t)
qqnorm(t)
qqline(t,col ='red')


mod_arima=arima(training,order=c(1,0,1))
plot(mod_arima)
t <- residuals(mod_arima)
#correlogramme
par(mfrow = c(2,1))
fac  <- acf(t)
facp <- pacf(t)
par(mfrow = c(1,1))
plot(t)
par(mfrow = c(1,2))
hist(t)
JarqueBeraTest(t)
qqnorm(t)
qqline(t,col ='red')

mod_auto =auto.arima(training)
plot(mod_auto)
t <- residuals(mod_auto)
#correlogramme
par(mfrow = c(2,1))
fac  <- acf(t)
facp <- pacf(t)
par(mfrow = c(1,1))
plot(t)
par(mfrow = c(1,2))
hist(t)
JarqueBeraTest(t)
qqnorm(t)
qqline(t,col ='red')


mod_noHW = HoltWinters(training, gamma=F) 
mod_noHW
plot(mod_noHW)
t <- residuals(mod_noHW)
mod_noHW
#correlogramme
par(mfrow = c(2,1))
fac  <- acf(t)
facp <- pacf(t)
par(mfrow = c(1,1))
plot(t)
par(mfrow = c(1,2))
hist(t)
JarqueBeraTest(t)
qqnorm(t)
qqline(t, col ='red')

plot(residuals(mod_ar))

#graphique residu avec la serie I(1)
plot(training,col='orange', main ='modele AR(1) résidu et fitted')
lines(residuals(Mod_arx), col=2)
lines(fitted(Mod_arx), col=8)
fitted(Mod_arx)

plot(training,col='orange', main ='modele ARIMA(1,1,0) résidu')
lines(residuals(mod_ar), col=3)

plot(training,col='orange' , main='modele AUTO ARIMA résidu')
lines(residuals(mod_auto), col=4)

plot(training,col='orange', main = 'modele ARIMA(1,1,1) résidu')
lines(residuals(mod_arima), col=5)

plot(training,col='orange' , main ='modele Holt-Winters LED résidu')
lines(residuals(mod_noHW), col=6)


#prevision pas a pas avec la serie non saisonniere 
ERROR <- function(x,y){
  me<-mean(x)
  mae<-mean(abs(x))
  mse <-rmse(x+test, test)
  
  return(data.frame(rbind(me,mae,mse)))
}

#prevision TRAMO SEATS 
y_nots<-training
for (i in 1:12) {
  y_nots <- y_nots[1:(127+i)]
  y_nots<-ts(data = y_nots, start=c(2007,01),frequency=12)
  model <-  x13(y_nots)  	# one-step ahead forecast
  w <-  model$final$forecasts[1,1]
  y_nots<-y_nots[1:(128+i)]
  y_nots[(128+i)] <- w
}
FCnots <- y_nots[129:140]
FCnots<-ts(data = FCnots, start=c(2017,10),frequency=12)

#PREVISION ARIMA(1,0,1)
y_noar<-training
y_upnoar<- NA
y_lownoar<- NA
for (i in 1:12) {
  y_noar <- y_noar[1:(127+i)]
  y_noar<-ts(data = y_noar, start=c(2007,01),frequency=12)
  model <-  arima(y_noar,order=c(1,0,0))  	# one-step ahead forecast
  w <-  forecast(model, h=1)
  y_noar<-y_noar[1:(128+i)]
  y_noar[(128+i)] <- w$mean
  y_upnoar[(128+i)] <- w$upper[2]
  y_lownoar[(128+i)] <- w$lower[2]
}
FCnoar <- y_noar[129:140]
FCnoar<-ts(data = FCnoar, start=c(2017,10),frequency=12)
y_upnoar<- ts(data = y_upnoar[129:140], start=c(2017,10),frequency=12)
y_lownoar<- ts(data = y_lownoar[129:140], start=c(2017,10),frequency=12)
plot(training,col=1, main= 'Prévision non saisonnière',ylim=c(min(y_lownoar), max(y_upnoar))
     ,xlim=c(2017,2019))
lines(FCnoar,col = 'red')
lines(y_upnoar,col = 'blue')
lines(y_lownoar,col = 'blue')

#AR(1)
y_ar1<-training
for (i in 1:12) {
  y_ar1 <- y_ar1[1:(127+i)]
  y_ar1<-ts(data = y_ar1, start=c(2007,01),frequency=12)
  model <-  arx(y_ar1, mc = T, ar = 1, vcov.type = "ordinary")  	# one-step ahead forecast
  w <-  predict(model, h=1)
  y_ar1<-y_ar1[1:(128+i)]
  y_ar1[(128+i)] <- w[2]
}
FCar1<- y_ar1[129:140]
FCar1<-ts(data = FCar1, start=c(2017,10),frequency=12)






y_noarcima<-training
for (i in 1:12) {
  y_noarcima <- y_noarcima[1:(127+i)]
  y_noarcima<-ts(data = y_noarcima, start=c(2007,01),frequency=12)
  model <-  arima(y_noarcima,order=c(1,0,1))  	# one-step ahead forecast
  w <-  predict(model, h=1)
  y_noarcima<-y_noarcima[1:(128+i)]
  y_noarcima[(128+i)] <- w$pred
}
FCnoarima <- y_noarcima[129:140]
FCnoarima<-ts(data = FCnoarima, start=c(2017,10),frequency=12)



y_noauto<-training
for (i in 1:12) {
  y_noauto <- y_noauto[1:(127+i)]
  y_noauto<-ts(data = y_noauto, start=c(2007,01),frequency=12)
  model <-  auto.arima(training)  	# one-step ahead forecast
  w <-  forecast(model, h=1)
  y_noauto<-y_noauto[1:(128+i)]
  y_noauto[(128+i)] <- w$mean
}
FCnoauto <- y_noauto[129:140]
FCnoauto<-ts(data = FCnoauto, start=c(2017,10),frequency=12)


y_noHW<-training[1:128]
y_noHW 
for (i in 1:12) {
  y_noHW <- y_noHW[1:(127+i)]
  y_noHW<-ts(data = y_noHW, start=c(2007,02),frequency=12)
  model <-  HoltWinters(y_noHW, gamma=F)   	# one-step ahead forecast
  w <-  forecast(model, h=1)
  y_noHW<-y_noHW[1:(128+i)]
  y_noHW[(128+i)] <- w$mean
}
FCnoHW <- y_noHW[129:140]
FCnoHW<-ts(data = FCnoHW, start=c(2017,10),frequency=12)

y_nona<-training
for (i in 1:12) {
  y_nona <- y_nona[1:(127+i)]
  y_nona<-ts(data = y_nona, start=c(2007,01),frequency=12)
  model <-  snaive(y_nona)	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_nona<-y_nona[1:(128+i)]
  y_nona[(128+i)] <- w$mean[1]
}
FCnona <-  y_nona[129:140]
FCnona<-ts(data = FCnona, start=c(2017,10),frequency=12)

plot(test,col='orange', main= 'Prévision non saisonnière')
lines(FCnots,col = 2)
lines(FCnoar,col = 3)
lines(FCnoarima,col = 4)
legend("bottomleft", legend=c("Série brute", "TRAMO",
                              "ARIMA(1,1,0)",
                              "ARIMA(1,1,1)"),
       col=c('orange', 4:5), lty=1:1, cex=0.8)


plot(test,col='orange')
lines(FCar1,col = 2)
lines(FCnoauto,col = 3)
lines(FCnoHW,col = 4)
lines(FCnona,col=5)
legend("bottomleft", legend=c("Série I(1)","AR(1)","AUTO ARIMA","Holt-Winters","naive"),
       col=c('orange',2:5), lty=1:1, cex=0.8)


#mettre les residus dans une liste 
RESI_NO<- list(resi_nots <- FCnots-test,
               resi_noar <- FCnoar-test,
               resi_noarima <- FCnoarima-test,
               resi_noauto <- FCnoauto -test,
               resi_NOHW <- FCnoHW-test,
               resi_ar1 <- FCar1 -test,
               resi_NONA <- FCnona-test)

#mettre les residus dans un vecteur 
tabRESI_NO<- c(resi_nots <- FCnots-test,
               resi_noar <- FCnoar-test,
               resi_noarima <- FCnoarima-test,
               resi_noauto <- FCnoauto -test,
               resi_NOHW <- FCnoHW-test,
               resi_ar1 <- FCar1 -test,
               resi_NONA <- FCnona-test)

plot(resi_NONA)
lines(test, col='red')
#fonction de calcul ME, MSE 
mse <- function(error)
{
  mean(error^2)
}
mae <- function(error)
{
  mean(abs(error))
}
mdse <- function(error)
{
  median(error^2)
}
mdae <- function(error)
{
  median(abs(error))
}

tabmse <- lapply(RESI_NO, mse)
tabmse <- lapply(tabmse, round,2)

tabmae<- lapply(RESI_NO, mae)
tabmae <- lapply(tabmae, round,2)

tabmdse<- lapply(RESI_NO, mdse)
tabmdse <- lapply(tabmdse, round,2)

tabmdae<- lapply(RESI_NO, mdae)
tabmdae <- lapply(tabmdae, round,2)

erroGG<- lapply(RESI_NO, ERROR)
erroGG <- lapply(tabmdae, round,2)
write.xlsx(x = erroGG, file = "erroGG.xlsx")

library(Metrics)
tab_err <- lapply(RESI_NO, ERROR)
tab_err <- lapply(tab_err, round,2)
library(xlsx)
write.xlsx(x = tab_err, file = "noer_pas.xlsx")
write.xlsx(x = tabRESI_NO, file = "tabnoer_pas.xlsx")

#----


#Choix des variables pour les prevision 
#----
#avec des variables explicatives : ACP et un stepwise 
library(MASS)
modselect_b=stepAIC(modlin,~.,trace=TRUE,
                    direction=c("backward"))
summary(modselect_b)

modselect_b=stepAIC(modlin,~.,trace=TRUE,
                    direction=c("forward"))
summary(modselect_b)

modselect_b=stepAIC(modlin,~.,trace=TRUE,
                    direction=c("both"))
summary(modselect_b)

#ACP 
library(factoextra)
library(FactoMineR)
res <- PCA(df, quanti.sup = 1)
res <- PCA(df_diff, quanti.sup = 1)
m1<-cor(df_diff)
library(ggcorrplot)
ggcorrplot(m1, hc.order = TRUE, type = "lower",
           outline.col = "white",
)

#----

plot(df_diff$Txch,col='orange', main ="Série brute avec différenciation de degrès 1")
plot(df_diff$CVT,col='grey')
plot(df_diff$GOOGLEUSA,col='grey')

#Prevision des trois modele 
##-----
#modele1
testingbase <- test
par(mfrow = c(2,3))
plot(df$touristeUSA)
plot(df$GOOGLEUSA)
plot(df$CVT)
plot(df$CVL)
plot(df$RT)
plot(df$CMRL)
df_diff
#modele 1 avec CVT et GOOGLEUSA
# Initialization
forecast <- NULL
dependvar <- cbind(df_diff$CVT,df_diff$GOOGLEUSA)
indepvar <- df_diff$Txch
dependvar2 <- data.frame(dependvar)

# One-step ahead forecasts for h=12
# forecast horizon h=12
y<- indepvar[1:129]
mX <- data.matrix(dependvar2[1:129,])
model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary", normality.JarqueB=T)
t <- residuals(model)
t<- ts(data = t , start=c(2007,01),frequency=12)
plot(myseries, col ='orange', main= 'Série I(1) avec résidu mod1')
lines(t, col='red')
legend("bottomright", legend=c("Série I(1)","résidu modele 1"),
       col=c('orange',2:5), lty=1:1, cex=0.8)


for (i in 1:12) {
  mX <- data.matrix(dependvar2[1:(128+i),])
  y <- indepvar[1:(128+i)]
  model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary")
  forecast <- c(forecast, predict(model, n.ahead = 1, 
                                  newmxreg = data.matrix(dependvar2[(128+i),])))	# one-step ahead forecast
}
forecast
forecast1<- ts(data = forecast , start=c(2017,10),frequency=12)
indepvar<- ts(data = indepvar , start=c(2007,02),frequency=12)
plot(indepvar, col ='orange', xlim=c(2017,2019))
lines(forecast1, col='red')

error1 <-  forecast1 -test
error1 <- ERROR(error1)




testingbase <- test
#MODELE 2 AVEC CVT 
# Initialization
forecast <- NULL
dependvar <- c(df_diff$CVT)
indepvar <- df_diff$Txch
dependvar2 <- data.frame(dependvar)

#modele 2 residu
y<- indepvar[1:129]
mX <- data.matrix(dependvar2[1:129,])
model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary",normality.JarqueB=T)
t <- residuals(model)
t<- ts(data = t , start=c(2007,01),frequency=12)
plot(myseries, col ='orange', main= 'Série I(1) avec résidu mod2')
lines(t, col='green')
legend("bottomright", legend=c("Série I(1)","résidu modele 2"),
       col=c('orange','green'), lty=1:1, cex=0.8)

# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:12) {
  mX <- data.matrix(dependvar2[1:(128+i),])
  y <- indepvar[1:(128+i)]
  model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary")
  forecast <- c(forecast, predict(model, n.ahead = 1, 
                                  newmxreg = data.matrix(dependvar2[(128+i),])))	# one-step ahead forecast
}
forecast
forecast2<- ts(data = forecast , start=c(2017,10),frequency=12)
indepvar<- ts(data = indepvar , start=c(2007,02),frequency=12)
plot(indepvar, col ='orange', xlim=c(2017,2019))
lines(forecast2, col='red')

error2 <-  forecast2 - test
error2<-ERROR(error2)

testingbase <- test
#MODELE 3 AVEC TOURISTEUSA ET CVT 
# Initialization
forecast <- NULL
dependvar <- cbind(df_diff$touristeUSA,df_diff$CVT)
indepvar <- df_diff$Txch
dependvar2 <- data.frame(dependvar)

#modele 3 residu 
y<- indepvar[1:129]
mX <- data.matrix(dependvar2[1:129,])
model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary",normality.JarqueB=T)
t <- residuals(model)
t<- ts(data = t , start=c(2007,01),frequency=12)
plot(myseries, col ='orange', main= 'Série I(1) avec résidu mod3')
lines(t, col='blue')
legend("bottomright", legend=c("Série I(1)","résidu modele 3"),
       col=c('orange','blue'), lty=1:1, cex=0.8)

# One-step ahead forecasts for h=12
# forecast horizon h=12
for (i in 1:12) {
  mX <- data.matrix(dependvar2[1:(128+i),])
  y <- indepvar[1:(128+i)]
  model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary")
  forecast <- c(forecast, predict(model, n.ahead = 1, 
                                  newmxreg = data.matrix(dependvar2[(128+i),])))	# one-step ahead forecast
}
forecast
forecast3<- ts(data = forecast , start=c(2017,10),frequency=12)
indepvar<- ts(data = indepvar , start=c(2007,02),frequency=12)
plot(indepvar, col ='orange', xlim=c(2017,2019))
lines(forecast3, col='red')

error3 <- forecast3 - test
error3 <- na.omit(error)

error3<- ERROR(error3)


plot(test,col= 'orange', main = 'Prévision avec variables explicative')
lines(forecast1,col = 6)
lines(forecast2,col = 7)
lines(forecast3,col=8)
legend("bottomleft", legend=c('Série brute',"Modèle 1","Modèle 2","Modèle 3"),
       col=c('orange',6:8), lty=1:1, cex=0.8)


df_ERROR <- data.frame(cbind(error1,error2,error3))
df_err1<-data.frame(c(testingbase - forecast1,testingbase - forecast2,testingbase - forecast3))

mean(resi_nots)
tabRESI_NO<- list(resi_nots <- FCnots-test,
               resi_noar <- FCnoar-test,
               resi_noarima <- FCnoarima-test,
               resi_noauto <- FCnoauto -test,
               resi_NOHW <- FCnoHW-test,
               resi_ar1 <- FCar1-test,
               resi_NONA <- FCnona-test,
               resid_mod1 <-forecast1-test, 
               resid_mod2 <-forecast2-test, 
               resid_mod3 <-forecast3-test)

erroGG<- lapply(tabRESI_NO, ERROR)
erroGG <- lapply(erroGG, round,2)
write.xlsx(x = erroGG, file = "erroGG.xlsx")


library(xlsx)
write.xlsx(x = df_ERROR, file = "ERRORmod.xlsx")
write.xlsx(x = df_err1, file = "df_err1.xlsx")

plot(df_diff$touristeUSA, col ='grey')


#------


#test DM
#----
resi_nots <- FCnots-test
resi_noar <- FCnoar-test
resi_noarima <- FCnoarima-test
resi_noauto <- FCnoauto -test
resi_NOHW <- FCnoHW-test
resi1 <- forecast1 -test
resi2 <- forecast2-test
resi3 <- forecast3-test
resi_ar1
resi_NONA <- FCnona-test


dm.test(resi_nots,resi_NONA,h=1)
dm.test(resi_noar,resi_NONA,h=1)
dm.test(resi_noarima,resi_NONA,h=1)
dm.test(resi_noauto,resi_NONA,h=1)
dm.test(resi_NOHW,resi_NONA,h=1)
dm.test(resi1,resi_NONA,h=1)
dm.test(resi2,resi_NONA,h=1)
dm.test(resi3,resi_NONA,h=1)
dm.test(resi_ar1,resi_NONA,h=1)

dm.test(resi_nots,resi_ar1,h=1)
dm.test(resi_noar,resi_ar1,h=1)
dm.test(resi_noarima,resi_ar1,h=1)
dm.test(resi_noauto,resi_ar1,h=1)
dm.test(resi_NOHW,resi_ar1,h=1)
dm.test(resi1,resi_ar1,h=1)
dm.test(resi2,resi_ar1,h=1)
dm.test(resi3,resi_ar1,h=1)



dm.test(resi_nots,resi3,h=1)
dm.test(resi_noar,resi3,h=1)
dm.test(resi_noarima,resi3,h=1)
dm.test(resi_noauto,resi3,h=1)
dm.test(resi_NOHW,resi3,h=1)
dm.test(resi1,resi3,h=1)
dm.test(resi2,resi3,h=1)

dm.test(resi_nots,resi2,h=1)
dm.test(resi_noar,resi2,h=1)
dm.test(resi_noarima,resi2,h=1)
dm.test(resi_noauto,resi2,h=1)
dm.test(resi_NOHW,resi2,h=1)
dm.test(resi1,resi2,h=1)

dm.test(resi_nots,resi1,h=1)
dm.test(resi_noar,resi1,h=1)
dm.test(resi_noarima,resi1,h=1)
dm.test(resi_noauto,resi1,h=1)
dm.test(resi_NOHW,resi1,h=1)

dm.test(resi_nots,resi_NOHW,h=1)
dm.test(resi_noar,resi_NOHW,h=1)
dm.test(resi_noarima,resi_NOHW,h=1)
dm.test(resi_noauto,resi_NOHW,h=1)

dm.test(resi_nots,resi_noauto,h=1)
dm.test(resi_noar,resi_noauto,h=1)
dm.test(resi_noarima,resi_noauto,h=1)

dm.test(resi_nots,resi_noarima,h=1)
dm.test(resi_noar,resi_noarima,h=1)

dm.test(resi_nots,resi_noar,h=1)
#----

#choix des modeles 
#-----
#creation data.Frame avec tous les forecast 
tabRESI_NO<- c(resi_nots <- FCnots-test,
               resi_noar <- FCnoar-test,
               resi_noarima <- FCnoarima-test,
               resi_noauto <- FCnoauto -test,
               resi_NOHW <- FCnoHW-test,
               resi_NONA <- FCnona-test,
               resid_mod1 <-forecast1-test, 
               resid_mod2 <-forecast2-test, 
               resid_mod3 <-forecast3-test, )

fore<- data.frame(cbind(FCnots,FCnoar,FCnoarima,FCnoauto,FCnoHW,FCnona, FCar1
             ,forecast1,forecast2,forecast3))
fore <- t(fore)
fore1 <- as.matrix(fore, rownames.force = TRUE)

library(xlsx)
#faire une matrice avec toutes les previsions 
fore<- data.frame(cbind(FCnots,FCnoar,FCnoarima,FCnoauto,FCnoHW,FCnona,FCar1
                        ,forecast1,forecast2,forecast3))
fore <- na.omit(fore)
#write.xlsx(x = fore, file = "prev.xlsx")
fore <- t(fore) #transposer le dataframe 
fore1 <- as.matrix(fore, rownames.force = TRUE) #creation de la matrice 


library(multDM)

ts <- test[1:12]#mettre en vecteur 
forecasts <- fore1
l <- loss(realized=ts,evaluated=forecasts,loss.type="SE")
d <- d_t(l)
q <- TB_MA(d=d,q.max=4)
MDM.selection(realized=ts,evaluated=forecasts,q=q,alpha=0.1,statistic="Sc",loss.type="SE")

#-----
plot(training, col ='orange')
lines(FCnots, col='red')

#calcul des prevision pour la serie brute --> Y_T
myseries <- txchange$Txch
myseries <- myseries[1:141]
myseries<-ts(data = myseries, start=c(2007,01),frequency=12)
plot(myseries,col='orange',main='TC première différenciation')
training <- window(myseries, end = c(2017+(8/12), 1))
test <- window(myseries, start = c(2017+9/12, 1))
myseries[128]
#forecast1,forecast2,forecast3
Y_mod1 <- NULL
Y_mod1 <- myseries[129]
Y_mod1[1]
forecast1ok <- c(0,forecast1)
for (i in 1:12){
  Y_mod1[i+1] <- forecast1ok[i+1] + Y_mod1[i]
}
Y_mod1 <- Y_mod1[2:13]
Y_mod1<-ts(data = Y_mod1, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_mod1, col='green')


Y_mod2 <- NULL
Y_mod2 <- myseries[129]
Y_mod2[1]
forecast2ok <- c(0,forecast2)
for (i in 1:12){
  Y_mod2[i+1] <- forecast2ok[i+1] + Y_mod2[i]
}
Y_mod2 <- Y_mod2[2:13]
Y_mod2<-ts(data = Y_mod2, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_mod1, col='green')
lines(Y_mod2, col='blue')

Y_mod3 <- NULL
Y_mod3 <- myseries[129]
Y_mod3[1]
forecast3ok <- c(0,forecast3)
for (i in 1:12){
  Y_mod3[i+1] <- forecast3ok[i+1] + Y_mod3[i]
}
Y_mod3 <- Y_mod3[2:13]
Y_mod3<-ts(data = Y_mod3, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_mod1, col='green')
lines(Y_mod2, col='blue')
lines(Y_mod3, col='red')


Y_modHW <- NULL
Y_modHW <- myseries[129]
Y_modHW[1]
FCnoHWok <- c(0,FCnoHW)
for (i in 1:12){
  Y_modHW[i+1] <- FCnoHWok[i+1] + Y_modHW[i]
}
Y_modHW <- Y_modHW[2:13]
Y_modHW<-ts(data = Y_modHW, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modHW, col='green')
lines(Y_mod2, col='blue')
lines(Y_mod3, col='red')


Y_modar1 <- NULL
Y_modar1 <- myseries[129]
Y_modar1[1]
FCar1ok <- c(0,FCar1)
for (i in 1:12){
  Y_modar1[i+1] <- FCar1ok[i+1] + Y_modar1[i]
}
Y_modar1 <- Y_modar1[2:13]
Y_modar1<-ts(data = Y_modar1, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modHW, col='green')
lines(Y_modar1, col='blue')
lines(Y_mod3, col='red')


Y_modnots <- NULL
Y_modnots <- myseries[129]
Y_modnots[1]
FCnotsok <- c(0,FCnots)
FCnotsok[1]
for (i in 1:12){
  Y_modnots[i+1] <- FCnotsok[i+1] + Y_modnots[i]
}
Y_modnots <- Y_modnots[2:13]
Y_modnots<-ts(data = Y_modnots, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modHW, col='green')
lines(Y_modnots, col='blue')
lines(Y_mod3, col='red')
test


Y_modnoar <- NULL
Y_modnoar <- myseries[129]
Y_modnoar[1]
FCnoarok <- c(0,FCnoar)
for (i in 1:12){
  Y_modnoar[i+1] <- FCnoarok[i+1] + Y_modnoar[i]
}
Y_modnoar <- Y_modnoar[2:13]
Y_modnoar<-ts(data = Y_modnoar, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modHW, col='green')
lines(Y_modnots, col='blue')
lines(Y_modnoar, col='red')

Y_modnnoarima <- NULL
Y_modnnoarima <- myseries[129]
Y_modnnoarima[1]
FCnoarimaok <- c(0,FCnoarima)
for (i in 1:12){
  Y_modnnoarima[i+1] <- FCnoarimaok[i+1] + Y_modnnoarima[i]
}
Y_modnnoarima <- Y_modnnoarima[2:13]
Y_modnnoarima<-ts(data = Y_modnnoarima, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modnnoarima, col='green')
lines(Y_modnots, col='blue')
lines(Y_modnoar, col='red')

Y_modnona<- NULL
Y_modnona <- myseries[129]
Y_modnona[1]
FCnoarimaok <- c(0,FCnona)
for (i in 1:12){
  Y_modnona[i+1] <- FCnoarimaok[i+1] + Y_modnona[i]
}
Y_modnona <- Y_modnona[2:13]
Y_modnona<-ts(data = Y_modnona, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modnnoauto, col='green')
lines(Y_modnnoarima, col='blue')
lines(Y_modnona, col='red')


Y_modnnoauto <- NULL
Y_modnnoauto <- myseries[129]
Y_modnnoauto[1]
FCnoautook <- c(0,FCnoauto)
for (i in 1:12){
  Y_modnnoauto[i+1] <- FCnoautook[i+1] + Y_modnnoauto[i]
}
Y_modnnoauto <- Y_modnnoauto[2:13]
Y_modnnoauto<-ts(data = Y_modnnoauto, start=c(2017,10),frequency=12)
plot(test, col='orange')
lines(Y_modnnoauto, col='green')
lines(Y_modnnoarima, col='blue')
lines(Y_modnoar, col='red')



#graphique 

plot(test,col='orange', main= 'Prévision non saisonnière')
lines(Y_modnots,col = 2)
lines(Y_modnoar,col = 3)
lines(Y_modnnoarima,col = 4)
legend("bottomleft", legend=c("Série brute", "X13",
                              "ARIMA(1,1,0)",
                              "ARIMA(1,1,1)"),
       col=c('orange', 2:4), lty=1:1, cex=0.8)



plot(test,col='orange')
lines(Y_modar1,col = 2)
lines(Y_modnnoauto,col = 3)
lines(Y_modHW,col = 4)
lines(Y_modnona,col=5)
legend("bottomleft", legend=c("Série I(1)","AR(1)","AUTO ARIMA","Holt-Winters","naive"),
       col=c('orange',2:5), lty=1:1, cex=0.8)


plot(test,col= 'orange', main = 'Prévision avec variables explicative')
lines(Y_mod1,col = 6)
lines(Y_mod2,col = 7)
lines(Y_mod3,col=8)
legend("bottomleft", legend=c('Série brute',"Modèle 1","Modèle 2","Modèle 3"),
       col=c('orange',6:8), lty=1:1, cex=0.8)

mean(resi_nots1)
#mettre les residus dans une liste 
RESI_NO<- list(resi_nots1 <- Y_modnots-test,
               resi_noar1 <- Y_modnoar-test,
               resi_noarima1 <- Y_modnnoarima-test,
               resi_noauto1 <- Y_modnnoauto -test,
               resi_NOHW1 <- Y_modHW-test,
               resi_ar11 <- Y_modar1-test,
               resi_nona1<- Y_modnona-test,
               resi_mod1 <- Y_mod1-test,
               resi_mod2 <- Y_mod2-test,
               resi_mod3 <- Y_mod3-test)
RESI_NO1 <- data.frame(RESI_NO)

calMSE <- lapply(RESI_NO, mse)
calMAE <- lapply(RESI_NO, mae)
calMdSE <- lapply(RESI_NO, mdse)
calMDAE <- lapply(RESI_NO, mdae)

calERROR <- lapply(RESI_NO, ERROR)
erreurY<- data.frame(cbind(calMSE,calMAE,calMSE,calMDAE))

#erreur final 
library(xlsx)
write.xlsx(x = erreurY, file = "erreurY.xlsx")
write.xlsx(x = RESI_NO1, file = "epp_erre.xlsx")
write.xlsx(x = calERROR, file = "calERROR.xlsx")


test
mean(test)
(mean(Y_mod1)-mean(test))/mean(test)*100
#test MP
#faire une matrice avec toutes les previsions 
fore<- data.frame(cbind(Y_modnots,Y_modnoar,Y_modnnoarima,Y_modnnoauto,Y_modHW,Y_modnona,Y_modar1
                        ,Y_mod1,Y_mod2,Y_mod3))
fore <- na.omit(fore)
#write.xlsx(x = fore, file = "prev.xlsx")
fore <- t(fore) #transposer le dataframe 
fore1 <- as.matrix(fore, rownames.force = TRUE) #creation de la matrice 


library(multDM)

ts <- test[1:12]#mettre en vecteur 
forecasts <- fore1
l <- loss(realized=ts,evaluated=forecasts,loss.type="SE")
d <- d_t(l)
q <- TB_MA(d=d,q.max=4)
MDM.selection(realized=ts,evaluated=forecasts,q=q,alpha=0.1,statistic="Sc",loss.type="SE")






#prévision avec la série brute en ajoutant la diff directement 
#dans le modele 
#library de la prevision 
#----
library(FitARMA)
library(lgarch) 
library(gets)
library(forecast)

#training entre oct 2017 et Sep 2018
myseries <- txchange$Txch
myseries <- myseries[1:141]
myseries<-ts(data = myseries, start=c(2007,01),frequency=12)
plot(myseries,col='orange',main='TC première différenciation')
training <- window(myseries, end = c(2017+(8/12), 1))
test <- window(myseries, start = c(2017+9/12, 1))














#prevision pas a pas avec la serie non saisonniere 
#PREVISION ARIMA(1,1,0)
y_noar<-training
y_upnoar<- NA
y_lownoar<- NA
for (i in 1:12) {
  y_noar <- y_noar[1:(127+i)]
  y_noar<-ts(data = y_noar, start=c(2007,01),frequency=12)
  model <-  arima(y_noar,order=c(1,1,0))  	# one-step ahead forecast
  w <-  forecast(model, h=1)
  y_noar<-y_noar[1:(128+i)]
  y_noar[(128+i)] <- w$mean
  y_upnoar[(128+i)] <- w$upper[2]
  y_lownoar[(128+i)] <- w$lower[2]
}
FCnoar <- y_noar[129:140]
FCnoar<-ts(data = FCnoar, start=c(2017,10),frequency=12)
y_upnoar<- ts(data = y_upnoar[129:140], start=c(2017,10),frequency=12)
y_lownoar<- ts(data = y_lownoar[129:140], start=c(2017,10),frequency=12)
plot(myseries,col=1, main= 'Prévision non saisonnière',xlim=c(2015,2019))
lines(FCnoar,col = 'blue')



y_noarcima<-training
for (i in 1:12) {
  y_noarcima <- y_noarcima[1:(128+i)]
  y_noarcima<-ts(data = y_noarcima, start=c(2007,01),frequency=12)
  model <-  arima(y_noarcima,order=c(1,1,1))  	# one-step ahead forecast
  w <-  predict(model, h=1)
  y_noarcima<-y_noarcima[1:(129+i)]
  y_noarcima[(129+i)] <- w$pred
}
FCnoarima <- y_noarcima[129:141]
FCnoarima<-ts(data = FCnoarima, start=c(2017,10),frequency=12)
lines(FCnoarima, col = 'grey')



y_noauto<-training
for (i in 1:12) {
  y_noauto <- y_noauto[1:(128+i)]
  y_noauto<-ts(data = y_noauto, start=c(2007,01),frequency=12)
  model <-  auto.arima(training)  	# one-step ahead forecast
  w <-  forecast(model, h=1)
  y_noauto<-y_noauto[1:(129+i)]
  y_noauto[(129+i)] <- w$mean
}
FCnoauto <- y_noauto[129:141]
FCnoauto<-ts(data = FCnoauto, start=c(2017,10),frequency=12)
lines(FCnoauto, col = 'green')


y_nona<-training
for (i in 1:12) {
  y_nona <- y_nona[1:(128+i)]
  y_nona<-ts(data = y_nona, start=c(2007,01),frequency=12)
  model <-  snaive(y_nona)	# one-step ahead forecast
  w <-  forecast(model, h = 1)
  y_nona<-y_nona[1:(129+i)]
  y_nona[(129+i)] <- w$mean[1]
}
FCnona <-  y_nona[129:141]
FCnona<-ts(data = FCnona, start=c(2017,10),frequency=12)
lines(FCnona, col = 'red')

legend("bottomright", legend=c("Série brute", "ARIMA(1,1,0)","ARIMA(1,1,1)",
                              "AUTO ARIMA",
                              "Naive"),
       col=c('black', 'blue','grey','green','red'), lty=1:1, cex=0.8)



r1 <- FCnoar - test
r2 <- FCnoarima - test
r3 <- FCnoauto - test
r4 <- FCnona- test

mse(r1)
mse(r2)
mse(r3)
mse(r4)

mae(r1)
mae(r2)
mae(r3)
mae(r4)

mdse(r1)
mdse(r2)
mdse(r3)
mdse(r4)

mdae(r1)
mdae(r2)
mdae(r3)
mdae(r4)

#Dossier evaluation des actifs financiers 
library(PerformanceAnalytics)
library(quantmod)
library(dygraphs)
library(tseries)
library(ggplot2)
library(gvlma)
library(moments)

#Exportation de la base 
#----
vect_name <- c("VBISX","VBLTX","VEIEX","VEURX","VFINX","VPACX")
VBISX1 <- merge(VBISX, VBLTX, by= "Date")
VEIEX1 <- merge(VEIEX, VBLTX, by= "Date")
VEURX1 <- merge(VEURX, VBLTX, by= "Date")
VFINX1 <- merge(VFINX, VBLTX, by= "Date")
VPACX1 <- merge(VPACX, VBLTX, by= "Date")
dim(VFINX1)

BD <- data.frame(VBISX1$Date,VBISX1$Adj.Close.x,VEIEX1$Adj.Close.x,VEURX1$Adj.Close.x
                 ,VFINX1$Adj.Close.x,VPACX1$Adj.Close.x, VBLTX$Adj.Close)


serie <- function(X,Y,name){
  serie1 <- data.frame(X,Y)
  serie1$ticker <- name
  names(serie1)<- c("date","price")
  return(serie1)
}
VBISX2 <- serie(VBISX1$Date,VBISX1$Close.x,"VBISX")
VEIEX2 <- serie(VEIEX1$Date,VEIEX1$Close.x,"VEIEX")
VEURX2 <- serie(VEURX1$Date,VEURX1$Close.x,"VEURX")
VFINX2 <- serie(VFINX1$Date,VFINX1$Close.x,"VFINX")
VPACX2 <- serie(VPACX1$Date,VPACX1$Close.x,"VPACX")
VBLTX2 <- serie(VBLTX$Date,VBLTX$Close,"VBLTX")



BD <- rbind(VBISX2,VEIEX2,VEURX2,VFINX2,VPACX2,VBLTX2)
names(BD)<- c("date","price","ticker")
table(BD$ticker)

library(data.table)
library(scales)
library(ggplot2)



dt <- data.table(BD)
dt$date<- as.Date(dt$date)
str(dt)
dt[, date := as.Date(date)]

# create indexed values
dt[, idx_price := price/price[1], by = ticker]
# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

#separer les indices dans 6 bases 
df <- BD

rendement_indice <- function(x,z){
  i <- 0
  i <- grep(x,dt$ticker)
  z<- dt[i,]
  return(z)
}
r_VBISX<-rendement_indice("VBISX",rendement_VBISX)
r_VBLTX<-rendement_indice("VBLTX",rendement_VBISX)
r_VEIEX<-rendement_indice("VEIEX",rendement_VBISX)
r_VEURX<-rendement_indice("VEURX",rendement_VBISX)
r_VFINX<-rendement_indice("VFINX",rendement_VBISX)
r_VPACX<-rendement_indice("VPACX",rendement_VBISX)


test <- dt[grep("VBISX",dt$ticker)]




#Calcul des rendements et graphique
#----

# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix (Indexed 2000 = 1)") +
  scale_color_discrete(name = "Indice")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# plot the return
ggplot(dt, aes(x = date, y = ret, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("rendement des indices") +
  xlab("Date") + ylab("Rendement") +
  scale_color_discrete(name = "Indice")


#-----

tab$sd^2
#Kurtosis et asymetrie 

base_combi <- cbind(r_VBISX$price,r_VBLTX$price,r_VEIEX$price
                    , r_VEURX$price, r_VFINX$price, r_VPACX$price)
base_combi <- data.frame(base_combi)

mean(base_combi$X1)

kurtosis(base_combi)
skewness(base_combi)

kurtosis(fusion_bp1)
skewness(fusion_bp1)

hist(fusion_bp1$VBISX)
densite <- density(fusion_bp1$VBISX)
lines(densite, col = "red",lwd=3)

hist(fusion_bp1$VBLTX)
densite <- density(fusion_bp1$VBISX)
lines(densite, col = "red",lwd=3)

hist(fusion_bp1$VEIEX)
densite <- density(fusion_bp1$VBISX)
lines(densite, col = "red",lwd=3)

hist(fusion_bp1$VEURX)
densite <- density(fusion_bp1$VBISX)
lines(densite, col = "red",lwd=3)

hist(fusion_bp1$VFINX)
densite <- density(fusion_bp1$VBISX)
lines(densite, col = "red",lwd=3)

hist(fusion_bp1$VPACX)
densite <- density(fusion_bp1$VBISX)
lines(densite, col = "red",lwd=3)


#Graphique Prix 
#----------
#graphique prix 
dt_without_VFINX <- dt[-c(grep("VFINX",dt$ticker)),]
only_VFINX <- dt[c(grep("VFINX",dt$ticker)),] 
VEIEX_VEURX <- dt[-c(grep("VFINX",dt$ticker),grep("VBISX",dt$ticker),
                     grep("VPACX",dt$ticker),grep("VBLTX",dt$ticker)),]

VBISX_BLTX_VPACX<- dt[-c(grep("VFINX",dt$ticker),grep("VEIEX",dt$ticker),
                         grep("VEURX",dt$ticker)),]

# plot prices by ticker 
ggplot(VBISX_BLTX_VPACX, aes(x = date, y = price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Prix : VBISX, VBLTX et VPACX") +
  xlab("Date") + ylab("Prix") +
  scale_color_discrete(name = "Indice")

ggplot(only_VFINX, aes(x = date, y = price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Prix : VFINX") +
  xlab("Date") + ylab("Prix") +
  scale_color_discrete(name = "Indice")

ggplot(VEIEX_VEURX, aes(x = date, y = price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Prix : VEIEX et VEURX") +
  xlab("Date") + ylab("Prix") +
  scale_color_discrete(name = "Indice")

ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix") +
  scale_color_discrete(name = "Indice")
#----

#Boite a moustache rendement 
#-----
r_VBISX<-rendement_indice("VBISX",rendement_VBISX)
r_VBLTX<-rendement_indice("VBLTX",rendement_VBISX)
r_VEIEX<-rendement_indice("VEIEX",rendement_VBISX)
r_VEURX<-rendement_indice("VEURX",rendement_VBISX)
r_VFINX<-rendement_indice("VFINX",rendement_VBISX)
r_VPACX<-rendement_indice("VPACX",rendement_VBISX)

fusion_bp <- cbind(r_VBISX$ret,r_VBLTX$ret,r_VEIEX$ret,
                   r_VEURX$ret,r_VFINX$ret, r_VPACX$ret)
fusion_bp <- data.frame(fusion_bp)
names(fusion_bp)<- vect_name

boxplot(fusion_bp, main ="Boite à moustache des rendement indiciels", col = 1:6)
#-----

#etude statistique univarié 
#----
summary(fusion_bp)


#----

#Calcul de ratio de sharpe 
#----
rs_VBISX <- r_VBISX[,-c(2:4)]
rs_VBLTX <- r_VBLTX[,-c(2:4)]
rs_VEIEX <- r_VEIEX[,-c(2:4)]
rs_VEURX <- r_VEURX[,-c(2:4)]
rs_VFINX <- r_VFINX[,-c(2:4)]
rs_VPACX <- r_VPACX[,-c(2:4)]

rs_VBISX[, date := as.Date(date)]
rs_VBLTX[, date := as.Date(date)]
rs_VEIEX[, date := as.Date(date)]
rs_VEURX[, date := as.Date(date)]
rs_VFINX[, date := as.Date(date)]
rs_VPACX[, date := as.Date(date)]



#----

#Matrice de variance-covariance et correlation
#----- 
fusion_bp1 <- fusion_bp[-1,]
mat_varcov <- round(cov(fusion_bp1),6)

library(corrplot)
mat_cor <- round(cor(fusion_bp1),4)
corrplot(mat_cor, method= "number")

summary(fusion_bp)



#----


#6 mesure de performance
library(timeDate)
library(timeSeries)
library(tseries)
library(FinCal)
str(fusion_bp2)

fusion_bp2 <- data.table(fusion_bp2)
fusion_bp2[, date := as.Date(date)]


TreynorRatio(fusion_bp2[,c(1:7)],fusion_bp2[,c(1:7)], Rf = 0.0004167/12) # oui 

SharpeRatio(fusion_bp2[,c(1:7)], Rf = 0.0004167/12) #oui 

CAPM.jensenAlpha(fusion_bp2[,c(1:7),drop = FALSE],fusion_bp2[,c(1:7), drop = FALSE] )#oui 

SortinoRatio(fusion_bp2[,c(1:7)], Rf = 0.0004167/12) #oui

InformationRatio(fusion_bp2[,c(1:7)],fusion_bp2[,c(1:7)], scale = NA) #oui



test <- fusion_bp2[,c(1,2)]
test <- data.table(test)
test[, date := as.Date(date)]


#Plan moyenne-variance ,calcul de rendement pf 6 fonds
#----
# summary table
# take only non-na values`
dt
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 6),
               sd = round(sd(ret), 6)),
           by = "ticker"]
mean(r_VBISX[-1,]$ret)


base_combi <- data.table(base_combi)

testtab <- dt[!is.na(ret), .(ticker, price)]
testtab <- testtab[, .(er = round(mean(price), 6),
               sd = round(sd(price), 6)),
           by = "ticker"]
mean(r_VBISX[-1,]$ret)


ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.0008)) +
  scale_x_continuous(label = percent, limits = c(0, 0.0125))


#Calculating portfolio returns
w <- c(1/6, 1/6, 1/6,1/6, 1/6, 1/6)

# Now use the built in PerformanceAnalytics function Return.portfolio
# to calculate the monthly returns on the portfolio,





fusion_bp2 <- cbind(r_VBISX,r_VBISX$ret,r_VBLTX$ret,r_VEIEX$ret,
                   r_VEURX$ret,r_VFINX$ret, r_VPACX$ret)
row.names(fusion_bp2) <- fusion_bp2$date
fusion_bp2 <- data.frame(fusion_bp2)
fusion_bp2 <- fusion_bp2[,-c(2:5)]
names(fusion_bp2)<- c("date",vect_name)

fusion_bp2 <- data.table(fusion_bp2)
fusion_bp2[, date := as.Date(date)]

pf_rendement <- Return.portfolio(fusion_bp2, weights = w)

er_pf <- round(mean(pf_rendement),6)
sd_pf <- round(sd(pf_rendement),4)
var(pf_rendement)

er_pf*100

tab1 <- tab
vect_add <- c("PF",er_pf, sd_pf)

tab1 <- data.frame(tab1)
tab1 <- rbind(tab1,vect_add)
tab1 <- data.table(tab1)
tab
summary(tab1)
tab1$er<- as.numeric(tab1$er)
tab1$sd<- as.numeric(tab1$sd)

ggplot(tab1, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.00075)) +
  scale_x_continuous(label = percent, limits = c(0, 0.012))



#----
#frontiere efficient 


# load the data

df <- cbind(r_VEIEX$ret,r_VEURX$ret
            ,r_VFINX$ret)
df <- data.frame(df)
df<- df[-1,]
vectxyz <- c("x","y","z")
names(df) <- vectxyz
df <- data.table(df)
cov(df)

# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)

# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]

# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0.00030, 0.00055)) +
  scale_x_continuous(label = percent, limits = c(0.00750, 0.01)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)



#Calculating the Efficient Frontier
#with Short selling
calcEFParams <- function(rets) {
  
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  
  return(retlist)
}



abcds <- calcEFParams(df)
abcds

## $alpha
## [1] 4037.551
##
## $beta
## [1] 147.8334
##
## $gamma
## [1] 5.992395
##
## $delta
## [1] 2339.881

calcEFValues <- function(x, abcd, upper = T) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  
  return(retval)
}

# calculate the risk-return tradeoff the two assets (for plotting the points)
df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]

# plot the values
ggplot(df_table, aes(x = sd, y = er)) +
  # add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  # add the upper efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = T), n = 10000,
                color = "red", size = 1) +
  # add the lower "efficient" frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = F), n = 10000,
                color = "blue", size = 1) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))


#Without Short-Selling

library(tseries)

##
##     'tseries' version: 0.10-34
##
##     'tseries' is a package for time series analysis and
##     computational finance.
##
##     See 'library(help="tseries")' for details.

df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]

er_vals <- seq(from = min(df_table$er), to = max(df_table$er), length.out = 1000)

# find an optimal portfolio for each possible possible expected return
# (note that the values are explicitly set between the minimum and maximum of the expected returns per asset)
sd_vals <- sapply(er_vals, function(er) {
  op <- portfolio.optim(as.matrix(df), er)
  return(op$ps)
})

plot_dt <- data.table(sd = sd_vals, er = er_vals)

# find the lower and the upper frontier
minsd <- min(plot_dt$sd)
minsd_er <- plot_dt[sd == minsd, er]
plot_dt[, efficient := er >= minsd_er]
plot_dt


ggplot() +
  geom_point(data = plot_dt[efficient == F], aes(x = sd, y = er), size = 0.5, color = "blue") +
  geom_point(data = plot_dt[efficient == T], aes(x = sd, y = er), size = 0.5, color = "red") +
  geom_point(data = df_table, aes(x = sd, y = er), size = 4, color = "red", shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier without Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))


#To directly compare the two options we can use the following code.

# combine the data into one plotting data.table called "pdat"
# use plot_dt with constraints
pdat1 <- plot_dt[, .(sd, er, type = "wo_short", efficient)]

# calculate the values without constraints
pdat2lower <- data.table(sd = seq(from = 0, to = max(pdat1$sd) * 1.2, length.out = 1000))
pdat2lower[, ':=' (er = calcEFValues(sd, abcds, F),
                   type = "short",
                   efficient = F)]

pdat2upper <- data.table(sd = seq(from = 0, to = max(pdat1$sd) * 1.2, length.out = 1000))
pdat2upper[, ':=' (er = calcEFValues(sd, abcds, T),
                   type = "short",
                   efficient = T)]

pdat <- rbindlist(list(pdat1, pdat2upper, pdat2lower))

# plot the values
ggplot() +
  geom_line(data = pdat, aes(x = sd, y = er, color = type, linetype = efficient), size = 1) +
  geom_point(data = df_table, aes(x = sd, y = er), size = 4, color = "red", shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontiers") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2)) +
  scale_color_manual(name = "Short-Sells", values = c("red", "blue"), labels = c("Allowed", "Prohibited")) +
  scale_linetype_manual(name = "Efficient", values = c(2, 1))

tab1
# volatility   Expected returns
#0.01711727	0.001764382	TRUE


#portefeuille tangeant

# construct the data
library(fPortfolio)
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)
asset.names = c("VEIEX", "VEURX", "VFINX")
er = c(er_x, er_y, er_z)
names(er) = asset.names
covmat = cov(df)
r.free = 2/(100*365)
dimnames(covmat) = list(asset.names, asset.names)

# tangency portfolio
tan.port <- tangency.portfolio(er, covmat, r.free)
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)



# compute portfolio frontier
ef <- efficient.frontier(er, covmat, alpha.min=-2,
                         alpha.max=1.5, nport=30)
attributes(ef)

plot(ef)
plot(ef, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)
sr.tan = (tan.port$er - r.free)/tan.port$sd
abline(a=r.free, b=sr.tan, col="green", lwd=2)




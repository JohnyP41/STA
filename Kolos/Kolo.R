#Zadanie 1


#Zadanie 2
library(UsingR)
attach(cfb)
dane<- cfb[cfb$INCOME>0 & cfb$NETWORTH <0,]
mean(dane$AGE)
var(dane$AGE)
sd(dane$AGE / mean(dane$AGE)) * 100
library(e1071)
skewness(dane$AGE)
kurtosis(dane$AGE)
#Zadanie 3


#Zadanie 4
dane <- read.table('Z4.txt', header = T, dec=".")
attach(dane)
head(dane)
model<-lm(y~x1+x2+x3,dane)
summary(model)
nowy <- data.frame(x1=30,x2=37,x3=6)
predict(model, nowy)
#Zadanie 5
library(EnvStats)
x<-c(43,53,39,45,51,48,32,41,34)
enorm(x, ci = TRUE, ci.type = "two-sided", conf.level = 0.90, ci.param = "mean")
#Zadanie 6
dane <- read.table('Z5.txt', header = T, dec=",",sep=":")
attach(dane)
matplot(dane,type = "b",lty=1:5, pch=1:5, col=1:5,xlab='Miesiac',ylab = 'CO2(ppm)',main="lol")
legend(10, 35, legend=c('60', '70','80','90'),col=1:5, lty=1:5,pch=1:5)

#Zadanie 7
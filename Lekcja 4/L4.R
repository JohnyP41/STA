#Zadanie 1
#b
awarie <- read.table('Awarie.txt')
library(EnvStats)
eexp(awarie$V1, ci = TRUE, ci.type = "two-sided", conf.level = 0.95)
#Zadanie 2
#a
load("Pomiary.RData")
head(Pomiary)
qqnorm(Pomiary$V1)
qqline(Pomiary$V1,col='red')
#b
library(EnvStats)
enorm(Pomiary$V1, ci = TRUE, ci.type = "two-sided", conf.level = 0.95, ci.param = "mean")
enorm(Pomiary$V1, ci = TRUE, ci.type = "two-sided", conf.level = 0.95, ci.param = "variance")

#Zadanie 3
#a
lambda.cint <- function(x,conf.level) {
  title<-c("lambda")
  a <-1-conf.level
  est<-sum(x^2)/length(x)
  l<-(sum(x^2)/length(x))*(1-(qnorm(1-a/2)/sqrt(length(x))))
  r<-(sum(x^2)/length(x))*(1+(qnorm(1-a/2)/sqrt(length(x))))
  b<-list(title=title,est=est,l=l,r=r,conf.level=conf.level)
  class(b)<-"confint"
  return(b)
}
#b
print.confint <- function(x){
  cat(x$conf.level*100, "percent confidence interval:", "\n")
  cat(x$l, " ", x$r, "\n")
}
summary.confint <- function(x){
  cat("\n", "Confidence interval of", x$title, "\n", "\n")
  cat(x$conf.level*100, "percent confidence interval:", "\n")
  cat(x$l, " ", x$r, "\n")
  cat("sample estimate", "\n")
  cat(x$est, "\n")
}

#wywolania
x<-c(0.9,6.2, 2.1 ,4.1, 7.3,1.0, 4.6 ,6.4 ,3.8 ,5.0,2.7, 9.2, 5.9, 7.4 ,3.0, 4.9, 8.2 ,5.0, 1.2 ,10.1,12.2, 2.8, 5.9 ,8.2, 0.5)
l<-lambda.cint(x,0.95)
print.confint(l)
summary.confint(l)


#Zadanie 4
Energia <- read.table("Energia.txt", dec = ".",header=TRUE)
head(Energia)
attach(Energia)

model <- lm(energia~produkcja, data = Energia)
nowy <- data.frame(produkcja=8)
predict(model, nowy, interval = 'prediction')


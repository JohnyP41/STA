#Zadanie 1
library(UsingR)
mean(rat)
t.test(rat, mu = 120, alternative = "less")
#0.2116>0.05 nie ma podstaw do odrzucenia H0

#Zadanie 2
#a
boxplot(homework)
#b
var.test(homework$Private, homework$Public)
#c
t.test(homework$Private, homework$Public, alternative = "greater", var.equal = TRUE)

#Zadanie 3
#a
stopien<-c(78.2, 78.5, 75.6, 78.5, 78.5, 77.4, 76.6,76.1, 75.2, 75.8, 77.3, 77.3,77.0, 74.4, 76.2, 73.5, 77.4)
proszek<-rep(c('A','B'),c(7,10))
dane<-data.frame(proszek,stopien)
boxplot(dane[proszek == "A", ]$stopien, dane[proszek == "B", ]$stopien,xlab="Typ proszku",ylab="Stopien wyprania (w %)",main="Róznice miedzy proszkami",names=c('A','B'))

#b
var.test(dane[proszek == "A", ]$stopien, dane[proszek == "B", ]$stopien)
t.test(dane[proszek == "A", ]$stopien, dane[proszek == "B", ]$stopien, alternative = "greater", var.equal = TRUE)

#Zadanie 4
#a
corn
t.test(corn$New, corn$Standard,alternative = "two.sided",var.equal = TRUE)

#b
var.test(corn$New, corn$Standard)
#0.9652>0.05 wiec wariancje sa rowne.

#c
t.test(corn$New, corn$Standard,alternative = "two.sided", paired = TRUE)

#Zadanie 5
a<-c(6.6, 6.5, 9.0, 10.3, 11.3, 8.1, 6.3, 11.6)
b<-c(6.8, 2.5, 7.4, 8.5, 8.1, 6.1, 3.4, 2.0)
t.test(a, b,alternative = "greater", paired = TRUE)
#H0: mu0-mu1=0
#H1: mu0-mu1>0
#0.009577<0.05 H0 odrzucamy
#Przyjmujemy H1 ze hipnoza redukuje odczuwany ból.
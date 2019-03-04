#Zadanie 1 
#a
ankieta <- read.table('Ankieta.txt',header=TRUE)
attach(ankieta)
head(ankieta,5)
#b
Liczebnosc1 <- table(WYNIK)
Procent <- prop.table(Liczebnosc1)
Liczebnosc2 <- table(WYNIK[ankieta$SZKOLA=='p'])
Procenty <- round(prop.table(Liczebnosc2),2)
#c
pie(Liczebnosc2,main = 'Podstawowe')
Liczebnosc3 <- table(WYNIK[ankieta$SZKOLA=='w'])
pie(Liczebnosc3,main = 'Wyzsze')
Liczebnosc4 <- table(WYNIK[ankieta$SZKOLA=='s'])
pie(Liczebnosc4,main = 'Srednie')
#d
Kobiety<-subset(ankieta,ankieta$PLEC=='k' & (ankieta$SZKOLA=='s' | ankieta$SZKOLA=='w'),select=c(PLEC,SZKOLA,WYNIK))

#e
library(car)
recode(WYNIK,"c('a','b')='1';c('c','d')='2';c('e')='3'")
#f
Liczebnosc <- table(WYNIK, PLEC)
Procent <- prop.table(Liczebnosc, 2) * 100
barplot(Liczebnosc,
       beside = TRUE,
       col = 1:5,
       ylab = '',
       legend.text = c('a','b','c','d','e'))

#Zadanie 2
load("Centrala.RData")
attach(Centrala)
head(Centrala, 5)
#a
Liczebnosc <- table(Liczba)
Procent <- prop.table(Liczebnosc) *100
Ocena <- cbind(Liczebnosc, Procent)
o<-data.frame(Ocena)
#b
barplot(Liczebnosc,
        col = 8,
        main = 'Rozklad liczby zgloszen w centrali telefonicznej',
        ylab = 'Liczba obserwacji',
        xlab = 'Liczba zgloszen')
#c
b<-cbind(mean(Liczba),median(Liczba),sd(Liczba),sd(Liczba) / mean(Liczba))
colnames(b)<-c('Œrednia','Mediana','Odch.stand','Wsp.zm.')
mean(Liczba)
median(Liczba)
sd(Liczba)
sd(Liczba) / mean(Liczba)

#Zadanie 3
awarie <- read.table('Awarie.txt',header=TRUE)
attach(awarie)
head(awarie,5)
#a
Liczebnosc <- table(cut(awarie[, 1], c(0, 500, 1000, 1500,2000,2500,3000,3500)))
Procent<-round(prop.table(Liczebnosc),2)
xd<-cbind(Liczebnosc,Procent)
xd1<-data.frame(xd)
#b
boxplot(awarie[, 1],
        main='Wykres ramkowy czasu bezawaryjnej pracy',
        ylab='Czas bezawaryjnej pracy')
#c
a<-cbind(mean(awarie[, 1]),median(awarie[, 1]),sd(awarie[, 1]),sd(awarie[, 1]) / mean(awarie[, 1]),skewness(awarie[, 1]),kurtosis(awarie[, 1]))
colnames(a)<-c('Œrednia','Mediana','Odch.stand','Wsp.zm.','Wsp.as.','Wsp.konc.')

#Zadanie 4
#a
Odmiana<-rep(c('Aster','Drop','Frezja','Irys','Ruta'),c(6,5,6,5,6))
Plon<-c(168, 160, 169, 175, 159, 162,130, 136, 140, 137, 124,148, 149, 130, 139, 138, 140,126, 128, 131, 130, 127,145, 149, 148, 152, 150, 145)
dane<-data.frame(Plon,Odmiana)

#b
boxplot(Plon[dane$Odmiana=='Aster'],Plon[dane$Odmiana=='Drop'],Plon[dane$Odmiana=='Frezja'],Plon[dane$Odmiana=='Irys'],Plon[dane$Odmiana=='Ruta'],main="Porównanie plennosci odmian ziemniaka", 
        xlab="Odmiana",names=c('Aster','Drop','Frezja','Irys','Ruta'), ylab="Plon")

#Zadanie 5
wspolczynnik.zmiennosci <- function(x,na.rm=FALSE) {
  if(na.rm==TRUE & is.numeric(x)){
    p<-na.omit(x)
    l<-sd(p)/mean(p)*100
    return(l)
  }
  
  if(is.numeric(x)) {
    l<-sd(x)/mean(x)*100
    return(l)
    } 
    else {
      stop('argument nie jest wartoœci¹ liczbow¹')
    }
}


x <- c(1,NA,3)
wspolczynnik.zmiennosci(x)
## [1] NA
wspolczynnik.zmiennosci(x,na.rm = TRUE)
## [1] 70.71068
wspolczynnik.zmiennosci()
## Error in wspolczynnik.zmiennosci() :
## argument "x" is missing, with no default
wspolczynnik.zmiennosci(c("x", "y"))
## Error in wspolczynnik.zmiennosci(c("x", "y")) :
## argument nie jest wartoœci¹ liczbow¹

















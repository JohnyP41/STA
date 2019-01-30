#Zadanie 1
x<- rep(c(TRUE,FALSE,TRUE,FALSE), c(3,4,2,5))
#Zadanie 2
x <- (1:1000)
x[seq(2, 1000, by = 2)] <- 1/seq(2, 1000, by = 2)
#Zadanie 3
p<-c(c(1:20),rep(0,each=10),seq(2,40,by=2))
d<-c(seq(40,2,by=-2),rep(0,each=10),c(20:1))
k<-c(p,d)

#Zadanie 4
imie=c('Jan','Przybylski')
pi=3.14159265358979323846264338327902884
lel=c(seq(0.02,1,by=0.02))
MojaLista<-list(imie = imie, Pi=pi,Ciag=lel )

#Zadanie 5
miesiac=c(Styczen:Grudzien)
NY_F=c(32,33,41,52,62,72,77,75,68,58,47,35)
dane=data.frame(NY_F)
rownames(dane)<-c('Styczeñ','Luty','Marzec','Kwiecieñ','Maj','Czerwiec','Lipiec','Sierpieñ','Wrzesieñ',"PaŸdziernik","Listopad","Grudzieñ")
dane$NY_C<-round((NY_F-32)*(5/9),2)
save(dane, file = 'Miasta.RData')

#Zadanie 6
Cities <- read.csv2('Cities.csv', row.names = 1)
attach(Cities)
Cities$Atlanta_C<- round((ATLANTA-32)*(5/9),6)
Cities$Phoenix_C<- round((PHOENIX-32)*(5/9),6)
Cities$Sandiego_C<- round((SANDIEGO-32)*(5/9),6)


Cities$Atlanta_C<- round(Cities$Atlanta_C,2)
Cities$Phoenix_C<- round(Cities$Phoenix_C,2)
Cities$Sandiego_C<- round(Cities$Sandiego_C,2)
lol=cbind(dane,Cities$Atlanta_C,Cities$Phoenix_C,Cities$Sandiego_C)
colnames(lol)[3] <- 'Atlanta_C'
colnames(lol)[4] <- 'Phoenix_C'
colnames(lol)[5] <- 'SanDiego_C'
lol$NY_F=NULL

colnames(lol)[1] <- 'Nowy York'
colnames(lol)[2] <- 'Atlanta'
colnames(lol)[3] <- 'Phoenix'
colnames(lol)[4] <- 'San Diego'

save(lol,file='Miasta1.RData')

matplot(lol,type = "b",lty=1:5, pch=1:5, col=1:5,xlab='Miesiac',ylab = 'Temperatura w (stopniach C)')
legend(1, 35, legend=c('Nowy York', 'Atlanta','Phoenix','San Diego'),col=1:5, lty=1:5,pch=1:5)


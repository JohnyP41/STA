#Zadanie 1
szybkosc<-c(0.9,6.2, 2.1 ,4.1, 7.3,1.0, 4.6 ,6.4 ,3.8 ,5.0,2.7, 9.2, 5.9, 7.4 ,3.0, 4.9, 8.2 ,5.0, 1.2 ,10.1,12.2, 2.8, 5.9 ,8.2, 0.5)
#b
sum((szybkosc^2)/25)
#c
par.lambda <- function(x) {
  b<-sum((x^2)/length(x))
    return(b)
}

#wywolanie
x<-szybkosc
par.lambda(x)

#Zadanie 2
#b
par.b <- function(x,y) {
  b<-sum(y*x)/sum(x*x)
  if(length(y)!=length(x))
  stop("size of x must be equal to size of y")
  else
  return(b)
}

#wywolanie
x <- c(1, 2, 3)
y <- c(4, 5, 6)
par.b(x, y)

y<-c(4, 5, 6, 7)
par.b(x, y)
#Zadanie 3
plot(cars)
#a
model <- lm(dist ~ speed, data = cars)
abline(model, col = "black")
#b
summary(model)
#dist=3.9324*speed-17.5791
#c
summary(model)
#dopasowanie=0.6511
#d
nowy <- data.frame(speed = 30:50)
predict(model, nowy)

#Zadanie 4
#a
model <- lm(Employed~GNP.deflator+GNP+Unemployed+Armed.Forces+Population+Year, data = longley)
#b
summary(model)
#dopasowanie= 0.9925
#c
summary(model)
#stymulanty=GNP.deflator,Year
#destymulanty=GNP,Unemployed,Armed.Forces,Population
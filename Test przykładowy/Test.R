#Zadanie 1
dane <- read.table('Z1.txt', header = T)
attach(dane)
head(dane)
Liczebnosc <- table(Wynik[dane$Plec=="dz" & dane$Rok_ur=="1980"])
Procent <- round(prop.table(Liczebnosc) * 100,2)
Ocena <- cbind(Liczebnosc, Procent)
tekst <- c('bardzo dobra', 'przeciêtna', 'z³a', 'fatalna')
rownames(Ocena) <- tekst
#C)

#Zadanie 2
#B)

#Zadanie 3
load("Z3.RData")
head(Z3)
attach(Z3)
t.test(Czas~Typ,mu = 0, var.equal = TRUE)
#C)
#Zadanie 4
#B)

#Zadanie 5
load("Z5.RData")
head(Z5)
analizaskupien <- hclust(dist(Z5, method = 'euclidean'), method = 'average')
#b                         
plot(analizaskupien, hang = -1)
w <- rect.hclust(analizaskupien, k = 4)
#D)

#Zadanie 6
load("Z6.RData")
head(Z6)
attach(Z6)
model1 <- lm(Wydatki ~ Dochody, data = Z6)
new <- data.frame(Dochody=350)
predict(model1, new, interval = 'prediction')

#(211.5352;334.1937)

#Zadanie 7
n.test<- function(x,sigma_zero,alternative="two.sided"){
  match.arg(alternative, choices = c("two.sided","less","greater"))
  S<-sum((x-mean(x))^2)/length(x)
  statistic<-(length(x)*S)/(sigma_zero^2)
  names(statistic)<-"T"
  parameter=length(x)-1
  names(parameter)<-"num df"
  method<-c("Test chi-kwadrat w modelu normalnym")
  data.name<-deparse(substitute(x))
  
  if(alternative=="greater"){
    p.value=pchisq(statistic,length(x)-1)
  }
  if(alternative=="less"){
    p.value=1-pchisq(statistic,length(x)-1)
  }
  if(alternative=="two.sided"){
    p.value=2*min(pchisq(statistic,length(x)-1),1-pchisq(statistic,length(x)-1))
  }
  
  b<-list(statistic=statistic,parameter=parameter,data.name=data.name,alternative=alternative,method=method,p.value=p.value)
  class(b)<-"htest"
  return(b)
}
x<-c(2,3,45,66,7,7,324,234,5345,435,123)
n<-n.test(x,0.001,"less")
print(n)

#Zadanie 8
load("Z8.RData")
head(Z8)
pca <- princomp(Z8)
summary(pca)
plot(pca)
#0.7776984+0.1347836
#0.912482
#91,2% 
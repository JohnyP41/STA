#Zadanie 1 
#a
#b
w.test<- function(x,lambda.zero,alternative="two.sided") {
  match.arg(alternative, choices = c("two.sided","less","greater"))
  statistic<-2*length(x)*lambda.zero*mean(x)

  if(alternative=="two.sided"){
    p.value=2*min(pchisq(statistic,2*length(x)),1-pchisq(statistic,2*length(x)))
  } 
  
  if(alternative=="greater"){
    p.value=pchisq(statistic,2*length(x))
  }
  
  if(alternative=="less"){
    p.value=1-pchisq(statistic,2*length(x))
  }
  
  method<-c("Test chi-kwadrat w modelu wyk³adniczym")
  parameter<-2*length(x)
  names(parameter)<-"num df"
  names(statistic)<-"T"
  data.name<-deparse(substitute(x))
  b<-list(statistic=statistic,parameter=parameter,p.value=p.value,alternative=alternative,method=method,data.name=data.name)
  class(b)<-"htest"
  return(b)
}
#c
Time <- read.table('Awarie.txt')
# wartoœæ statystyki testowej
2*length(Time$V1)*0.001*mean(Time$V1)
# wartoœæ krytyczna
qchisq(1-0.05, 100)
#T(x)<k wiec nie ma podstaw do odrzucenia Hipotezy H0

w<-w.test(Time$V1,0.001,"less")
print(w)
#p-wartosc>a wiec nie ma podstaw do odrzucenia Hipotezy H0
#Zosta³a podjeta ta sama decyzja.



#Zadanie 1
#a
USArrests
pca <- princomp(USArrests)
#Mamy tu 4 sk³adowe g³ówne

#b
summary(pca)
#Te dwie sk³adowe wyjaœniaja 99% zmiennoœci

#c
plot(pca)
#Pierwsza sk³adowa wyjaœnia najbardziej zmiennoœæ.

#Zadanie 2
#a
analizaskupien <- hclust(dist(USArrests), method = 'complete')

#b                         
plot(analizaskupien, hang = -1)
w <- rect.hclust(analizaskupien, k = 3)
#Liczba skupien = 3

#c
analizaskupien <- hclust(dist(USArrests, method = 'manhattan'), method = 'average')
plot(analizaskupien, hang = -1)
w <- rect.hclust(analizaskupien, k = 3)
#Wyniki nie zmieni³y siê istotnie.

#Zadanie 3
#a
library(MASS)
attach(iris)
lda <- lda(Species ~ ., data = iris)

#b
mean(predict(lda)$class != iris$Species)

#c
predict(lda, data.frame(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.3, Petal.Width = 0.3))

#Do gantunku setosa z p=1


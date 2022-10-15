setwd("C://Users//gasto//Downloads//R MAIN 4 S8//projet R student")
data = read.csv2("student/student-mat.csv",sep=";",header=TRUE)
head(data)


# on enl?ve les notes du S1, S2 pour ne garder que la moyenne finale car sinon trop corr?ler
# entre elles. De plus, on a parfois des 0 comme moyennes qui peuvent poser prol?me
# on cr?e une colonne average qui sera la moyenne entre la note au S1 et au S2
for(i in 1:dim(data)[1])
{
  data$Average[i] = (data$G1[i] + data$G2[i])/2 
}

# on peut enlever les variables inutiles : guardian (tuteur) et G1, G2
data$guardian = NULL
data$G1 = NULL
data$G2 = NULL
data$G3 = NULL
# on change la colonne failure en binaire, 0 : n'a pas redoubl?, 1: ? redoubl? au moins 1 fois
size = dim(data)[1]
for(i in 1:size)
{
  if(data$failures[i] == 0)
  {
    data$failures[i] = 0
  }
  else
  {
    data$failures[i] = 1
  }
}
data$failures = as.integer(data$failures)


# on met en factor toute nos colonnes binaires (0/1)
size = dim(data)[2]
colName = names(data)
for(i in 1:size)
{
  col = data[i]
  
  if(dim(unique(col))[1] == 2)
  {
    data[i] = as.factor(unlist(data[i]))
  }
}

# ACP
dataPCA = data
dataPCA = data[,-c(1,2,3,4,6,9,10,11,12,15,16,17,18,19,20,21,22,23,24,28,29)]
str(dataPCA)
library(FactoMineR)
res = PCA(dataPCA,scale.unit = TRUE, graph = FALSE, quali.sup = 1) 
res$eig
barplot(res$eig[,2])
plot(res,choix = "var")
plot(res,choix = "ind", habillage = 7, cex = 0.7)
# AXE 1 : ? droite ?tudiants ne travaillant pas bcp vs ?tudiants s?rieux
# AXE 2 : en haut ?tudiants ayant des parents ?duqu? vs non ?duqu?s 

# graphe des individus : on voit clairement deux groupes : en bleu, les etudiants avec une bonne
# moyenne, s?rieux et en rouge les ?tudiants festifs et ?chouant 


# LDA 
dataLDA = data[,-c(1,2,3,4,5,6,9,10,11,12,15,16,17,18,19,20,21,22,23,24,28,29)]
dataLDA <- dataLDA[c("Medu", "Fedu", "studytime", "goout","Dalc","Walc","Average","failures")]

for(i in 1:dim(dataLDA)[1])
{
  if(dataLDA$failures[i] == 3)
  {
     dataLDA$failures[i] = 2
  }
}

dataLDA$failures = as.factor(dataLDA$failures)
str(dataLDA)
library(MASS)
res.afd.lda = lda(failures ~., data=dataLDA)
res.afd.lda
plot(res.afd.lda)

F12 = predict(res.afd.lda, prior=rep(1/3,3))$x
cercle_correlation=cor(dataLDA[,1:7],F12)
cercle_correlation

a=seq(0,2* pi,length=100)
plot(cos(a), sin(a), type='l',lty=3,xlab='Dim 1', ylab='Dim
2',main="Cercle des corr?lations" )
arrows(0,0,cercle_correlation[,1],cercle_correlation[,2],col=2)
text(cercle_correlation,labels=colnames(dataLDA[,1:7]))

# QDA 
dataQDA = dataLDA
res.qda = qda(failures ~., data=dataQDA)
res.qda
str(dataQDA)

# Pr?diction 
newdataGood= data.frame(Medu = 5, Fedu = 5, studytime = 4, goout = 1, Dalc = 0, Walc = 1,Average = 17.0)
newdataBad= data.frame(Medu = 1, Fedu = 1, studytime = 0, goout = 4, Dalc = 4, Walc = 4, Average = 6.5)
newdataGood

#LDA
pred.ldaGood = predict(res.afd.lda,newdataGood)
pred.ldaBad = predict(res.afd.lda,newdataBad)
pred.ldaGood$posterior
pred.ldaBad$posterior

#QDA
pred.qdaGood = predict(res.qda,newdataGood)
pred.qdaBad = predict(res.qda,newdataBad)
pred.qdaGood$posterior
pred.qdaBad$posterior




#______________Classification non supervis?e______________________
#k-means

#K-means en sp?cifiant le nombre de classes
data_Kmeans = dataPCA
summary(data_Kmeans)
str(data_Kmeans)
kmeans.result=kmeans(data_Kmeans[,2:9], centers = 3)
names(kmeans.result)
kmeans.result$size
kmeans.result$centers
kmeans.result$cluster
table(kmeans.result$cluster)

#les inerties intra et inter classes
kmeans.result$totss           # inertie totale 
kmeans.result$withinss
kmeans.result$tot.withinss    # inertie intra-cluster = sum(kmeans.result$withinss)
kmeans.result$betweenss       # inertie inter-cluster

#On peut comparer la classification obtenue avec la r?partition en trois esp?ces : 
table(data_Kmeans$failures, kmeans.result$cluster)

#Repr?sentation des classes obtenues sur les graphes des variables 2 ? 2 : 
pairs(data_Kmeans[,1:4], col=kmeans.result$cluster)


#CAH
dataCAH = dataPCA
str(dataCAH)
d.dataCAH = dist(dataCAH[,2:9])
hc <- hclust(d.dataCAH, method = "ward.D2")

#dendrogramme
plot(hc)
plot(hc,labels=dataCAH$Species)

#Graphe des hauteurs des branches
barplot(hc$height)

#K=3 car on a 3 classes
plot(hc,labels=dataCAH$Species)
rect.hclust(hc, k = 3)

#pour obtenir les groupes
groupes.cah <- cutree(hc, k = 3)
groupes.cah
table(groupes.cah)

#On compare la classification obtenue avec les failures dans dataCAH
table(dataCAH$failures, groupes.cah)

#On peut comparer les classifications obtenues avec kmeans et avec CAH :
table(groupes.cah, kmeans.result$cluster)

#comparaison entre les deux classification avec l'ARI : mclust
library("mclust")
adjustedRandIndex(kmeans.result$cluster,groupes.cah)
#ARI prends des valeurs entre 0 et 1 : 0 si les deux classifications
#sont al?atoires et 1 s'ils sont en accord
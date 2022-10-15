setwd("C:/Users/Amine/Documents/#Polytech/MAIN 4/S2/Analyse de données/Projet")
data = read.csv2("student/student-mat.csv",sep=";",header=TRUE)
head(data)


# on enlève les notes du S1, S2 pour ne garder que la moyenne finale car sinon trop corréler
# entre elles. De plus, on a parfois des 0 comme moyennes qui peuvent poser prolème
# on crée une colonne average qui sera la moyenne entre la note au S1 et au S2
for(i in 1:dim(data)[1])
{
  data$Average[i] = (data$G1[i] + data$G2[i])/2 
}


data$guardian = NULL
data$age      = NULL
data$nursery  = NULL
#data$internet = NULL
data$reason   = NULL
data$address  = NULL
data$sex      = NULL
data$school   = NULL
data$G1       = NULL
data$Pstatus  = NULL
data$G2       = NULL
data$G3       = NULL


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
# famsize, Medu,Fedu,studytime,failures,goout,Dalc,Walc,Average
dataPCA = data[,-c(4,5,6,9,10,11,12,13,14,15,16,20,21)]
str(dataPCA)
library(FactoMineR)
res = PCA(dataPCA,scale.unit = TRUE, graph = FALSE, quali.sup = 1) 
res$eig
barplot(res$eig[,2])
plot(res,choix = "var")
plot(res,choix = "ind", habillage = 7, cex = 0.7)
# AXE 1 : à droite étudiants ne travaillant pas bcp vs étudiants sérieux
# AXE 2 : en haut étudiants ayant des parents éduqué vs non éduqués 

# graphe des individus : on voit clairement deux groupes : en bleu, les etudiants avec une bonne
# moyenne, sérieux et en rouge les étudiants festifs et échouant 

 
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

#LDA
res.afd.lda = lda(failures ~., data=dataLDA)
res.afd.lda
plot(res.afd.lda)

F12 = predict(res.afd.lda, prior=rep(1/3,3))$x
cercle_correlation=cor(dataLDA[,1:7],F12)
cercle_correlation

a=seq(0,2* pi,length=100)
plot(cos(a), sin(a), type='l',lty=3,xlab='Dim 1', ylab='Dim
2',main="Cercle des corrélations" )
arrows(0,0,cercle_correlation[,1],cercle_correlation[,2],col=2)
text(cercle_correlation,labels=colnames(dataLDA[,1:7]))

# QDA 
dataQDA = dataLDA
res.qda = qda(failures ~., data=dataQDA)
res.qda
str(dataQDA)

# Nouveau dataframe pour la prédiction 
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


#CART
# On étudie la probabilité pour un lycéen d'appartenir a une famille nombreuse ou non
# en fonction de différents critères
# la variable à prédire est la variable qualitative famsize à deux modalités 
# LE3 : moins de freres/soeurs, GT3 + de 3
dataCart = data
dataCart = data[,-c(2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,21)]
str(dataCart)

# famsize, studytime,Walc,absences,Averages
library(rpart)
library(rpart.plot)
arbre = rpart(famsize ~., dataCart,control = rpart.control(minsplit = 5))
print(arbre)
rpart.plot(arbre,type = 0)
# arbre plutôt cohérent 

# Elagage
arbre.opt = rpart(famsize ~., dataCart,control = rpart.control(minsplit = 5,cp = 0))
set.seed(1)
printcp(arbre.opt)
plotcp(arbre.opt)
# on va rester avec cp = 0

#Prédiction 
PredCartGood = data.frame(studytime = 4,Walc = 1,absences = 0,Average = 16.5)
PredCartBad = data.frame(studytime = 1,Walc = 3,absences = 40,Average = 7)
predGood= predict(arbre, newdata=PredCartGood, type="prob")
predBad= predict(arbre, newdata=PredCartBad, type="prob")
predGood
predBad

# 66% de chance que le 1er soit dans une famille de 3 personnes ou moins
# 100 % de chance que le second ait plus de 3 personnes dans sa famille


# RANDOM FOREST
dataRF = dataCart
str(dataRF)
library(randomForest)
forest <- randomForest(famsize~.,dataRF)
forest
p=ncol(dataRF)
sqrt(p)
# 2 variables sélectionnes à chaque étapes

print(forest)
# Importance des variables 
importance = forest$importance
importance
barplot(t(forest$importance),col = "blue")
# score pour chaque variable
arbre.opt$variable.importance
# on a le même ordre d'importance entre Cart et RF






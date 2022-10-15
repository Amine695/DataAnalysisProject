
# Importation du jeu de données
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

# supression des colonnes inutiles
data$guardian = NULL
data$age      = NULL
data$nursery  = NULL
data$internet = NULL
data$reason   = NULL
data$address  = NULL
data$sex      = NULL
data$school   = NULL
data$G1       = NULL
data$Pstatus  = NULL
data$G2       = NULL
data$G3       = NULL

# on change la colonne failure en binaire, 0 : n'a pas redoublé, 1: à redoublé au moins 1 fois
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

head(data)
str(data)


# On veut appliquer une régression multiple
# on crée un sous dataframe contenant uniquement des variables quantitatives
dataRM = data
#studytime,famrel,goout,absences,average
dataRM = data[,-c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,24,26,27,28)]
attach(dataRM)
str(dataRM) # on vérifie

boxplot(dataRM)

res=lm(Average ~., dataRM)

#etude colinéarité
library(car)
vif(res)  
# tous < 10 : OK

# Analyse des résidus
par(mfrow=c(2,2))
plot(res)

# on passe au sqrt
# 
res=lm(sqrt(Average) ~., dataRM)
par(mfrow=c(2,2))
plot(res)

shapiro.test(res$residuals)  
# p-value < 5% => résidus non gaussiens

# on veut enlever les points extremes
abs(rstudent(res))[abs(rstudent(res))>2]
dataRM2=dataRM[-c(138),]
res=lm(Average ~., dataRM2)
par(mfrow=c(2,2))
plot(res)


summary(res)
# Test de Fisher => p-value < 5% => au moins une variable significative

# On cherche le sous-modèle pertinent
resBON = lm((Average) ~ studytime+famrel+absences, dataRM5)
anova(resBON, res)





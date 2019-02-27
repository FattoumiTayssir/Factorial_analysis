library(FactoMineR)
#Question 1 et 2
##Affichage
data(decathlon)
View(decathlon)
attach(decathlon)
#Description
str(decathlon)
#La base de données {Decathlon} se trouve dans le {package FactoMineR}. Les données contiennent les performances d' athlètes 
#lors de deux compétitions.
#Le tableau de données contient 41 lignes et 13 colonnes.
#Les colonnes de 1 à 12 sont des variables continues: les dix premières colonnes correspondent aux performances 
#des athlètes pour les dix épreuves du décathlon et les colonnes 11 et 12 correspondent respectivement au rang et 
#au nombre de points obtenus. La dernière colonne est une variable qualitative correspondant au nom de la compétition (Jeux Olympiques de 2004 ou Decastar 2004).

#Question 3
library(plyr)
ddply(decathlon, c("Competition"), summarise,
      N = sum(100m),
      mean = mean(100m),
      sd = sd(100m))

ddply(decathlon, c("Competition", "Points"), summarise,
      N = sum(Javeline),
      mean = mean(Javeline),
      sd = sd(Javeline))

#Question 4
library(corrgram)

corrgram(X[,-12], order=TRUE,lower.panel=panel.conf)
#Question 5
#des unités différentes et pour pouvoir les comparer,il faut les normaliser
#Question 6 7 et 8

res.pca <- PCA(decathlon,quanti.sup=11 :12,quali.sup=13)
round(res.pca$eig,2)
barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=paste("dim",1 :nrow(res.pca$eig)))
# Les deux premiers facteurs principaux résument 50% de l'inertie totale, 
#'est à dire 50% de la variabilité totale du nuage des individus (ou variables) 
#est repr´esent´ee par le plan. Cependant, la variabilit´e des performances 
#ne peut pas être résumée par les 2 premières dimensions seulement. 
#Il peut également très intéressant d'interpréter composantes 3 et 4 dont 
#l'inertie est sup´erieur 1 (cette valeur est utilis´ee comme une r´ef´erence
#car elle repr´esente, dans le cas de variables standardis´ees, la contribution 
#d'une seule variable).


#Questions restantes
res.pca$ind
round(cbind(res.pca$ind$coord[,1 :4],res.pca$ind$cos2[,1 :4],res.pca$ind$contrib[,1 :4]),2)
res.pca$quali.sup
round(cbind(res.pca$quali.sup$coord[,1 :4],res.pca$quali.sup$cos2[,1 :4],res.pca$quali.sup$vtest[,1 :4]),2)
plot(res.pca,choix='var',axes= 2:3)
plot(res.pca,choix='ind',habillage=13,cex=0.7)
Z <- cbind.data.frame(decathlon[,13],res.pca$ind$coord) 
cercle <- coord.ellipse(Z,bary=TRUE) 
plot.PCA(res.pca,habillage=13,ellipse=cercle,cex=0.8)
library(ade4)
Xpca=dudi.pca(decathlon[,-c(13)],scannf=F,scale=T,nf=2)
biplot(Xpca)

#Aller plus loin
##Package factoextra fournit des graphes plus clairs
require(devtools)
require(factoextra)
fviz_screeplot(res.pca, ncp=10)
fviz_pca_ind(res.pca,geom = "point",col.ind.sup = 'gray')
fviz_pca_ind(res.pca,geom = "text",col.ind.sup = 'gray')
fviz_pca_ind(res.pca,geom = "text",col.ind="cos2")+
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.5)
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.6) + 
  theme_minimal()
##pca en 3D
library(pca3d)
pca3d( res.pca3, group= decathlon$Competition, 
       fancy= TRUE, bg= "white", 
       axes.color= "black" )
##varimax
library(psych)
acp.varimax <- principal(r=cor.matrix, nfactors=3, rotate="varimax")
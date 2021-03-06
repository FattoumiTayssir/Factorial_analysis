#ACM
#Tracer la typologie des indiv
#Liaison entre les vars qualitatives
#Les variables qui se repoussent ou s'attirent

#ACM il faut que les toutes les var soient qualitatives

#Q1
credit<-read.csv("Credit.csv",header=T , sep=";",dec=".") 
#Q2
str(credit)
credit

#On doit convertir la var age en qualitative par l'aura besoin
credit[,"Age"]<-as.factor(credit[,"Age"])

#Les modalit�s rarent peuvent biaiser les sorties de l'ACM
#1er solution Regroupement naturel: On peut ajouter ces modalit�s � d'autre modalit�s
#2�me solution Ventilation : level.ventil=0.05 dans la fonction MCA ventile les modalit�s rares 
#3�me solution Suppression

#Q3
for(i in 1:ncol(credit)){
  par(ask=TRUE) #Permet de voir graphe le 1 par 1
    plot(credit[,i])
}
levels()
library("FactoMineR")
#Un seul indiv prent la modalit� Sidecar de la var Mache
#On r�alise un regroupement naturel de Sidecar avec Moto
levels(credit[,"Marche"])[5]<-"Moto"

#Q4
res.acm=MCA(credit,quali.sup=6:11,level.ventil=0)
#Le param level.ventil est par d�fault nul, si cet arg vaut 0.05 cela signifie 
#que la modalit� d'effectif inferieure ou �gale � 5% du total
#d'indiv sont ventil� de fa�on automatique par la fonction MCA

barplot(res.acm$eig[,1])
#La d�croissant des val propres est r�guliaires nous observant pas de cassures
#ou d�crochage flagrant donc il devient difficle de choisir les dimensions

res.acm$eig
#Les 2 premiers axe explique 28% de l'inertie totale autrement dit 28% de informations
#des donn�es est r�sum� par les 2 premiers dimensions

#Q6
plot(res.acm, invisible=c("var","quali.sup"))
#Ce graphe d'indiv d'apprecier l'allure g�n�rale de nuage de points
#pour voir s'il existe un groupe d'indiv particulier
#Ici ce n'est pas le cas


#Onutilise bcp l'habillage dans L'ACM on vise typologie
plot(res.acm, invisible=c("var","quali.sup"),habillage="Marche")
"le graphe des individus montre que les 2 premiers axes factorielle exprime presque 28% 
d'inertie,comme dans la plpart des traitement des donnes d'enquete le nuage des individus comporte bcp 
de point et on cherche uniq a voir si se degage une typologie remarquable or ce n'est le cas ici,
pour aider � l'interpretation ca sera interresant d'habiller en  fct de certaine variables"

#Q7
plot(res.acm,invisible="ind")
"1er axe ;il oppose un profil jeune a droite a  un  profil vieu a gauche ,on retrouve ainsi des personne 
plus g� propri�taire qui ont contract� des credit pour financer les traveaux des renoovtion oppose 
a des personne plus jeune pour acheter un scooter,une voiture ....les jeunes on tendance a avoir un taux
d'endettement plus elev�es"
"2eme axe oppose principalement les individus qui ont des difficult�s financi�res en bas onretrouve
ainsi en bas les personne qui ont difficult� a rembourser leur credit et qui sont souscrit
a l'assurance aid+chommage
"
#Q8
'on aura besoin des graphe des varible'
plot(res.acm,choix = "var")
"Note:la conribution de la variable est la somme des contribution de ces modalit�s"
res.acm$var$contrib
#Q9
res.acm$var$eta2
"projection numerique de Q8"
"et2 coorespond au raport de coorrelatiion entre la composante principale(coordones des individus sur 
la dim) et chaque variable qualitatif"
#Q10
dimdesc(res.acm)
#Q11
plot(res.acm, invisible=c("var","quali.sup"),habillage="Marche",axes = 3:4)

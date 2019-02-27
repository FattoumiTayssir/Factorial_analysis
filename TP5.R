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

#Les modalités rarent peuvent biaiser les sorties de l'ACM
#1er solution Regroupement naturel: On peut ajouter ces modalités à d'autre modalités
#2ème solution Ventilation : level.ventil=0.05 dans la fonction MCA ventile les modalités rares 
#3ème solution Suppression

#Q3
for(i in 1:ncol(credit)){
  par(ask=TRUE) #Permet de voir graphe le 1 par 1
    plot(credit[,i])
}
levels()
library("FactoMineR")
#Un seul indiv prent la modalité Sidecar de la var Mache
#On réalise un regroupement naturel de Sidecar avec Moto
levels(credit[,"Marche"])[5]<-"Moto"

#Q4
res.acm=MCA(credit,quali.sup=6:11,level.ventil=0)
#Le param level.ventil est par défault nul, si cet arg vaut 0.05 cela signifie 
#que la modalité d'effectif inferieure ou égale à 5% du total
#d'indiv sont ventilé de façon automatique par la fonction MCA

barplot(res.acm$eig[,1])
#La décroissant des val propres est réguliaires nous observant pas de cassures
#ou décrochage flagrant donc il devient difficle de choisir les dimensions

res.acm$eig
#Les 2 premiers axe explique 28% de l'inertie totale autrement dit 28% de informations
#des données est résumé par les 2 premiers dimensions

#Q6
plot(res.acm, invisible=c("var","quali.sup"))
#Ce graphe d'indiv d'apprecier l'allure générale de nuage de points
#pour voir s'il existe un groupe d'indiv particulier
#Ici ce n'est pas le cas


#Onutilise bcp l'habillage dans L'ACM on vise typologie
plot(res.acm, invisible=c("var","quali.sup"),habillage="Marche")
"le graphe des individus montre que les 2 premiers axes factorielle exprime presque 28% 
d'inertie,comme dans la plpart des traitement des donnes d'enquete le nuage des individus comporte bcp 
de point et on cherche uniq a voir si se degage une typologie remarquable or ce n'est le cas ici,
pour aider à l'interpretation ca sera interresant d'habiller en  fct de certaine variables"

#Q7
plot(res.acm,invisible="ind")
"1er axe ;il oppose un profil jeune a droite a  un  profil vieu a gauche ,on retrouve ainsi des personne 
plus gé propriétaire qui ont contracté des credit pour financer les traveaux des renoovtion oppose 
a des personne plus jeune pour acheter un scooter,une voiture ....les jeunes on tendance a avoir un taux
d'endettement plus elevées"
"2eme axe oppose principalement les individus qui ont des difficultés financiéres en bas onretrouve
ainsi en bas les personne qui ont difficulté a rembourser leur credit et qui sont souscrit
a l'assurance aid+chommage
"
#Q8
'on aura besoin des graphe des varible'
plot(res.acm,choix = "var")
"Note:la conribution de la variable est la somme des contribution de ces modalités"
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

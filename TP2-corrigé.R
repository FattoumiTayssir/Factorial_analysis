#1
library(FactoMineR)
#2
t<-read.csv(file=file.choose(),row.names = 1,header=T , sep=";",dec=".") 
t

#3
str(t)
summary(t)
names(t)

#4
res<-PCA(t,ind.sup =  24:35,quanti.sup = 13:16,quali.sup = 17)
res

plot(res)
plot.PCA(res)
plot(res , choix="ind",invisible="ind.sup") #qu'est ce que je veux masquer #enlever les indiv supp
#ind : indivisus 
#choix=var : afficher le cercle de corrélation 

#5
plot(res,choix="ind",habillage=17)

#Rq 
res$ind



#6
dimdesc(res)
# si cons2 tend vers 1 l'indiv ou la variable est tres projeté à la dimX par rapport 
#a la variable (position dans l'espace est tres proche de la projection )

#contribution : (9adech ysahem fel creation mta3 l espace jdid)
#eli 3andou l coord akther howa eli 3andou importance del construction des axes 
#l'effet de variable area sur les coord des indiv sur le 1er et le 2eme axe

round(res$ind$contrib,2)

#7
res$eig
#eigvalue , varience % , varience cummulé 
#on a une cassure remarquable fi 3eme axe 

#8
#9
res$ind
#si on veut voir les données sur les ind sup 
res$ind.sup

#10,11
sort(round(res$ind$contrib[,1],2)) 

#round -> arrondir les chiffres
#2: deux chiffres apres la virgules

sort(round(res$ind$contrib[,2],2))

#12
sort(round(res$ind$cos2[,1]+res$ind$cos2[,2],3))
#on fait la somme pour avoir le resultat final 

#14
res.pca$var
res$var
#15
#correclation : dim1 octobre #axe2 june

#16
res$quanti.sup

#17
res$quali.sup
#etat 2 =test anaova

#18
#scale
#Par défault centrés
scale(t[1:23,1:16])

#19
cor(t[,1:16])
cor(t[1:23,1:16])

#20
#pour k'interpretation on affiche les graphes et on fait des résumés sur le resultat 

La premiere composante principale est prédominante car elle represente pour elle seul 80% de l'inertie totale.

#INTERPRETATION
#la 1ere composatnte est dominante elle resume pour elle seule 82% de l'information qui existe des jeux de données
#de depart(inertie)
plot(res,choix="var")
#tOUTES LES variables actives sont du meme coté de la 1ere composatne (d'apres le cercle de corrélation )
#la variavles annual est corrolé positivement par rapport aux 
#annual : variable illustratif
#avril septembre octobre represente mieux  temperature annual
#ils sont plus etroitement liée à ces autres à ces composantes 
#laltitude est bien representé
#laltitude est très corrélée négativement 
#la correlation entre la latitude et la 1ere compsante ^rincipale vaut -0.85
#ce qui signifie que les villes qui sont plus au sud (latitude inferieur ) ont une
#plus grandes coordonnées sur la compsante et sont donc les villes les plus chaudes 

plot(res , choix="ind",invisible="ind.sup")
#les villes qui se trouve au centre sont à profil moyen 

# la 2eme composante principale est importante car elle resume 15.4% de l'inertie totale
#les mois d'été sont corrolé positivement et les mois d'hiver sont corrolé negarivement 
#les villes en haut sont chaudes pendant l'ete et froides en hiver (amplitude thermique est tres elevé)
#l'amplitude thermique lié à cette composante 

#Réduction en 2 dimensions temperature annualle et amplitude thermique

#21

#cONCLUSION :
#les temperatures peuvent etre resumé par des deux variables syntétique 
#la temperature annual et la temperature thermique à partir de ces deux variables nous pouvon créeer les typologies des villes 
#en réunissant les villes qui sont proches sur le plan factoriel et en resperctant le plan de l'emplacement geographique nous
# pouvant proposer la typologie suivante :
#ville d'europe mérdionnale caractérisé par des tempertature elevée tout au long de l'année 
#ville d'europe occidentale caracterisé par des temperature moyenne sur toute l'année 
#ville d'europe du nord caracterisé par des temperatures faibles en particulier pendant l'ete


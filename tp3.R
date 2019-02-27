  baseC=read.csv(file.choose(),sep = ";",row.names = 1,header = T)
baseC  
library("FactoMineR")
res.pca=PCA(baseC,ind.sup = 8:18,quanti.sup = 27:30)
res.pca$eig
#3 pour obtenir une typologie des classe d'age fondé sur leur depense on defint la distance entre 2 classe d'ages
#uniquement sur la base de leurs depenses sur les diff rubrique
#toto ne sont pas des variable mais considéées comme des variable supplémentaire
#6
barplot(res.pca$eig[,1],)
#7
plot.PCA(res.pca,choix = "ind",invisible = "ind.sup")
"Le 1er axe oppose les tranches d'age moyenne au tranche d'age extreme
Pour l'axe 2 range les tranches d'age de la plus haute au plus basse"
'c-'
res.pca$ind
sort(round(res.pca$ind$cos2[,1]+res.pca$ind$cos2[,2],2))
#8
'a-'
plot.PCA(res.pca,choix = "var",invisible = "quanti.sup")
'b-'
"tte les variables sont correle positivement au 1er axe sauf gaz et eau "
"les tranches d'ages moins agées sont plus liées au  "
'c'
res.pca$var
sort(round(res.pca$var$contrib[,1],2))
sort(round(res.pca$var$contrib[,1]+res.pca$var$contrib[,2],3))


'd-'
sort(round(res.pca$var$cos2[,1]+res.pca$var$cos2[,2],2))
""
'-e,f'
'matrice de correlation'
round(cor(baseC[,c(1,4,17)]),2)
"les 3 var ne sont pas correllées"
'g'
plot.PCA(res.pca,choix = "var",invisible = "var")

res.pca$quanti.sup$cor
"correllation tres forte alimentaire et 1 er axe de variabilité"
#9
"1er axe : oppose les tranches d'age qui depense bcp avec celle qui consomme pe cette 1er composante est tres
liées au depense alimentaire ce qui est bien resumé par la forte coorelation de la var supp total aimentation avec
le 1er axe(0.99)
rappelond=s aussi que la 1er composante principale est la comb lineaire de l'nensemble des var qui la 
synthetise le mieu
le 2em axe oppose principalement de poisson huile santé avec logement gaz electricite tabac communication
restauration cet axe separe globalement les classe d'age qui depense peu
les moins agé consomme tabac et les personne agée presente un profil de depense opposée
"
"Enrichir l'interpretation"
'1-'
plot.PCA(res.pca,choix = "ind",invisible = "ind")
"on peut interpreter que la consommation s"
'2-'
"anallyse conjointe entre individus et variable"
plot.PCA(res.pca,choix = "ind",axes = 2:3)
plot.PCA(res.pca,choix = "var",axes = 2:3)
"l'axe 3 est essentiellement est tres liée a la var enseignement 
il oppose principalement les tranches d'ae 25-34 au tranxhe moins de 25 et 45-54 qui depense que les aures
en enseignement
"

library(FactoMineR)
data("JO")
View(JO)
summary(JO)
str(JO)
#2--le tableau de données croise en lignes les épreuves d'athlétisme et en colonnes les différents pays
#chaque cellule contient le nombre total de médailles(or,argent et bronze obtenues lors des olympiades de 1992 à 2008)
#3--la problématique associé à ce tableau consiste à étudier la liaison entre les deux variables épreuve
#et pays cad la question qui se pose peut on considérer que tous les pays ont le mem profil athlétique
#ou au contraire certains pays so,t ils plutot performants dans certaines épreuves?Peut on considérer que toutes les
#épreuves ont le même profil géographique ou au contraire certaines épreuves sont elles la spécialité de certaines pays?
#4--
res.ca <- CA(JO)
res.ca
#5-- les deux premiers axes expriment 24.4% d'inertie total.il peut
#etre interessant d'interpréter les axes suivants
round(res.ca$eig,2)
barplot(res.ca$eig[,1], main="Valeurs propres",names.arg=paste("dim",1:nrow(res.ca$eig)))
plot(res.ca, invisible="row")
#6--le premier axe factoriel sépare les épreuves de courses d'endurance par rapport aux autres épreuves
#toutes les épreuves sont triées de la plus longue distance 10000m à la plus courte 800m sans aucune
#exception.les pays qui ont des coordonnées négatives sur le premier axe sont les pays qui gagnent de nombreuses médailles dans les épreuves
#d'endurance parmi lesquels on trouve plusieurs pays africains(erithrée,éthiopie,burundi,Maroc,qatar,kenya)
res.ca$col$contrib[rev(order(res.ca$col$contrib[,1])),1]
#Ethiopie,kenya et le maroc ont contribué pour 65% à la construction de la première dimension.ce sont
#ce sont des pays qui ont obtenu beaucoup de médailles.
res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])),2]
#le 2 eme axe sépare les épreuves de sprint des épreuves de lancers du disque et du marteau et des épreuves des marches
#les pays qui gagnent des médailles en sprint sont la barbade ,la namibie,Trinidad  et tobago..
#les USA ont fortement contribué à la formation de l'axe 2. ceci s'explique par le nombre très important 
#de médailles qu'ils ont obtenues:82 au total
#Pour les épreuves de lancers du marteau et du disque,on retrouve que les pays de l'est Honggrie,slovinie et turquie
#sont les plus performants
#8--

plot(res.ca, axes = 3:4)
#les axes 3 et 4 séparent a nouveau disque et marteau des épreuves de marche (20km et 50 km)
#le javelot est une épreuve de lancer vraiment différente des épreuves de marteau et du disque
#les pays nordiques (norvege ,tchikoslovaquie,tchéquie,Finlande,lettonie)sont performants au lancer du javelot
#9--les grandes tendances qui se dégagent des données:
#1//les épreuves d'endurance sont dominées par les athletes africains
#2//le sprint est dominé par l'USA
#3//sprint,endurance et lancers sont des épreuves assez différentes 


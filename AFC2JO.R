library(FactoMineR)
data("JO")
View(JO)
summary(JO)
str(JO)
#2--le tableau de donn�es croise en lignes les �preuves d'athl�tisme et en colonnes les diff�rents pays
#chaque cellule contient le nombre total de m�dailles(or,argent et bronze obtenues lors des olympiades de 1992 � 2008)
#3--la probl�matique associ� � ce tableau consiste � �tudier la liaison entre les deux variables �preuve
#et pays cad la question qui se pose peut on consid�rer que tous les pays ont le mem profil athl�tique
#ou au contraire certains pays so,t ils plutot performants dans certaines �preuves?Peut on consid�rer que toutes les
#�preuves ont le m�me profil g�ographique ou au contraire certaines �preuves sont elles la sp�cialit� de certaines pays?
#4--
res.ca <- CA(JO)
res.ca
#5-- les deux premiers axes expriment 24.4% d'inertie total.il peut
#etre interessant d'interpr�ter les axes suivants
round(res.ca$eig,2)
barplot(res.ca$eig[,1], main="Valeurs propres",names.arg=paste("dim",1:nrow(res.ca$eig)))
plot(res.ca, invisible="row")
#6--le premier axe factoriel s�pare les �preuves de courses d'endurance par rapport aux autres �preuves
#toutes les �preuves sont tri�es de la plus longue distance 10000m � la plus courte 800m sans aucune
#exception.les pays qui ont des coordonn�es n�gatives sur le premier axe sont les pays qui gagnent de nombreuses m�dailles dans les �preuves
#d'endurance parmi lesquels on trouve plusieurs pays africains(erithr�e,�thiopie,burundi,Maroc,qatar,kenya)
res.ca$col$contrib[rev(order(res.ca$col$contrib[,1])),1]
#Ethiopie,kenya et le maroc ont contribu� pour 65% � la construction de la premi�re dimension.ce sont
#ce sont des pays qui ont obtenu beaucoup de m�dailles.
res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])),2]
#le 2 eme axe s�pare les �preuves de sprint des �preuves de lancers du disque et du marteau et des �preuves des marches
#les pays qui gagnent des m�dailles en sprint sont la barbade ,la namibie,Trinidad  et tobago..
#les USA ont fortement contribu� � la formation de l'axe 2. ceci s'explique par le nombre tr�s important 
#de m�dailles qu'ils ont obtenues:82 au total
#Pour les �preuves de lancers du marteau et du disque,on retrouve que les pays de l'est Honggrie,slovinie et turquie
#sont les plus performants
#8--

plot(res.ca, axes = 3:4)
#les axes 3 et 4 s�parent a nouveau disque et marteau des �preuves de marche (20km et 50 km)
#le javelot est une �preuve de lancer vraiment diff�rente des �preuves de marteau et du disque
#les pays nordiques (norvege ,tchikoslovaquie,tch�quie,Finlande,lettonie)sont performants au lancer du javelot
#9--les grandes tendances qui se d�gagent des donn�es:
#1//les �preuves d'endurance sont domin�es par les athletes africains
#2//le sprint est domin� par l'USA
#3//sprint,endurance et lancers sont des �preuves assez diff�rentes 


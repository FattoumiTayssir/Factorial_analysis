#Appication1
#1
data1=chickwts
data1
str(data1)
summary(data1)
#2
tapply(data1$weight,data1$feed,summary)
#3
plot(data1$weight~data1$feed,data=data1)
#4
modele=lm(data1$weight~data1$feed,data = data1)

summary(modele)
#5:test de normalité
shapiro.test(modele$residuals)
#5B:test d'homosedasticité(homogénité des variance)
"
H0:Varaiance des classes similaires
H1:contre H0

"

bartlett.test(data1$weight~data1$feed,data=data1)
#6
anova(modele)
#p-value<0.05 rejet de H1 
#on voit ainsi un  efet tres marque de  type de nourriture sur la masse des poulets


#Application2
bout=as.data.frame(de(""))
bout
d=stack(bout)$values
t=stack(bout)$ind
?stack()
stb=stack(bout)
tapply(stb$values,stb$ind,summary)
plot(stb$values~stb$ind,data=data1)
#homosedacticite
aov(values~ind,data = stb)

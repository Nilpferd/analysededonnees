#08/12/16
#Adrien

table2=read.csv("../donnees/donnees_par_hab.csv",sep=";")

table3<-table2[,c(9,10,11,14,17,19,20,21,22,23,24,27,29)]
table3<-table3[1:(nrow(table3)-1),]

table3[is.na(table3)] <- 0
M=max(table3[,"TP6013"])
table3[,"TP6013"]<-table3[,"TP6013"]/M

#http://eric.univ-lyon2.fr/~ricco/cours/didacticiels/R/cah_kmeans_avec_r.pdf

#ATTENTION ! CALCUL TRES LONG
distance<-dist(table3)

write.table(distance, "distance_CAH.csv", row.names=TRUE, sep=";")
distance=read.csv("../donnees/distance_CAH.csv",sep=";")

#CAH 
cah.ward<-hclust(distance,method="ward.D2")
#Erreur : impossible d'allouer un vecteur de taille 5.0 Go
plot(cah.ward)

#Pour remédier à ça, je suis forcé de faire un échantillonnage.

selection_indices=sample(1:nrow(table3),size=1000)

distance_sample=dist(table3[selection_indices,])

cah.ward<-hclust(distance_sample,method="ward.D2")
rect.hclust(cah.ward,k=4)
reduit<-cutree(cah.ward,k=4)
print(sort(reduit))
plot(cah.ward,labels = FALSE,hang = -0.1)





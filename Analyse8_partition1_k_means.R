#07/12/16
#Adrien

library(FactoMineR)

#Charger la base de données
#cat("Chargement de la base de données...")
table = read.csv("../donnees/base_cc_resume_20161013_COM.csv")

#Emprunt de code à Rodolphe pour travailler sur les taux

#Modifications de la base de données
table2 <- data.frame()
table2 = table
table2$WEIGHT <- NULL
conditionNORM = !(colnames(table2) %in% c("CODGEO",
										  "LIBGEO",
										  "REG",
										  "DEP",
										  "PIMP13",
										  "MED13",
										  "TP6013"))
conditionNORM2 = colnames(table2[conditionNORM])
pb <- txtProgressBar(min = 1, max = nrow(table2), initial = 1, char = "=")
for(i in 1:nrow(table2)){
	#table2[i,conditionNORM2] = table2[i,conditionNORM2]/table2[i,"P13_POP"]
	if(!is.na(table2[i,"P13_POP"])&(table2[i,"P13_POP"]>0)){
		table2[i,conditionNORM2] = table2[i,conditionNORM2]/table2[i,"P13_POP"]
	}else if(!is.numeric(table2[i,"P13_POP"])){
		table2[i,conditionNORM2] = 0
	}else{
		table2[i,conditionNORM2] = 1
	}
	setTxtProgressBar(pb,i)
}
Sys.sleep(1)
close(pb)
write.table(table2, "../donnees/table2.csv", row.names=TRUE, sep=";")
#Cette table est longue à calculer, cette ligne de commande permet donc de la sauvergarder
#pour repartir de là pour de prochaines études de la base de données
View(table2)
summary(table2)

table2=read.csv("../donnees/donnees_par_hab.csv",sep=";")

#k-means clustering

#Beaucoup de problèmes viennent du fait que le dataframe contient des strings et des numerics,
#ces derniers sont alors convertis en strings ce qui pose des problèmes lros du traitement
#par des algorithmes numériques comme le clustering.

#Les accents dans les noms posent aussi problème !  Je les remplace.
table3<-table2[,c(9,10,11,14,17,19,20,21,22,23,24,27,29)]
table3<-table3[1:(nrow(table3)-1),]
#do.call(data.frame,lapply(table3, function(x) replace(x, is.infinite(x),NA)))
#do.call(data.frame,lapply(table3, function(x) replace(x, is.nan(x),NA
#table3[,4][!is.finite(table3[,4])]<-NA
table3[is.na(table3)] <- 0 #Ce n'est peut-être pas comme ça qu'il faut faire mais j'essaye déjà d'obtenir un résultat.

#Bon, je n'ai pas encore réussi à retirer tous les infinis.

res<-kmeans(table3,5)

#Within cluster sum of squares by cluster:
#[1] 1.070158e+10 2.850978e+03 9.198318e+09 9.445573e+09 1.211329e+10
# (between_SS / total_SS =  97.5 %)

#97.5% de l'intertie est expliquée par cette partition !

#http://eric.univ-lyon2.fr/~ricco/cours/didacticiels/R/cah_kmeans_avec_r.pdf

#(1)évaluer la proportion d'inertie expliquée
inertie.expl<-rep(0,times=10)
for (k in 2:10){
	clus<-kmeans(table3,centers=k,nstart=5)  
	inertie.expl[k] <-clus$betweenss/clus$totss
}
#graphique
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#d'après ce graphe, on voit qu'avec une partition en 5 classes on a déjà presque atteint l'asymptote
#donc je conserve cette partition.

res$size
#1125  3795 14648  6769 10354
#On constate que deux des partitions comportent plus de 10 000 individus sur 36 691.

res$centers

rm(n)
n=ncol(res$centers)
ecart_type=vector()
moy=vector()
for(k in 1:n){
	moy=append(moy,mean(res$centers[,k]))
	ecart_type=append(ecart_type,sd(res$centers[,k]))
}

#    P13_MEN     NAISD15    DECESD15 P13_RSECOCC NBMENFISC13    MED13   TP6013  P13_EMPLT P13_EMPLT_SAL P08_EMPLT P13_POP1564    ETTOT14      ETBE14
#1 0.3967395 0.010649399 0.007338570  0.03297283 0.387774270 29287.71 1.037333	0.2532778     0.2069721 0.2405153   0.6532736 0.10048201 0.005650706
#2 0.4594611 0.017281537 0.018441670  0.31690582 0.008959157     0.00 0.000000	0.2111719     0.1112282 0.2212703   0.6075431 0.15805056 0.017668473
#3 0.4142005 0.010663570 0.009413242  0.09046046 0.408033928 20109.59 1.477813	0.2155394     0.1612261 0.2130973   0.6236585 0.09246546 0.006627668
#4 0.3980083 0.011140965 0.007765595  0.05644881 0.394217701 23206.03 1.015660	0.2188966     0.1735562 0.2113544   0.6472300 0.09146952 0.006036616
#5 0.4379833 0.009625638 0.012062045  0.15622101 0.423752944 17535.35 2.494591	0.2427120     0.1658270 0.2472431   0.5986265 0.10911387 0.008471251
# __________________________________________________________________________________________________________________________________________________
#mean 0.42
#sd 2.7-02 		3.07e-03 4.55e-03 		1.14e-01 1.77e-01 	1.1e+04 	9.02e-01 1.85e-02 		3.44e-02 1.63e-02 	2.39e-02 	2.76e-02 	5.02e-03

#On voit que pour MED13 le centre de la classe 2 est à 0 alors que les autres sont à plus de 17 000. 
#Regardons l'écart-type entre les MED13 des quatre autres centres.

sd(res$centers[c(1,3,4,5),"MED13"])
# 5063.872

#Conclusion : c'est quasiquement uniquement sur MED13 qu'a été faite la partition.
#Vérification en utilisant uniquement MED13.

verif<-kmeans(table3[,"MED13"],5)

verif$size
#1125 10354 14648  3795  6769
#Et oui, exactement la même !



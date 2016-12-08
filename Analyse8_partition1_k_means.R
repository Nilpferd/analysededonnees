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

#Je retire les NA
table2[is.na(table2)] <- 0

#Je normalise la colonne MED13
M=max(table2[,"MED13"])
table2[,"MED13"]<-table2[,"MED13"]/M

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
#donc je conserve cette partition. (sans normaliser MED13)

#En normalisant MED13, il vaut mieux 6 classes. 
res<-kmeans(table3,6)

res$size
#1125  3795 14648  6769 10354 (sans normaliser MED13)
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

#Sans normaliser MED13

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

# =================================================================	   #

#En normalisant MED13

#    P13_MEN     NAISD15    DECESD15 P13_RSECOCC NBMENFISC13      MED13  TP6013     P13_EMPLT P13_EMPLT_SAL P08_EMPLT  P13_POP1564   ETTOT14 ETBE14 
#1 0.4407960 0.011579130 0.012104120  0.05854766  0.42892974 0.38745319 20.20544218 0.4267735    0.37581594 0.4334414   0.6146311 0.09153859 0.005622892
#2 0.4576977 0.008219205 0.009806247  0.36762264  0.04787681 0.03963549 0.00000000  0.1818303    0.08202334 0.1854765   0.6019552 0.14691689 0.008774387
#3 0.6070916 0.255987638 0.255470275  2.22344408  0.49856601 0.30020741 0.02232143 1.4851213    1.35285699 1.5708605   0.7342312 0.60591500  0.258364608
#4 0.4140972 0.009688207 0.008594728  0.08198191  0.40601520 0.43812031 0.00000000 0.1559460    0.09851236 0.1557841   0.6226489 0.09226505  0.006072620
#5 0.4212227 0.010670396 0.009813619  0.06246566  0.41866663 0.46920131 9.60588016 0.3618682    0.31252512 0.3536864   0.6233221 0.09230449  0.005722906
#6 0.4197615 0.009203962 0.012184139  0.08654196  0.39155704 0.42175342 0.00000000 0.5900582    0.50786565 0.5775470   0.6144273 0.12828825  0.009973414
#_______________________________________________________________________________________________________________________________________________________
#m 0.46011111 0.05089142 0.05132885 0.48010065 0.36526857 0.34272852    4.97227396 0.53359959 0.45493324 0.54613263   0.63520263   0.19287138  0.04908847
#e 0.07381034 0.10048295 0.10001845 0.86225408 0.15985068 0.15938232    8.39276207 0.49312238 0.46940519 0.52597221 0.04912351 	   0.20366222  0.10253966
#

#Même problème avec TP6013

#Normalisons TP6013 !

M=max(table3[,"TP6013"])
table3[,"TP6013"]<-table3[,"TP6013"]/M

#Bon, ce coup-ci l'intertie expliquée à franchement diminué : l'asymptote est à 0.8.
#Du coup il faut que je regarde plus loin.

inertie.expl<-rep(0,times=20)
for (k in 2:10){
	clus<-kmeans(table3,centers=k,nstart=5)  
	inertie.expl[k] <-clus$betweenss/clus$totss
}
#graphique
plot(1:20,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#Très très surprenant ce graphique ! Je vais partir sur 10 classes donc.

res<-kmeans(table3,10)

res$size
#2970 19046  6951   608   152     2   619  3337  2932    74
#Il y a deux clusters qui ne représentent presque rien (2 et 74 individus) et 1 qui est très important (19046 individus).

res$centers

#     P13_MEN     NAISD15    DECESD15 P13_RSECOCC NBMENFISC13     MED13	TP6013  	P13_EMPLT 	P13_EMPLT_SAL  P08_EMPLT P13_POP1564   ETTOT14		ETBE14
#1  0.4329414 0.010005556 0.012523169  0.04878499   0.4143830 0.4211632	0.180119716  0.5318338    0.46388036  0.5315064   0.6125318 	0.1117867	0.008081308
#2  0.4058659 0.010097167 0.007691720  0.05108326   0.3994124 0.4459022	0.006379876  0.1232762    0.07449667  0.1225782   0.6294660 	0.0829444	0.005149697
#3  0.4152120 0.010049035 0.010576589  0.04771979   0.4073161 0.4387568	0.067232532  0.2854828    0.22192284  0.2821890   0.6170444 	0.1011786	0.007321578
#4  0.4187858 0.010105041 0.009928786  0.05147253   0.3815575 0.4270150	0.094042398  0.9708930    0.88876747  0.9493489   0.6369329 	0.1339419	0.009900489
#5  0.6262624 0.374407747 0.373423676  0.43470946   0.5806940 0.3229974	0.015935673  1.9463445    1.87795869  1.9622367   0.7769423 	0.4770301	0.378051344
#6  0.3832101 0.007412637 0.002470879  0.35397708   0.1809036 0.2437136	0.000000000 21.4680422   20.60376400 24.6096872   0.7981499 	1.0895199	0.031449898
#7  0.5142365 0.006064322 0.011297556  1.40254297   0.2455290 0.2095314	0.025668641  0.2783582    0.16789476  0.2791114   0.5824049 	0.1993217	0.009830711
#8  0.4465710 0.008703502 0.009483160  0.19466509   0.0000000 0.0000000	0.000000000  0.1735917    0.07553994  0.1784845   0.6057074 	0.1412687	0.008502877
#9  0.4729099 0.006998680 0.011768122  0.39443289   0.4556770 0.3911219	0.011459754  0.2011797    0.10400890  0.2045393   0.5867260 	0.1358259	0.009373916
#10 0.5192453 0.007795057 0.005811846  4.62945980   0.3419386 0.3255564	0.011411411  0.7558063    0.55688506  0.7419448   0.6635308 	0.7024706	0.008100475
#_______________________________________________________________________________________________________________________________________________________________
#s  0.07271065 0.11569394 0.11526170 1.41920276 0.16209414 	  0.14027571 0.05776135 6.62681090 		6.38384096 7.61793134 0.07590474 0.33755967 	0.11635330

#bien alors ici on voit 4 variables d'écarts-type supérieurs à 1 : P13_RSECOCC, P13_EMPLT, P13_EMPLT_SAL et P08_EMPLT.
#Ce sont donc les logements secondaires et logements occasionnels et les emplois qui permettent de faire cette classification.

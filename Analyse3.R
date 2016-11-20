#Charger le fichier et les bibliothèques
setwd("~/Documents/[2014-2017] Mines ParisTech/[S5] Analyse de données/Mini-projet")
library(FactoMineR)

#Charger la base de données
cat("Chargement de la base de données...")
table = read.csv("base_cc_resume_20161013_COM.csv")
cat("OK\n")

#Modifier la base de données - enlever les variables non pertinentes
cat("\nModification de la base de données :\n")
cat("	Suppression des variables non pertinentes...")
conditionC = !(colnames(table) %in% c("CODGEO")) #,"LIBGEO","REG","DEP"))
table <- table[,conditionC]
cat("OK\n")

#Modifier la base de données - remplacer les taux par des effectifs
cat("	Remplacement des taux par des effectifs...")
conditionC = colnames(table) %in% c("MED13","TP6013","PIMP13")
	#Problème : plein de données manquantes
	#MED13 : 3793/36689 NA values -> 10%
	#TP6013 : 32531/36689 NA values -> 88%
	#PIMP13 : 31598/36689 NA values -> 86%
	#Calculé avec la commande : sum(is.na(table[,"TP6013"]))
#Nombre de pauvres
conditionL = !(is.na(table[,"TP6013"])) & !(is.na(table[,"P13_POP"]))
table[conditionL,"NBPAUVRES"] = floor(table[conditionL,"TP6013"] * table[conditionL,"P13_POP"] / 100)
#Nombre de ménages imposés fiscalement
conditionL = !(is.na(table[,"PIMP13"])) & !(is.na(table[,"NBMENFISC13"]))
table[conditionL,"NBMENPAYFISC13"] = floor(table[conditionL,"PIMP13"] * table[conditionL,"NBMENFISC13"] / 100)
#Cas particulier : le niveau de vie
	#A chaque ménage est associée à nombre d'unités de consommation
	#1UC pour le premier adulte, 0.5UC pour les autres personnes de plus de 14 ans, et 0.3UC pour les enfants de moins de 14 ans
	#Le niveau de vie est le quotient du revenu annuel du ménage et des unités de consommation
	#C'est le revenu du ménage par unité de consommation
	#On en trace la fonction de répartition
	#On prend le niveau de vie qui couvre 50% de la population de la commune
	#Mais on n'a aucune information supplémentaire sur le profil de la fonction de répartition
	#Ce n'est pas un taux, sa dimension est celle d'un revenu affecté à chaque unité de consommation
cat("OK\n")

#Modifier la base de données - créer de nouvelles variables pertinentes
cat("	Création de nouvelles variables...")
	#Densité de population en 2013 (hab/km2)
	conditionL = !(is.na(table[,"P13_POP"])) & !(is.na(table[,"SUPERF"]))
	table[conditionL,"DENSITE13"] = floor(table[conditionL,"P13_POP"] / table[conditionL,"SUPERF"])
	#Densité de population en 2008 (hab/km2)
	conditionL = !(is.na(table[,"P08_POP"])) & !(is.na(table[,"SUPERF"]))
	table[conditionL,"DENSITE08"] = floor(table[conditionL,"P08_POP"] / table[conditionL,"SUPERF"])
cat("OK\n")

#Analyse en composantes principales sur les taux
cat("\nAnalyse en composantes principales des taux :\n")
condInd = !(is.na(table[,"PIMP13"])) & !(is.na(table[,"MED13"])) & !(is.na(table[,"TP6013"]))
condVar = colnames(table) %in% c("PIMP13","MED13","TP6013")
SupInd = c()
SupVar = c()
cat("	Les variables considérées sont : ", colnames(table[,condVar]), ".\n")
taux.pca = PCA(
						table[condInd,condVar],
						scale.unit = TRUE,
						ncp = 5,
						quanti.sup = NULL,
						quali.sup = NULL,
						ind.sup = NULL,
						graph = T)
cat("\nAnalyse des données :\n")
nbVariables = length(colnames(table[,condVar]))
cat("	On ne garde que les axes d'inertie supérieure à ", paste(signif(100/nbVariables,digits=3)), "%\n")
nbAxes = sum(taux.pca$eig$'percentage of variance'>=(100/nbVariables)) 
if(nbAxes==1)
{
	nbAxes=2
	cat("		Un seul axe respecte ce critère, on garde aussi le suivant.\n")
}
cat("		On garde donc les ", nbAxes, " premiers axes.\n")
axesPrint = taux.pca$eig$`percentage of variance`[1:nbAxes]
cat("	Voici l'inertie expliquée des ", nbAxes," premiers axes (en %) :",axesPrint,"\n")

cat("\nEnregistrement des résultats dans un fichier:\n")
cat(	"Génération de plusieurs sou-échantillons pour tracer le graphe des individus...")
pdf("Resultats.pdf")
	for(i in 1:(nbAxes-1))
	{
		ech1 = sample(rownames(table),10)
		ech2 = sample(rownames(table),50)
		ech3 = sample(rownames(table),100)
		plot(taux.pca, axes = c(i,i+1), choix = "var", select = 1, title = "Graphe des variables")
		plot(taux.pca, axes = c(i,i+1), choix = "ind", select = ech1, title = "50 individus")
		plot(taux.pca, axes = c(i,i+1), choix = "ind", select = ech2, title = "10 individus")
		plot(taux.pca, axes = c(i,i+1), choix = "ind", select = ech3, title = "100 individus")
	}
dev.off()
cat("OK\n")





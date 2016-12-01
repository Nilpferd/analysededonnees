#Charger le fichier et les bibliothèques
setwd("~/Documents/[2014-2017] Mines ParisTech/[S5] Analyse de données/Mini-projet")
library(FactoMineR)

#Charger la base de données
cat("Chargement de la base de données...")
table = read.csv("base_cc_resume_20161013_COM.csv")
View(table)
cat("OK\n")

#Nettoyer la base de données des données manquantes
	#Enlever les communes dont on ne connaît pas la population
	conditionPOP = !(is.na(table[,"P13_POP"]))
	table <- table[conditionPOP,]
	#Combien de valeurs NA dans chaque colonnes
	conditionNA = vector()
	nombreNA = vector()
	for(i in colnames(table)){
		cat(i," : ",sum(is.na(table[,i])),"\n")
		conditionNA[i] = (sum(is.na(table[,i])) > 0)
		nombreNA[i] = sum(is.na(table[,i]))
	}
	#A partir de là, les seuls valeurs NA sont dans PIMP13 ou TP0613
	#Supprimer quelques communes avec des valeurs NA sauf les taux
	conditionSUPPR = nombreNA[conditionNA & !(names(conditionNA) %in% c("MED13","PIMP13","TP6013"))]
	for(i in names(conditionSUPPR)){
		condition = !(is.na(table[,i]))
		table <- table[condition,]
	}	
	#Condition 1 : tous les départements sauf l'outre mer
	conditionDEP = !(table[,"DEP"] %in% c("971","972","973","974"))
	#Condition 2 : sous-échantillonnage des communes
	nbEch = 100
	conditionSAMPLE = sample(nrow(table))
	conditionSAMPLE = conditionSAMPLE[1:nbEch]

#Ajouter une colonne pour les pondérations
table$WEIGHT <- table[,"P13_POP"]/sum(table[,"P13_POP"])
View(table)

#Analyse factorielle avec pondération
	#Variables testées
	conditionVAR1 = (colnames(table) %in% c("P13_POP",
											"SUPERF",
											"NAIS0813",
											"DECE0813",
											"P13_MEN",
											"P13_LOG",
											"P13_EMPLT",
											"ETTOT14"))
	#AF
	res.af = PCA(table[,conditionVAR1],
				 ncp = 5,
				 scale.unit = TRUE,
				 row.w = table$WEIGHT,
				 graph = TRUE
				)
	#Commentaires : fort gradient observé, les individus 4459 et 30807 tirent les axes.
	
#Analyse factorielle avec variables ramenées par habitants
	#On teste les mêmes variables
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
		table2[i,conditionNORM2] = table2[i,conditionNORM2]/table2[i,"P13_POP"]
		setTxtProgressBar(pb,i)
	}
	Sys.sleep(1)
	close(pb)
	write.table(table2, "table2.csv", row.names=TRUE, sep=";")
	#Cette table est longue à calculer, cette ligne de commande permet donc de la sauvergarder
	#pour repartir de là pour de prochaines études de la base de données
	View(table2)
	summary(table2)
	#La modification de la base de données pour ramener toutes les grandeurs à des grandeurs
	#par habitant prend un certain temps, une barre de progression permet de suivre
	#l'avancement des calculs.
	#Une fois les calculs faits, on peut faire des ACP sans avoir besoin de pondérations
	#mais sachant qu'on analyse maintenant uniquement des grandeurs rapportées au nombre
	#d'habitant. Néanmoins, l'effet de taille des communes devrait avoir disparu
	conditionVAR2 = (colnames(table2) %in% c("SUPERF",
											 "NAIS0813",
											 "DECE0813",
											 "P13_MEN",
											 "P13_LOG",
											 "P13_EMPLT",
											 "ETTOT14"))
	res.af2 = PCA(table2[,conditionVAR2],
				 ncp = 5,
				 scale.unit = TRUE,
				 row.w = table$WEIGHT,
				 graph = TRUE
				)
	#Les résultats obtenus sont plus intéressants mais il ne faut pas oublier qu'ils porte sur
	#des variables intensives (ramenées au nombre d'habitant) et donc qui ne sont plus des effectifs
	#On peut reprendre les deux commandes ci-dessus et choisir les variables sur lesquelles opérer une
	#ACP. Note au passage : ne pas intégrer la variable de normalisation P13_POP dans l'ACP. On essaie 
	#une ACP ci-dessous avec plus de variables:
	conditionVAR3 = (colnames(table2) %in% c("SUPERF",
											 "NAIS0813",
											 "DECE0813",
											 "P13_MEN",
											 "P13_LOG",
											 "P13_RP",
											 "P13_RSECOCC",
											 "P13_LOGVAC",
											 "NBMENFISC13",
											 "PIMP13",
											 "MED13",
											 "TP6013",
											 "P13_EMPLT",
											 "ETTOT14"))	
		res.af2 = PCA(table2[,conditionVAR3],
				 ncp = 5,
				 scale.unit = TRUE,
				 row.w = table$WEIGHT,
				 graph = TRUE
				)
	

#BIBLIOTHEQUES
setwd("~/Documents/[2014-2017] Mines ParisTech/[S5] Analyse de données/Mini-projet")
library(FactoMineR)

#-----------------------------------------------------------------------
#FONCTIONS
#-----------------------------------------------------------------------

#BASES DE DONNÉES
InitBDD <- function()
{
	cat("Chargement de la base de données initiale...")
	table <- read.csv("base_cc_resume_20161013_COM.csv")

		#Enlever les communes dont on ne connaît pas la population
		conditionPOP = !(is.na(table[,"P13_POP"]))
		table <- table[conditionPOP,]
		
		#Combien de valeurs NA dans chaque colonnes
		conditionNA = vector()
		nombreNA = vector()
		for(i in colnames(table)){
			conditionNA[i] = (sum(is.na(table[,i])) > 0)
			nombreNA[i] = sum(is.na(table[,i]))
		}

		#Supprimer quelques communes avec des valeurs NA sauf les taux
		conditionSUPPR = nombreNA[conditionNA & !(names(conditionNA) %in% c("MED13","PIMP13","TP6013"))]
		for(i in names(conditionSUPPR)){
			condition = !(is.na(table[,i]))
			table <- table[condition,]
		}	
		
		#Supprimer tous les départements sauf l'outre mer
		conditionDEP = !(table[,"DEP"] %in% c("971","972","973","974"))
		table <- table[conditionDEP,]
	cat("OK\n")
	return(table)
}
NormalizeBDD <- function(table)
{
	table2 <- data.frame()
	table2 = table
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
	write.table(table2, "table2.csv", row.names=TRUE, sep=",")
	return(table2)
}
LoadNormBDD <- function(table)
{
	cat("Chargement de la base de données...")
	table2 = read.csv("table2.csv")
	table2[,"P13_POP"] = table[,"P13_POP"]
	correction = which(table$SUPERF == 0)
	table[correction,"SUPERF"] = 1
	table2[,"DENSITY13"] = ceiling(table[,"P13_POP"]/table[,"SUPERF"])
	cat("OK\n")
	return(table2)
}
AddQualiVar <- function(table,table2)
{
	
	#CLASSEMENT PAR NOMBRE D'HABITANTS
	cat("\nClassement par nombre d'habitants:\n")
	table$POPLOG10 <- log10(table[,"P13_POP"])
	maxPOP = max(table[,"POPLOG10"])
	minPOP = min(table[,"POPLOG10"])
	nbClassesPOP = 5
	pas = (maxPOP-minPOP)/nbClassesPOP
	for(i in 1:nbClassesPOP)
	{
		conditionInf = (table[,"POPLOG10"] >= minPOP + (i-1)*pas)
		conditionSup = (table[,"POPLOG10"] < minPOP + i*pas)
		table2[conditionInf & conditionSup,"POP"] = paste("POP",i)
		cat("L'intervalle",i,": nombre d'habitants entre",floor(10^(minPOP + (i-1)*pas)),"et",floor(10^(minPOP + i*pas)),"habitants.\n")
	}
	table2[,"POP"]=factor(table2[,"POP"])

	#CLASSEMENT PAR DENSITÉ
	cat("\nClassement par densité:\n")
	table$DENSLOG10 <- log10(table2[,"DENSITY13"])
	maxDENS = max(table[,"DENSLOG10"])
	minDENS = min(table[,"DENSLOG10"])
	nbClassesDENS = 5
	pas = (maxDENS-minDENS)/nbClassesDENS
	for(i in 1:nbClassesDENS)
	{
		conditionInf = (table[,"DENSLOG10"] >= minDENS + (i-1)*pas)
		conditionSup = (table[,"DENSLOG10"] < minDENS + i*pas)
		table2[conditionInf & conditionSup,"DENS"] = paste("DENS",i)
		cat("L'intervalle",i,": densité entre",ceiling(10^(minDENS + (i-1)*pas)),"et",ceiling(10^(minDENS + i*pas)),"habitants par km carrés.\n")
	}
	table2[,"DENS"]=factor(table2[,"DENS"])

	#CLASSEMENT PAR MEDIANE DU NIVEAU DE VIE
	cat("\nClassement par médiane du niveau de vie:\n")
	maxMED = max(table[,"MED13"])
	minMED = min(table[,"MED13"])
	nbClassesMED = 5
	pas = (maxMED-minMED)/nbClassesMED
	for(i in 1:nbClassesMED)
	{
		conditionInf = (table[,"MED13"] >= minMED + (i-1)*pas)
		conditionSup = (table[,"MED13"] < minMED + i*pas)
		table2[conditionInf & conditionSup,"MED"] = paste("MED",i)
		cat("L'intervalle",i,": médiane de niveau entre",minMED + (i-1)*pas,"et",minMED + i*pas,"euros par unités de consommation.\n")
	}
	table2[,"MED"]=factor(table2[,"MED"])
	
	#CLASSEMENT PAR POPULATION AU CHOMAGE
	cat("\nClassement par nombre de chômeurs:\n")
	table[(table[,"P13_CHOM1564"] == 0),"P13_CHOM1564"] = 1
	maxCHOM = max(log10(table[,"P13_CHOM1564"]))
	minCHOM = min(log10(table[,"P13_CHOM1564"]))
	nbClassesCHOM = 5
	pas = (maxCHOM-minCHOM)/nbClassesCHOM
	for(i in 1:nbClassesCHOM)
	{
		conditionInf = (log10(table[,"P13_CHOM1564"]) >= minCHOM + (i-1)*pas)
		conditionSup = (log10(table[,"P13_CHOM1564"]) < minCHOM + i*pas)
		table2[conditionInf & conditionSup,"CHOM"] = paste("CHOM",i)
		cat("L'intervalle",i,": nombre de chômeurs entre",ceiling(10^(minCHOM + (i-1)*pas)),"et",ceiling(10^(minCHOM + i*pas)),"chômeurs.\n")
	}
	table2[,"CHOM"]=factor(table2[,"CHOM"])
	
	#CLASSEMENT PAR NOMBRE DE NAISSANCES
	cat("\nClassement par nombre de naissances:\n")
	table[(table[,"NAIS0813"] == 0),"NAIS0813"] = 1
	maxNAIS = max(log10(table[,"NAIS0813"]))
	minNAIS = min(log10(table[,"NAIS0813"]))
	nbClassesNAIS = 5
	pas = (maxNAIS-minNAIS)/nbClassesNAIS
	for(i in 1:nbClassesNAIS)
	{
		conditionInf = (log10(table[,"NAIS0813"]) >= minNAIS + (i-1)*pas)
		conditionSup = (log10(table[,"NAIS0813"]) < minNAIS + i*pas)
		table2[conditionInf & conditionSup,"NAIS"] = paste("NAIS",i)
		cat("L'intervalle",i,": nombre de naissances entre",ceiling(10^(minNAIS + (i-1)*pas)),"et",ceiling(10^(minNAIS + i*pas)),"naissances.\n")
	}
	table2[,"NAIS"]=factor(table2[,"NAIS"])
	
	#CLASSEMENT PAR NOMBRE DE DECES
	cat("\nClassement par nombre de décès:\n")
	table[(table[,"DECE0813"] == 0),"DECE0813"] = 1
	maxDECE = max(log10(table[,"DECE0813"]))
	minDECE = min(log10(table[,"DECE0813"]))
	nbClassesDECE = 5
	pas = (maxDECE-minDECE)/nbClassesDECE
	for(i in 1:nbClassesDECE)
	{
		conditionInf = (log10(table[,"DECE0813"]) >= minDECE + (i-1)*pas)
		conditionSup = (log10(table[,"DECE0813"]) < minDECE + i*pas)
		table2[conditionInf & conditionSup,"DECES"] = paste("DECES",i)
		cat("L'intervalle",i,": nombre de décès entre",ceiling(10^(minDECE + (i-1)*pas)),"et",ceiling(10^(minDECE + i*pas)),"décès.\n")
	}
	table2[,"DECES"]=factor(table2[,"DECES"])
	
	#CLASSEMENT PAR NOMBRE D'ÉTABLISSEMENTS ACTIFS
	cat("\nClassement par nombre d'établissements actifs:\n")
	table[(table[,"ETTOT14"] == 0),"ETTOT14"] = 1
	maxETAB = max(log10(table[,"ETTOT14"]))
	minETAB = min(log10(table[,"ETTOT14"]))
	nbClassesETAB = 5
	pas = (maxETAB-minETAB)/nbClassesETAB
	for(i in 1:nbClassesETAB)
	{
		conditionInf = (log10(table[,"ETTOT14"]) >= minETAB + (i-1)*pas)
		conditionSup = (log10(table[,"ETTOT14"]) < minETAB + i*pas)
		table2[conditionInf & conditionSup,"ETAB"] = paste("ETAB",i)
		cat("L'intervalle",i,": nombre d'établissements actifs entre",ceiling(10^(minETAB + (i-1)*pas)),"et",ceiling(10^(minETAB + i*pas)),"établissements.\n")
	}
	table2[,"ETAB"]=factor(table2[,"ETAB"])
	
	return(table2)
}

#VARIABLES ACTIVES
{s1.act <<- c("SUPERF",
		     "NAIS0813",
		     "DECE0813",
		     "P13_LOG",
		     "NBMENFISC13",
		     "PIMP13",
		     "MED13",
		     "TP6013",
		     "P13_EMPLT",
		     "ETTOT14")
		     
s2.act <<- c("P13_RP",
			 "P13_RSECOCC",
			 "P13_LOGVAC",
			 "P13_RP_PROP")
			 
s3.act <<- c("P13_EMPLT",
			 "P13_EMPLT_SAL",
			 "P13_POP1564",
			 "P13_CHOM1564",
			 "P13_ACT1564")
			 
s4.act <<- c("ETAZ14",
		     "ETBE14",
		     "ETFZ14",
		     "ETGU14",
		     "ETGZ14",
		     "ETOQ14",
		     "ETTEF114",
		     "ETTEFP1014")
		     
s5.act <<- c("PIMP13",
		     "MED13",
		     "TP6013",
		     "NBMENFISC13")	
}
		     			
#VARIABLES SUPPLEMENTAIRES
pop = c("POP")
dens = c("DENS")
med = c("MED")

s.tot = c("POP","MED","DENS","CHOM","NAIS","DECES","ETAB")
s1.supp = c("POP","MED","DENS","CHOM")
s2.supp = c("POP","MED","DENS","CHOM","ETAB")
s3.supp = c("POP","MED","DENS","CHOM","NAIS","DECES","ETAB")
s4.supp = c("POP","MED","DENS","CHOM","ETAB")
s5.supp = c("POP","DENS","ETAB")

#ANALYSE EN COMPOSANTES PRINCIPALES
ACP <- function(varact,varsup,table2)
{
	conditionVAR = (colnames(table2) %in% c(varact,varsup))
	suppl = which(colnames(table2[,conditionVAR]) %in% varsup)										 	
	res.af = PCA(table2[,conditionVAR],
					 ncp = 5,
					 quali.sup = suppl,
					 scale.unit = TRUE,
					 graph = FALSE
					)
	return(res.af)	
}
ACP.plot <- function(res,axes=c(1,2),choix="ind",titre,habillage="none")
{
	variables = c(rownames(res$var$coord),rownames(res$quali.sup$eta2))
	index = which(variables == habillage)
	if(length(index)==0){index="none"}
	plot.PCA(res,
			 axes = axes,
			 choix = choix,
			 invisible="ind",
			 title = titre,
			 habillage = index,
			 palette = palette(c("black","black")))
}
ACP.analyse <- function(res,titre)
{
	var.act = rownames(res$var$coord)
	nbAct = length(var.act)
	var.sup = rownames(res$quali.sup$eta2)
	nbSup = length(var.sup)
	
	cat("\nANALYSE DE L'ACP",titre,"\n")
	
	#Sélection des axes
	cat("Étape 1 - Sélection des axes :\n")
	cat("Seuil d'inertie :",100/nbAct,"%\n")
	axesRetenus = which(res$eig[2] >= 100/nbAct)
	cat("Axes retenus :",axesRetenus,"\n")
	
	#Nuage des variables : interprétation des axes
	cat("\nÉtape 2 - Quelles variables expliquent les axes :\n")
	cat("Seuil d'inertie expliquée :",100/nbAct,"%\n")
	cat("Seuil de cosinus carré :",1/nbAct,"%\n")
	for(i in axesRetenus)
	{
		variablesRetenues = (res$var$contrib[,i]>=100/nbAct & res$var$cos2[,i]>=1/nbAct)
		variablesRetenues = names(res$var$contrib[variablesRetenues,i])
		variablesRetenues = sort(res$var$coord[variablesRetenues,i],decreasing = FALSE)
		variablesRetenuesNEG = names(variablesRetenues[which(variablesRetenues < 0)])
		variablesRetenuesPOS = names(variablesRetenues[which(variablesRetenues > 0)])
		cat("Variables retenues sur l'axe",i,
				":\n	(-):",variablesRetenuesNEG,
				"\n	(+):",variablesRetenuesPOS,"\n")
	}
	cat("Cf. les graphes des variables affichés.\n")
	for(i in axesRetenus[1:(length(axesRetenus)-1)])
	{
		ACP.plot(res,axes=c(i,i+1),choix="var",titre=paste(titre,"Variables - Axes",i,"et",i+1))
	}
	
	#Nuage des individus : interprétation par modalités
	cat("\nÉtape 3 - Quelles classes individus contribuent le plus aux axes :\n")
	cat("Regarder les graphes directement.\n")
	for(i in axesRetenus[1:(length(axesRetenus)-1)])
	{
		ACP.plot(res,axes=c(i,i+1),choix="ind",titre=paste(titre,"Modalités - Axes",i,"et",i+1))
	}
}

#-----------------------------------------------------------------------
#ROUTINE PRINCIPALE
#-----------------------------------------------------------------------

#BASES DE DONNÉES
table = InitBDD()
table2 = LoadNormBDD(table)
sink("Résultats_de_l'ACP.txt")
table2 = AddQualiVar(table,table2)

#ANALYSES EN COMPOSANTES PRINCIPALES
recapitulatif = ACP(s1.act,s1.supp,table2)
logement = ACP(s2.act,s2.supp,table2)
emploi = ACP(s3.act,s3.supp,table2)
economie = ACP(s4.act,s4.supp,table2)
social = ACP(s5.act,s5.supp,table2)

#ANALYSE DES RÉSULTATS
pdf("Résultats_de_l'ACP.pdf")
	ACP.analyse(recapitulatif,"Récapitulatif")
	ACP.analyse(logement,"Logement")
	ACP.analyse(emploi,"Emploi")
	ACP.analyse(economie,"Économie")
	ACP.analyse(social,"Social")
dev.off()
sink()


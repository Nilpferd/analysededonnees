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
	
	cat("\n\n")
	#CLASSEMENT PAR NOMBRE D'HABITANTS
	table$POPLOG10 <- log10(table[,"P13_POP"])
	maxPOP = max(table[,"POPLOG10"])
	minPOP = min(table[,"POPLOG10"])
	nbClassesPOP = 10
	pas = (maxPOP-minPOP)/nbClassesPOP
	for(i in 1:nbClassesPOP)
	{
		conditionInf = (table[,"POPLOG10"] >= minPOP + (i-1)*pas)
		conditionSup = (table[,"POPLOG10"] < minPOP + i*pas)
		table2[conditionInf & conditionSup,"POP"] = paste("POP",i)
		cat("L'intervalle",i,"regroupe les villes avec un minimum de",floor(10^(minPOP + (i-1)*pas)),"habitants et un maximum de",floor(10^(minPOP + i*pas)),"habitants.\n")
	}
	table2[,"POP"]=factor(table2[,"POP"])
	cat("\n\n")

	#CLASSEMENT PAR DENSITÉ
	table$DENSLOG10 <- log10(table2[,"DENSITY13"])
	maxDENS = max(table[,"DENSLOG10"])
	minDENS = min(table[,"DENSLOG10"])
	nbClassesDENS = 10
	pas = (maxDENS-minDENS)/nbClassesDENS
	for(i in 1:nbClassesDENS)
	{
		conditionInf = (table[,"DENSLOG10"] >= minDENS + (i-1)*pas)
		conditionSup = (table[,"DENSLOG10"] < minDENS + i*pas)
		table2[conditionInf & conditionSup,"DENS"] = paste("DENS",i)
		cat("L'intervalle",i,"regroupe les villes avec une densité comprise entre",ceiling(10^(minDENS + (i-1)*pas)),"et",ceiling(10^(minDENS + i*pas)),"habitants par km carrés.\n")
	}
	table2[,"DENS"]=factor(table2[,"DENS"])
	cat("\n\n")

	#CLASSEMENT PAR MEDIANE DU NIVEAU DE VIE
	maxMED = max(table[,"MED13"])
	minMED = min(table[,"MED13"])
	nbClassesMED = 10
	pas = (maxMED-minMED)/nbClassesMED
	for(i in 1:nbClassesMED)
	{
		conditionInf = (table[,"MED13"] >= minMED + (i-1)*pas)
		conditionSup = (table[,"MED13"] < minMED + i*pas)
		table2[conditionInf & conditionSup,"MED"] = paste("MED",i)
		cat("L'intervalle",i,"regroupe les villes avec une médiane de niveau comprise entre",minMED + (i-1)*pas,"et",minDENS + i*pas,"euros par unités de consommation.\n")
	}
	table2[,"MED"]=factor(table2[,"MED"])
	cat("\n\n\n")
	return(table2)
}

#VARIABLES SUPPLEMENTAIRES
pop = c("POP")
dens = c("DENS")
med = c("MED")

#ANALYSE EN COMPOSANTES PRINCIPALES
PCAcompute <- function(varact,varsup,table2)
{
	conditionVAR = (colnames(table2) %in% cbind(varact,varsup))
	suppl = which(colnames(table2[,conditionVAR]) == varsup)										 	
	res.af = PCA(table2[,conditionVAR],
					 ncp = 5,
					 quali.sup = suppl,
					 scale.unit = TRUE,
					 graph = FALSE
					)
	return(res.af)	
}

PCAplot <- function(res,varact,varsup,axesplot,titre)
{
conditionVAR = (colnames(table2) %in% cbind(varact,varsup))
suppl = which(colnames(table2[,conditionVAR]) == varsup)

concat = cbind.data.frame(table2[,varsup],res$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res,
		 axes = axesplot,
		 choix = "ind",
		 ellipse=ellipse.coord,
		 habillage = suppl,
		 #invisible="ind",
		 title = titre,
		 label="none",
		 select = sample(1:nrow(table2),2000),
		 unselect = 1)
plot.PCA(res,
		 axes = axesplot,
		 choix = "ind",
		 ellipse=ellipse.coord,
		 habillage = suppl,
		 invisible="ind",
		 title = titre)		
}

#ANALYSES EN COMPOSANTES PRINCIPALES
Recapitulatif <- function(table2,supp)
{
	cat("AFC1 - Récapitulatif...")
	s1.act <<- c("SUPERF",
			   "NAIS0813",
			   "DECE0813",
			   "P13_LOG",
			   "NBMENFISC13",
			   "PIMP13",
			   "MED13",
			   "TP6013",
			   "P13_EMPLT",
			   "ETTOT14")
	s1 = PCAcompute(s1.act,supp,table2)
	cat("OK\n")
	return(s1)
}
Logement <- function(table2,supp)
{
	cat("AFC5 - Variables de logement...")
	s2.act <<- c("P13_RP",
				"P13_RSECOCC",
				"P13_LOGVAC",
				"P13_RP_PROP")	
	s2 = PCAcompute(s2.act,supp,table2)
	cat("OK\n")
	return(s2)
}
Emploi <- function(table2,supp)
{
	cat("AFC3 - Variables d'emploi...")
	s3.act <<- c("P13_EMPLT",
				"P13_EMPLT_SAL",
				"P13_POP1564",
				"P13_CHOM1564",
				"P13_ACT1564")	
	s3 = PCAcompute(s3.act,supp,table2)
	cat("OK\n")
	return(s3)
}
Economie <- function(table2,supp)
{
	cat("AFC4 - Variables économiques...")
	s4.act <<- c("ETAZ14",
			   "ETBE14",
			   "ETFZ14",
			   "ETGU14",
			   "ETGZ14",
			   "ETOQ14",
			   "ETTEF114",
			   "ETTEFP1014")	
	s4 = PCAcompute(s4.act,supp,table2)
	cat("OK\n")
	return(s4)
}
Social <- function(table2,supp)
{
	cat("AFC5 - Variables sociales...")
	s5.act <<- c("PIMP13",
			   "MED13",
			   "TP6013",
			   "NBMENFISC13")	
	s5 = PCAcompute(s5.act,supp,table2)
	cat("OK\n")
	return(s5)
}

#-----------------------------------------------------------------------
#ROUTINE PRINCIPALE
#-----------------------------------------------------------------------

#BASES DE DONNÉES
table = InitBDD()
table2 = LoadNormBDD(table)
table2 = AddQualiVar(table,table2)

#ANALYSES EN COMPOSANTES PRINCIPALES
{recapPOP = Recapitulatif(table2,pop)
recapDENS = Recapitulatif(table2,dens)
recapMED = Recapitulatif(table2,med)

logementPOP = Logement(table2,pop)
logementDENS = Logement(table2,dens)
logementMED = Logement(table2,med)

emploiPOP = Emploi(table2,pop)
emploiDENS = Emploi(table2,dens)
emploiMED = Emploi(table2,med)

economiePOP = Economie(table2,pop)
economieDENS = Economie(table2,dens)
economieMED = Economie(table2,med)

socialPOP = Social(table2,pop)
socialDENS = Social(table2,dens)
socialMED = Social(table2,med)}

#ANALYSE DES RÉSULTATS
{pdf("ACP_Results.pdf")
	
	#Récapitulatif
	cat("\n\nPDF Récapitulatif...")
	plot.PCA(recapPOP,choix="var",axes=c(1,2),title="Récapitulatif Axes 1-2")
	plot.PCA(recapPOP,choix="var",axes=c(2,3),title="Récapitulatif Axes 2-3")
	plot.PCA(recapPOP,choix="var",axes=c(3,4),title="Récapitulatif Axes 3-4")
		#POP
		PCAplot(recapPOP,s1.act,"POP",c(1,2),"Récapitulatif Population Axes 1-2")
		PCAplot(recapPOP,s1.act,"POP",c(2,3),"Récapitulatif Population Axes 2-3")
		PCAplot(recapPOP,s1.act,"POP",c(3,4),"Récapitulatif Population Axes 3-4")
		#DENS
		PCAplot(recapDENS,s1.act,"DENS",c(1,2),"Récapitulatif Densité Axes 1-2")
		PCAplot(recapDENS,s1.act,"DENS",c(2,3),"Récapitulatif Densité Axes 2-3")
		PCAplot(recapDENS,s1.act,"DENS",c(3,4),"Récapitulatif Densité Axes 3-4")
		#MED
		PCAplot(recapMED,s1.act,"MED",c(1,2),"Récapitulatif Médiane Axes 1-2")
		PCAplot(recapMED,s1.act,"MED",c(2,3),"Récapitulatif Médiane Axes 2-3")
		PCAplot(recapMED,s1.act,"MED",c(3,4),"Récapitulatif Médiane Axes 3-4")
	cat("OK\n")

	#Logement
	cat("PDF Logement...")
	plot.PCA(logementPOP,choix="var",axes=c(1,2),title="Logement Axes 1-2")
	plot.PCA(logementPOP,choix="var",axes=c(2,3),title="Logement Axes 2-3")
	plot.PCA(logementPOP,choix="var",axes=c(3,4),title="Logement Axes 3-4")
		#POP
		PCAplot(logementPOP,s2.act,"POP",c(1,2),"Logement Population Axes 1-2")
		PCAplot(logementPOP,s2.act,"POP",c(2,3),"Logement Population Axes 2-3")
		PCAplot(logementPOP,s2.act,"POP",c(3,4),"Logement Population Axes 3-4")
		#DENS
		PCAplot(logementDENS,s2.act,"DENS",c(1,2),"Logement Densité Axes 1-2")
		PCAplot(logementDENS,s2.act,"DENS",c(2,3),"Logement Densité Axes 2-3")
		PCAplot(logementDENS,s2.act,"DENS",c(3,4),"Logement Densité Axes 3-4")
		#MED
		PCAplot(logementMED,s2.act,"MED",c(1,2),"Logement Médiane Axes 1-2")
		PCAplot(logementMED,s2.act,"MED",c(2,3),"Logement Médiane Axes 2-3")
		PCAplot(logementMED,s2.act,"MED",c(3,4),"Logement Médiane Axes 3-4")
	cat("OK\n")
			
	#Emploi
	cat("PDF Emploi...")
	plot.PCA(emploiPOP,choix="var",axes=c(1,2),title="Emploi Axes 1-2")
	plot.PCA(emploiPOP,choix="var",axes=c(2,3),title="Emploi Axes 2-3")
	plot.PCA(emploiPOP,choix="var",axes=c(3,4),title="Emploi Axes 3-4")
		#POP
		PCAplot(emploiPOP,s3.act,"POP",c(1,2),"Emploi Population Axes 1-2")
		PCAplot(emploiPOP,s3.act,"POP",c(2,3),"Emploi Population Axes 2-3")
		PCAplot(emploiPOP,s3.act,"POP",c(3,4),"Emploi Population Axes 3-4")
		#DENS
		PCAplot(emploiDENS,s3.act,"DENS",c(1,2),"Emploi Densité Axes 1-2")
		PCAplot(emploiDENS,s3.act,"DENS",c(2,3),"Emploi Densité Axes 2-3")
		PCAplot(emploiDENS,s3.act,"DENS",c(3,4),"Emploi Densité Axes 3-4")
		#MED
		PCAplot(emploiMED,s3.act,"MED",c(1,2),"Emploi Médiane Axes 1-2")
		PCAplot(emploiMED,s3.act,"MED",c(2,3),"Emploi Médiane Axes 2-3")
		PCAplot(emploiMED,s3.act,"MED",c(3,4),"Emploi Médiane Axes 3-4")
	cat("OK\n")
		
	#Économie
	cat("PDF Économie...")
	plot.PCA(economiePOP,choix="var",axes=c(1,2),title="Economie Axes 1-2")
	plot.PCA(economiePOP,choix="var",axes=c(2,3),title="Economie Axes 2-3")
	plot.PCA(economiePOP,choix="var",axes=c(3,4),title="Economie Axes 3-4")
		#POP
		PCAplot(economiePOP,s4.act,"POP",c(1,2),"Economie Population Axes 1-2")
		PCAplot(economiePOP,s4.act,"POP",c(2,3),"Economie Population Axes 2-3")
		PCAplot(economiePOP,s4.act,"POP",c(3,4),"Economie Population Axes 3-4")
		#DENS
		PCAplot(economieDENS,s4.act,"DENS",c(1,2),"Economie Densité Axes 1-2")
		PCAplot(economieDENS,s4.act,"DENS",c(2,3),"Economie Densité Axes 2-3")
		PCAplot(economieDENS,s4.act,"DENS",c(3,4),"Economie Densité Axes 3-4")
		#MED
		PCAplot(economieMED,s4.act,"MED",c(1,2),"Economie Médiane Axes 1-2")
		PCAplot(economieMED,s4.act,"MED",c(2,3),"Economie Médiane Axes 2-3")
		PCAplot(economieMED,s4.act,"MED",c(3,4),"Economie Médiane Axes 3-4")
	cat("OK\n")
		
	#Social
	cat("PDF Social...")
	plot.PCA(socialPOP,choix="var",axes=c(1,2),title="Social Axes 1-2")
	plot.PCA(socialPOP,choix="var",axes=c(2,3),title="Social Axes 2-3")
	plot.PCA(socialPOP,choix="var",axes=c(3,4),title="Social Axes 3-4")
		#POP
		PCAplot(socialPOP,s5.act,"POP",c(1,2),"Social Population Axes 1-2")
		PCAplot(socialPOP,s5.act,"POP",c(2,3),"Social Population Axes 2-3")
		PCAplot(socialPOP,s5.act,"POP",c(3,4),"Social Population Axes 3-4")
		#DENS
		PCAplot(socialDENS,s5.act,"DENS",c(1,2),"Social Densité Axes 1-2")
		PCAplot(socialDENS,s5.act,"DENS",c(2,3),"Social Densité Axes 2-3")
		PCAplot(socialDENS,s5.act,"DENS",c(3,4),"Social Densité Axes 3-4")
		#MED
		PCAplot(socialMED,s5.act,"MED",c(1,2),"Social Médiane Axes 1-2")
		PCAplot(socialMED,s5.act,"MED",c(2,3),"Social Médiane Axes 2-3")
		PCAplot(socialMED,s5.act,"MED",c(3,4),"Social Médiane Axes 3-4")
	cat("OK\n")
dev.off()}

#PLOTTER LE NUAGE AVEC LES MODALITÉS
#plot.PCA(recapPOP,
#		 choix="ind",
#		 axes=c(1,2),
#		 title="Récapitulatif Population Axes 1-2",
#		 label="none",
#		 habillage = 11,
#		 select = sample(1:nrow(table2),100),
#		 unselect = 1)

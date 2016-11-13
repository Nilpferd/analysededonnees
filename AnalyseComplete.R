#Charger le repertoire
setwd("~/Documents/[2014-2017] Mines ParisTech/[S5] Analyse de données/Mini-projet")

#Charger la base de donnees
cat("Chargement de la base de données...")
table = read.csv("base_cc_resume_20161013_COM.csv")
cat(paste("OK\n\n"))

#Enlever des colonnes inutiles ou non pertinentes
cat("Suppression des variables non pertinentes...")								
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO"))]
conditionL = !(table[,"DEP"] %in% c("971","972","973","974"))
table <- table[conditionL,]																													
rm(conditionL)
cat(paste("OK\n\n"))

#Remplacer les valeurs non attribuées par les moyennes en colonnes
cat("Remplacement des valeurs non attribuées par les moyennes en colonnes...")
conditionC = colnames(table)[!(colnames(table) %in% c("REG","DEP"))]
mean = vector()
for(i in conditionC)
{
	conditionL = is.na(table[,i])
	mean[i] = sum(table[!conditionL,i])/sum(!conditionL)
	table[conditionL,i] = mean[i]
}
rm(conditionL,conditionC,mean)
cat(paste("OK\n\n"))

#Afficher la table
View(table)

#Faire une ACP sur l'ensemble
cat("Analyse en composante principales...")
library(FactoMineR)
variables = c("ETAZ14",
							"ETBE14",
							"ETFZ14",
							"ETGU14",
							"ETGZ14",
							"ETOQ14",
							"ETTEF114",
							"ETTEFP1014")
nbVariables = length(variables)
variablesPCA = (colnames(table) %in% variables)
citiesPCA = !(table[,"DEP"] %in% c("75"))
variablesSupPCA = which(colnames(table) == c())
citiesSupPCA = which(table[,"DEP"] == c("75"))
ncp = 10
res.pca = PCA(table[citiesPCA,variablesPCA],
							scale.unit=TRUE,
							ncp,
							quanti.sup=NULL,
							quali.sup=NULL,
							ind.sup=citiesSupPCA,
							graph=F
							)
cat(paste("OK\n"))
cat("L'analyse porte sur les variables : ",variables,"\n\n")
rm(variables,variablesPCA,citiesPCA,variablesSupPCA,citiesSupPCA)

#Organiser les données
cat("Organisation des données :\n")
cat("On ne garde que les axes d'inertie supérieure à ",paste(100/nbVariables),"%.\n")
nbAxes = sum(res.pca$eig$'percentage of variance'>=(100/nbVariables))

if(nbAxes==1)
{
	nbAxes=2
	cat("Un seul axe respecte ce critère, on garde aussi le suivant.\n")
}

cat("On garde donc les ", nbAxes, " premiers axes.\n")
cat("Voici l'inertie expliquée des ", nbAxes," premiers axes (en %) :\n")
print(res.pca$eig$`percentage of variance`[1:nbAxes])

axes = list()
for(i in 1:nbAxes)
{
	cat("\nDonnées sur l'axe ",i,"\n")
	columns = c(res.pca$var$coord[,i],res.pca$var$contrib[,i],res.pca$var$cos2[,i])
	axe = matrix(data = columns,nrow = dim(res.pca$var$coord)[1],ncol = 3)
	rownames(axe) = rownames(res.pca$var$coord)
	colnames(axe) = c("Coord","Contr","Cos2")
	axes[[i]] = axe
	print(axes[[i]])
}
View(axes)
write.table(data.frame(axes),file="AnalyseComplete.txt",sep=";")
rm(i,ncp,columns)
cat("\nOrganisation des données terminées.\n\n")

#Afficher les graphiques
cat("Afficher les graphiques...")
pdf("PlotsComplete.pdf")
	for(i in 1:(nbAxes-1))
	{
		plot(res.pca, axes = c(i,i+1), choix = "var")
		#plot(res.pca, axes = c(i,i+1), choix = "ind")
	}
dev.off()
cat("OK\n")
rm(i,nbAxes,nbVariables,axe)

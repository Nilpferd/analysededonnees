#Load the directory
setwd("~/Documents/[2014-2017] Mines ParisTech/[S5] Analyse de données/Mini-projet")	#to be modified by the user

#Load and create the dataframes
table = read.csv("base_cc_resume_20161013_COM.csv")									#load the dateframe
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]	#delete unrelevant columns

View(table)																													#display the dataframe

#Create and dimension department table
department = data.frame()																			#department dataframe
listdepartment = levels(table[,colnames(table) == "DEP"])			#department levels

nb_departments = length(listdepartment)								#number of departments
department[1:nb_departments,1] <- NA 									#rows
rownames(department) <- listdepartment								#add row names for departments
rm(nb_departments)

nb_variables = length(colnames(table))-1												#number of variables
department[,1:nb_variables] <- NA 															#columns (removing "DEP")
colnames(department) <- colnames(table)[2:(nb_variables+1)]			#add header to dataframe
rm(nb_variables)
rm(listdepartment)

View(department)

#Fill in the department table
variables = colnames(table)[2:length(colnames(table))]				#vector of variables, except "DEP"

for(i in rownames(department))
{
	conditionL = (table[,"DEP"] == i)														#condition to select only the current department's lines
	for(j in colnames(department)){
		department[i,j] = sum(na.omit(table[conditionL,j]))				#sum of all the extensive variables
	}
	department[i,c("PIMP13","MED13","TP6013")] = c(NA,NA,NA)

	ponderation = table[conditionL,"P13_POP"]/sum(na.omit(table[conditionL,"P13_POP"]))

	meanPIMP13 = ponderation * table[conditionL,"PIMP13"]
	department[i,"PIMP13"] = sum(na.omit(meanPIMP13))				#population-weighted mean for "PIMP13"

	meanMED13 = ponderation * table[conditionL,"MED13"]
	department[i,"MED13"] = sum(na.omit(meanMED13))					#population-weighted mean for "MED13"

	meanTP6013 = ponderation * table[conditionL,"TP6013"]
	department[i,"TP6013"] = sum(na.omit(meanTP6013))				#population-weighted mean for "TP6013"

}
rm(conditionL, i, j, meanMED13, meanPIMP13, meanTP6013, ponderation, variables)
colnames(department)[3] <- "SUPERF_TOT"										#rename the "SUPERF" column more appropriately

#Add new variables to the department dataframe
department$SUPERF_MOY <- NA																#adds a column for average superficy in department
department$DENSITY13 <- NA																#adds a column for avery population density
department$POP0813 <- NA																	#adds a columns for the population growth
for(i in rownames(department))
{
	conditionL = (table[,"DEP"] == i)
	department[i,"SUPERF_MOY"] = mean(na.omit(table[conditionL,"SUPERF"]))
	department[i,"DENSITY13"] = department[i,"P13_POP"]/department[i,"SUPERF_TOT"]
	department[i,"POP0813"] = department[i,"P13_POP"] - department[i,"P08_POP"]
}
rm(conditionL)

#Principal Component Analysis
#Selection of actives individuals and variables, others will only be illustrative
#Drop variables which are linear combination of others not to diminish the rank of the inertia matrix
library(FactoMineR)
variablesPCA = !(colnames(department) %in% c("ETTOT14","P08_POP","P13_LOG","P08_EMPLT"))
departmentsPCA = !(rownames(department) %in% c("971","972","973","974","75"))
variablesSupPCA = which(colnames(department) == c())
departmentsSupPCA = which(rownames(department) == c("75"))
ncp = 5
res.pca = PCA(department[departmentsPCA,variablesPCA],
							scale.unit=TRUE,
							ncp,
							quanti.sup=NULL,
							quali.sup=NULL,
							ind.sup=departmentsSupPCA,
							graph=F
							)
rm(departmentsPCA,variablesPCA)
write.table(res.pca$eig,file="output_eigen.csv",sep=";")

#Organization of the results in a list of matrixes
axes = list()
for(i in 1:ncp)
{
	columns = c(res.pca$var$coord[,i],res.pca$var$contrib[,i],res.pca$var$cos2[,i])
	axe = matrix(data = columns,nrow = dim(res.pca$var$coord)[1],ncol = 3)
	rownames(axe) = rownames(res.pca$var$coord)
	colnames(axe) = c("Coord","Contr","Cos2")
	axes[[i]] = axe
}
View(axes)
write.table(data.frame(axes),file="output_axes.csv",sep=";")

#Plot of the factor and individual maps
pdf("Plots.pdf")
	plot(res.pca, axes = c(1,2), choix = "ind")
	plot(res.pca, axes = c(1,2), choix = "var")
	plot(res.pca, axes = c(2,3), choix = "ind")
	plot(res.pca, axes = c(2,3), choix = "var")
	plot(res.pca, axes = c(3,4), choix = "ind")
	plot(res.pca, axes = c(3,4), choix = "var")
dev.off()

#Command to execute : source(file = "Departements3.R")
#Useful commands to study the results
#res.pca$ind$coord[conditionL,"Dim.1"]
#names(res.pca$ind$coord[conditionL,"Dim.1"])


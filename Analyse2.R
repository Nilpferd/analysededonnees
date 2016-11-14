#Je me suis permis de reprendre ton code Rodolphe, merci pour le coup de main ! Adrien

#Load the directory
#setwd("~/Documents/2016-2017/Mines/Cours/Analyse_donnees/Mini-projet/Statistiques_geographiques_Insee/scripts")	#to be modified by the user

#Load and create the dataframes
table = read.csv("../donnees/base_cc_resume_20161013_COM.csv")		#load the dateframe
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]	#delete unrelevant columns

View(table)	#display the dataframe

#Create and dimension department table
department = data.frame()											#department dataframe
listdepartment = levels(table[,colnames(table) == "DEP"])			#department levels

nb_departments = length(listdepartment)								#number of departments
department[1:nb_departments,1] <- NA 								#rows
rownames(department) <- listdepartment								#add row names for departments
rm(nb_departments)

nb_variables = length(colnames(table))-1							#number of variables
department[,1:nb_variables] <- NA 									#columns (removing "DEP")
colnames(department) <- colnames(table)[2:(nb_variables+1)]			#add header to dataframe
rm(nb_variables)
rm(listdepartment)

View(department)

#Fill in the department table
variables = colnames(table)[2:length(colnames(table))]				#vector of variables, except "DEP"

for(i in rownames(department))
{
	conditionL = (table[,"DEP"] == i)								#condition to select only the current department's lines
	for(j in colnames(department)){
		department[i,j] = sum(na.omit(as.numeric(table[conditionL,j])))			#sum of all the extensive variables
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
department$POP0813 <- NA																#adds a columns for the population growth
for(i in rownames(department))
{
	conditionL = (table[,"DEP"] == i)
	department[i,"SUPERF_MOY"] = mean(na.omit(table[conditionL,"SUPERF"]))
	department[i,"DENSITY13"] = department[i,"P13_POP"]/department[i,"SUPERF_TOT"]
	department[i,"POP0813"] = department[i,"P13_POP"] - department[i,"P08_POP"]
}
rm(conditionL)

#Multiple correspondence analysis
#Converts quantitative variables to qualitative variables by introducing 3 classes for each variable
#Data is stored in a new dataframe : department_quali

department <- department[,(colnames(department) %in% c("P13_POP","DENSITY13","P13_MEN","P13_LOG","PIMP13","MED13","TP6013","P13_CHOM1564"))] #keep only relevant columns for MCA (if not, 111 columns, a bit too much to analyse!)

department_quali = data.frame()										#department_quali dataframe for MCA
listdepartment = levels(table[,colnames(table) == "DEP"])			#department levels

nb_departments = length(listdepartment)								#number of departments
department_quali[1:nb_departments,1] <- FALSE 						#rows
rownames(department_quali) <- listdepartment						#add row names for departments
rm(nb_departments)

nb_variables = length(colnames(department))*3						#number of variables (3 classes per variable)
department_quali[,1:nb_variables] <- FALSE 							#columns (removing "DEP")

for(j in c(1:length(colnames(department))))							
{
	colnames(department_quali)[3*(j-1)+1] <- paste(colnames(department)[j],"_1st_third")
	colnames(department_quali)[3*(j-1)+2] <- paste(colnames(department)[j],"_2nd_third")
	colnames(department_quali)[3*j] <- paste(colnames(department)[j],"_3rd_third")
	thirds=quantile(department[[j]],c(.33,.66),na.rm=TRUE)			#This will remove Na and NaN.
	for(i in rownames(department)){
		department_quali[i,3*(j-1)+1]<-as.logical(department[i,j]<thirds[[1]])
		department_quali[i,3*(j-1)+2]<-as.logical((department[i,j]>=thirds[[1]])&(department[i,j]<thirds[[2]]))
		department_quali[i,3*j]<-as.logical(department[i,j]>=thirds[[2]])
	}
}

View(department_quali)

#MCA
library(FactoMineR)
res.mca = MCA(department_quali,ncp=4)
plot.MCA(res.mca)

#100/24=4.16667% donc je conserve 4 dimensions
#J'analyse 101 individus selon 24 variables.

#write.csv(res.mca$var$contrib,file="../resultats/Analyse2_ACM_contrib.csv")

#Encore une fois, je me permets de reprendre ton code Rodolphe
ncp = 4

axes = list()
for(i in 1:ncp)
{
	columns = c(res.mca$var$coord[,i],res.mca$var$contrib[,i],res.mca$var$cos2[,i])
	axe = matrix(data = columns,nrow = dim(res.mca$var$coord)[1],ncol = 3)
	rownames(axe) = rownames(res.mca$var$coord)
	colnames(axe) = c("Coord","Contr","Cos2")
	axes[[i]] = axe
}
View(axes)
write.table(data.frame(axes),file="../resultats/Analyse2_ACM_axes.csv",sep=";")


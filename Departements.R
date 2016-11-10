#Load the directory
setwd("~/Documents/[2014-2017] Mines ParisTech/[S5] Analyse de données/Mini-projet")	#to be modified by the user

#Load and create the dataframes
table = read.csv("base_cc_resume_20161013_COM.csv")					#load the dateframe
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]	#delete unrelevant columns

View(table)															#display the dataframe

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
		department[i,j] = sum(na.omit(table[conditionL,j]))			#sum of all the extensive variables
	}
	department[i,c("PIMP13","MED13","TP6013")] = c(NA,NA,NA)

	ponderation = population = table[conditionL,"P13_POP"]/sum(na.omit(table[conditionL,"P13_POP"]))

	meanPIMP13 = population * table[conditionL,"PIMP13"]
	department[i,"PIMP13"] = sum(na.omit(meanPIMP13))				#population-weighted mean for "PIMP13"

	meanMED13 = population * table[conditionL,"MED13"]
	department[i,"MED13"] = sum(na.omit(meanMED13))					#population-weighted mean for "MED13"

	meanTP6013 = population * table[conditionL,"TP6013"]
	department[i,"TP6013"] = sum(na.omit(meanTP6013))				#population-weighted mean for "TP6013"

}
rm(conditionL, i, j, meanMED13, meanPIMP13, meanTP6013, ponderation, population, variables)




	


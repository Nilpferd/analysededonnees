#Adrien 24/11/16
#
#
#Il faut bien que les sommes en ligne et les sommes en colonne aient un sens.
#
#AFC établissements de moins de 9 salariés contre nombre d'établissements agricoles (ETTEF114 et ETAZ14)
#

#Load the directory
#setwd("~/Documents/2016-2017/Mines/Cours/Analyse_donnees/Mini-projet/Statistiques_geographiques_Insee/scripts")	#to be modified by the user

#Load and create the dataframes
table = read.csv("../donnees/base_cc_resume_20161013_COM.csv")		#load the dateframe
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]	#delete unrelevant columns

#tableau_contingence_chom_logt_vacants=as.data.frame(table(table$ETTEF114, table$ETAZ14)) #tableau de contingence

library(ade4)
plot(table$ETTEF114,main="Etablissements de moins de 9 salariés en France en 2014",col="green") #histogramme brut x : numéro commune, y : nombre chômeurs

quantile(table$ETTEF114,c(.33,.66),na.rm=TRUE)
#33% 66% 
# 5  13 

quantile(table$ETTEF114,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,0.99),na.rm=TRUE) 
#   10%    20%    30%    40%    50%    60%    70%    80%    90%    95%    99% 
#  2.00   3.00   4.00   6.00   8.00  10.00  15.00  24.00  57.00 118.00 491.45 
    
#Conclusion : je vais faire les classes suivantes pour les établissements de moins de 9 salariés :
#[0,5[,[5,13[,[13,24[,[24,57[,[57,118[,[118,+oo[
#etb9_0_5,etb9_5_13, etb9_13_24,etb9_24_57,etb9_57_118,etb9_118_plus

#Même travail à mener sur les établissements agricoles.

plot(table$ETAZ14,main="Etablissements agricoles par commune en France en 2014",col="red")

quantile(table$ETAZ14,c(.33,.66),na.rm=TRUE)
# 33% 66% 
# 5  11 
quantile(table$ETAZ14,c(.25,.5,0.75),na.rm=TRUE)
# 25% 50% 75% 
# 4   7  14
quantile(table$ETAZ14,c(.1,.2,.3,.4,.5,.6,.7,.8,.9),na.rm=TRUE)
# 10% 20% 30% 40% 50% 60% 70% 80% 90% 
# 2   3   4   6   7   9  12  16  24
quantile(table$ETAZ14,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99),na.rm=TRUE)
#10% 20% 30% 40% 50% 60% 70% 80% 90% 95% 99% 
#2   3   4   6   7   9  12  16  24  33  66
  
#Conclusion : je vais faire les classes suivantes pour les établissements agricoles :
#[0,5[,[5,11[, [11,16[, [16,24[, [24,33[, [33,66[, [66,+oo[
#etb_agri_0_5,etb_agri_5_11,etb_agri_11_16,etb_agri_16_24,etb_agri_24_33,etb_agri_33_66 et etb_agri_66_plus

#Mise en forme des données
donnees_classees = data.frame()	
nb_communes=length(table$ETTEF114)
avoidNA1=complete.cases(table$ETTEF114) 
avoidNA2=complete.cases(table$ETAZ14) 

donnees_AFC=matrix(data=0,nrow=6,ncol=7)
rownames(donnees_AFC)<-c("etb9_0_5","etb9_5_13", "etb9_13_24","etb9_24_57","etb9_57_118","etb9_118_plus")
colnames(donnees_AFC)<-c("etb_agri_0_5","etb_agri_5_11","etb_agri_11_16","etb_agri_16_24","etb_agri_24_33","etb_agri_33_66","etb_agri_66_plus")

indices<-function(i){
	k=0
	p=0
	if(avoidNA2[i]&avoidNA1[i]){				
		if (table$ETTEF114[i]<5){
			k=1
		} else if(table$ETTEF114[i]<13){
			k=2
		}else if(table$ETTEF114[i]<24){
			k=3
		}else if(table$ETTEF114[i]<57){
			k=4
		}else if(table$ETTEF114[i]<118){
			k=5
		}else {
			k=6
		}
	}
	if(avoidNA2[i]&avoidNA1[i]){
						
		if (table$ETAZ14[i]<5){
			p=1#"etb_agri_0_5"
		} else if(table$ETAZ14[i]<11){
			p=2#"etb_agri_5_11"
		}else if(table$ETAZ14[i]<16){
			p=3#"etb_agri_11_16"
		}else if(table$ETAZ14[i]<24){
			p=4#"etb_agri_16_24"
		}else if(table$ETAZ14[i]<33){
			p=5#"etb_agri_24_33"
		}else if(table$ETAZ14[i]<66){
			p=6#"etb_agri_33_66"
		}else {
			p=7#"etb_agri_66_plus"
		}
	}

	return(c(k,p))
}

for (i in c(1:nb_communes)){
	if(avoidNA2[i]&avoidNA1[i]){				
		donnees_AFC[indices(i)[1],indices(i)[2]]<-donnees_AFC[indices(i)[1],indices(i)[2]]+1
	}
}

#AFC
library(FactoMineR)
res.ca = CA(donnees_AFC)



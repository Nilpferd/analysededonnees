#Adrien 21/11/16
#AFC chômeurs contre logements vacants (P13_CHOM1564 et P13_LOGVAC)

#Load the directory
#setwd("~/Documents/2016-2017/Mines/Cours/Analyse_donnees/Mini-projet/Statistiques_geographiques_Insee/scripts")	#to be modified by the user

#Load and create the dataframes
table = read.csv("../donnees/base_cc_resume_20161013_COM.csv")		#load the dateframe
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]	#delete unrelevant columns

#tableau_contingence_chom_logt_vacants=as.data.frame(table(table$P13_CHOM1564, table$P13_LOGVAC)) #tableau de contingence

library(ade4)
plot(table$P13_CHOM1564,main="Chômage par commune en France en 2013",col="blue") #histogramme brut x : numéro commune, y : nombre chômeurs

#Les communes au nombre de chômeurs élevés déforment l'échelle : on ne voit rien.
#Passons par les quantiles.

quantile(table$P13_CHOM1564,c(.33,.66),na.rm=TRUE)
#33% 66% 
# 11  34 Ca me paraît minuscule ! L'Insee dit que 10% de la population active est au chômage !
#Est-ce que ça s'explique par un effet de taille des communes ?
 quantile(table$P13_POP,c(.1,.2,.3,.4,.5,.6,.7,.8,.9),na.rm=TRUE)
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 
# 102  162  231  318  437  608  872 1382 2876
#Conclusion : oui il y a un effet de taille des communes, je ne m'étais pas rendu compte que c'était si petit...
quantile(table$P13_CHOM1564,c(.25,.5,0.75),na.rm=TRUE)
#25% 50% 75% 
#  8  19  49 
quantile(table$P13_CHOM1564,c(.1,.2,.3,.4,.5,.6,.7,.8,.9),na.rm=TRUE)
#10% 20% 30% 40% 50% 60% 70% 80% 90% 
#  4   7  10  14  19  27  39  63 143 

#Conclusion : je vais faire les classes suivantes pour le chômage :
#[0,11[,[11,34[,[34,63[,[63,144[,[144,+oo[
#chom_0_11, chom_11_34, chom_34_63, chom_63_144, chom_144_plus

donnees_classees = data.frame()	
nb_communes=length(table$P13_CHOM1564)
avoidNA1=complete.cases(table$P13_CHOM1564) 

#for (i in c(1:nb_communes)){
#	if(avoidNA[i]){				
#		if (table$P13_CHOM1564[i]<11){
#			donnees_classees[i,1]<-"chom_0_11"
#		} else if(table$P13_CHOM1564[i]<34){
#			donnees_classees[i,1]<-"chom_11_34"
#		}else if(table$P13_CHOM1564[i]<63){
#			donnees_classees[i,1]<-"chom_34_63"
#		}else if(table$P13_CHOM1564[i]<144){
#			donnees_classees[i,1]<-"chom_63_144"
#		}else{
#			donnees_classees[i,1]<-"chom_144_plus"
#		}
#	}
#}

#Même travail à mener sur les logements vacants.

plot(table$P13_LOGVAC,main="Logements vacants par commune en France en 2013",col="red")

quantile(table$P13_LOGVAC,c(.33,.66),na.rm=TRUE)
#33% 66% 
#10  29 
quantile(table$P13_LOGVAC,c(.25,.5,0.75),na.rm=TRUE)
#25% 50% 75% 
# 8  17  41 
quantile(table$P13_LOGVAC,c(.1,.2,.3,.4,.5,.6,.7,.8,.9),na.rm=TRUE)
#10% 20% 30% 40% 50% 60% 70% 80% 90% 
# 4   6   9  13  17  24  34  52 108

#Conclusion : je vais faire les classes suivantes pour les logements vacants :
#[0,10[,[10,24[, [24,52[, [52,108[, [108,+oo[
#logts_vac_0_10, logts_vac_10_24, logts_vac_24_52, logts_vac_52_108, logts_vac_108_plus

avoidNA2=complete.cases(table$P13_LOGVAC) 

#for (i in c(1:nb_communes)){
#	if(avoidNA[i]){				
#		if (table$P13_LOGVAC[i]<10){
#			donnees_classees[i,2]<-"logts_vac_0_10"
#		} else if(table$P13_LOGVAC[i]<24){
#			donnees_classees[i,2]<-"logts_vac_10_24"
#		}else if(table$P13_LOGVAC[i]<52){
#			donnees_classees[i,2]<-"logts_vac_24_52"
#		}else if(table$P13_LOGVAC[i]<108){
#			donnees_classees[i,2]<-"logts_vac_52_108"
#		}else{
#			donnees_classees[i,2]<-"logts_vac_108_plus"
#		}
#	}
#}

donnees_AFC=matrix(data=0,nrow=5,ncol=5)
rownames(donnees_AFC)<-c("chom_0_11","chom_11_34", "chom_34_63", "chom_63_144", "chom_144_plus")
colnames(donnees_AFC)<-c("logts_vac_0_10", "logts_vac_10_24", "logts_vac_24_52", "logts_vac_52_108", "logts_vac_108_plus")

indices<-function(i){
	k=0
	p=0
	if(avoidNA2[i]&avoidNA1[i]){				
		if (table$P13_CHOM1564[i]<11){
			k=1#"chom_0_11"
		} else if(table$P13_CHOM1564[i]<34){
			k=2#"chom_11_34"
		}else if(table$P13_CHOM1564[i]<63){
			k=3#"chom_34_63"
		}else if(table$P13_CHOM1564[i]<144){
			k=4#"chom_63_144"
		}else{
			k=5#"chom_144_plus"
		}
	}
	if(avoidNA2[i]&avoidNA1[i]){
						
		if (table$P13_LOGVAC[i]<10){
			p=1#"logts_vac_0_10"
		} else if(table$P13_LOGVAC[i]<24){
			p=2#"logts_vac_10_24"
		}else if(table$P13_LOGVAC[i]<52){
			p=3#"logts_vac_24_52"
		}else if(table$P13_LOGVAC[i]<108){
			p=4#"logts_vac_52_108"
		}else{
			p=5#"logts_vac_108_plus"
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



#Adrien 24/11/16
#
#
#Il faut bien que les sommes en ligne et les sommes en colonne aient un sens.
#AFC chômeurs contre nombre d'établissements agricoles (P13_CHOM1564 et ETAZ14)
#
#
#Je suis conscient que je compare des chiffres de 2013 pour le chômage et de 2014 pour les établissements agricoles.
#
#
#Je me souviens également de la remarque de Rodolphe sur le fait qu'il s'agit des sièges des entreprises 
#et donc qu'à cet égard Paris est la commune qui a le plus grand nombre d'établissements agricoles en France.
#
#
#En conséquence je vais retirer Paris, Lyon et Marseille.

#Load the directory
#setwd("~/Documents/2016-2017/Mines/Cours/Analyse_donnees/Mini-projet/Statistiques_geographiques_Insee/scripts")	#to be modified by the user

#Load and create the dataframes
table = read.csv("../donnees/base_cc_resume_20161013_COM.csv")		#load the dateframe
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]	#delete unrelevant columns

#suppression des trois plus grandes villes de France : Paris (75), Lyon (69) et Marseille (13).
table <- subset(table,table$DEP !=75&table$DEP !=69&table$DEP !=13)

#tableau_contingence_chom_logt_vacants=as.data.frame(table(table$P13_CHOM1564, table$ETAZ14)) #tableau de contingence

library(ade4)
plot(table$P13_CHOM1564,main="Chômage par commune en France en 2013",col="blue") #histogramme brut x : numéro commune, y : nombre chômeurs

#Les communes au nombre de chômeurs élevés déforment l'échelle : on ne voit rien.
#Passons par les quantiles.

quantile(table$P13_CHOM1564,c(.33,.66),na.rm=TRUE)
#33% 66% 
# 11  33 
#Les chiffres ont changé puisque j'ai retiré Paris, Lyon et Marseille.
quantile(table$P13_POP,c(.1,.2,.3,.4,.5,.6,.7,.8,.9),na.rm=TRUE)
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 
#102  161  229  315  423  596  853 1347  2774 

quantile(table$P13_CHOM1564,c(.25,.5,0.75),na.rm=TRUE)
#25% 50% 75% 
# 8  19  48  
quantile(table$P13_CHOM1564,c(.1,.2,.3,.4,.5,.6,.7,.8,.9),na.rm=TRUE)
#10% 20% 30% 40% 50% 60% 70% 80% 90% 
# 4   7  10  14  19  27  38  62 138

quantile(table$P13_CHOM1564,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,0.99),na.rm=TRUE)  
# 10%     20%     30%     40%     50%     60%     70%     80%     90%     95% 	99% 
# 4     	7      10     14      19      27      38      62      138     297   1593
    
#Conclusion : je vais faire les classes suivantes pour le chômage :
#[0,11[,[11,33[,[33,62[,[62,138[,[138,297[,[297,1593[,[1593,+oo[
#chom_0_11, chom_11_33, chom_33_62, chom_62_138, chom_138_297, chom_297_1593 et chom_1593_plus

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
nb_communes=length(table$P13_CHOM1564)
avoidNA1=complete.cases(table$P13_CHOM1564) 
avoidNA2=complete.cases(table$ETAZ14) 

donnees_AFC=matrix(data=0,nrow=7,ncol=7)
rownames(donnees_AFC)<-c("chom_0_11", "chom_11_33", "chom_33_62", "chom_62_138", "chom_138_297", "chom_297_1593", "chom_1593_plus")
colnames(donnees_AFC)<-c("etb_agri_0_5","etb_agri_5_11","etb_agri_11_16","etb_agri_16_24","etb_agri_24_33","etb_agri_33_66","etb_agri_66_plus")

indices<-function(i){
	k=0
	p=0
	if(avoidNA2[i]&avoidNA1[i]){				
		if (table$P13_CHOM1564[i]<11){
			k=1#"chom_0_11"
		} else if(table$P13_CHOM1564[i]<33){
			k=2#"chom_11_33"
		}else if(table$P13_CHOM1564[i]<62){
			k=3#"chom_33_62"
		}else if(table$P13_CHOM1564[i]<138){
			k=4#"chom_62_138"
		}else if(table$P13_CHOM1564[i]<297){
			k=5#"chom_138_297"
		}else if(table$P13_CHOM1564[i]<1593){
			k=6#"chom_297_1593"
		}else {
			k=7#"chom_1593_plus"
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



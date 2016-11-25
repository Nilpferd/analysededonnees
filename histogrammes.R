setwd("~/GitHub/analysededonnees")

table = read.csv("base_cc_resume_20161013_COM.csv")
table <- table[,!(colnames(table) %in% c("CODGEO","LIBGEO","REG"))]

pdf('histogrammes.pdf')
# Population
hist(table$P13_POP, breaks=5000, xlim=c(0,5000))

# superficie
hist(table$SUPERF, breaks=5000, xlim=c(0,50))

# naissances
hist(table$NAIS0813, breaks=5000, xlim=c(0,500))

# décès
hist(table$DECESD15, breaks=5000, xlim=c(0,100))

# ménages
hist(table$P13_MEN, breaks=5000, xlim=c(0,4000))

# logements
hist(table$P13_LOG, breaks=5000, xlim=c(0,5000))

# résidences principales
hist(table$P13_RP, breaks=5000, xlim=c(0,4000))

# résidences secondaires occasionnelles
hist(table$P13_RSECOCC, breaks=5000, xlim=c(0,300))

# logements vacants
hist(table$P13_LOGVAC, breaks=5000, xlim=c(0,300))

# résidences principales propriétaires
hist(table$P13_RP_PROP, breaks=5000, xlim=c(0,1000))

# Nombre de ménages fiscaux
hist(table$NBMENFISC13, breaks=5000, xlim=c(0,3000))

# Part des ménages fiscaux imposés
hist(table$PIMP13, breaks=5000, xlim=c(20,90))

# Médiane du niveau vie
hist(table$MED13, breaks=100, xlim=c(10000,40000))

# taux de pauvreté
hist(table$TP6013, breaks=100, xlim=c(0, 50))

# Emplois au LT
hist(table$P13_EMPLT, breaks=50000, xlim=c(0, 2000))

# Emplois salariés au LT
hist(table$P13_EMPLT_SAL, breaks=50000, xlim=c(0, 1000))

# population 15-64 ans
hist(table$P13_POP1564, breaks=50000, xlim=c(0, 2000))

# Chômeurs 15-64 ans
hist(table$P13_CHOM1564, breaks=50000, xlim=c(0, 300))

# actifs 15-64 ans
hist(table$P13_ACT1564, breaks=50000, xlim=c(0, 2000))

# Total des ets actifs
hist(table$ETTOT14, breaks=50000, xlim=c(0, 500))

# Ets actifs agriculture
hist(table$ETAZ14, breaks=500, xlim=c(0, 100))

# Ets actifs industrie
hist(table$ETBE14, breaks=50000, xlim=c(0, 50))

# Ets actifs construction
hist(table$ETFZ14, breaks=50000, xlim=c(0, 50))

# Ets actifs commerce services
hist(table$ETGU14, breaks=50000, xlim=c(0, 300))

# Ets actifs commerce réparation auto
hist(table$ETGZ14, breaks=50000, xlim=c(0, 50))

# Ets actifs adm publique
hist(table$ETOQ14, breaks=50000, xlim=c(0, 50))

# Ets actifs de 1 à 9 salariés
hist(table$ETTEF114, breaks=50000, xlim=c(0, 50))

# Ets actifs 10 salariés ou plus
hist(table$ETTEFP1014, breaks=10000, xlim=c(0, 30))

dev.off()

#######################################################
##### DONNEES BREF Ve REP #####
#####
##### Analyse : composition partisane Maire villes +9k
#####
##### AVRIL 2023
#####
#######################################################


#Je regarde la regularite des donnees sur les partis
sort(unique(tab.m9[,"NuancePolitique"]))

#Je procede a quelques nettoyages basees sur les map realisees pour l'Ass
idx <- which(tab.m9[,'NuancePolitique']=="PCF")
tab.m9[idx,"NuancePolitique"] <- "COM"

idx <- which(tab.m9[,'NuancePolitique']=="PS")
tab.m9[idx,"NuancePolitique"] <- "SOC"

idx <- which(tab.m9[,'NuancePolitique']=="SFIO")
tab.m9[idx,"NuancePolitique"] <- "SOC"

idx <- which(tab.m9[,'NuancePolitique']=="PRG")
tab.m9[idx,"NuancePolitique"] <- "RAD"

idx <- which(tab.m9[,'NuancePolitique']=="UDF")
tab.m9[idx,"NuancePolitique"] <- "CENTRE"

idx <- which(tab.m9[,'NuancePolitique']=="UDI")
tab.m9[idx,"NuancePolitique"] <- "CENTRE"

idx <- which(tab.m9[,'NuancePolitique']=="MRP")
tab.m9[idx,"NuancePolitique"] <- "CENTRE"

idx <- which(tab.m9[,'NuancePolitique']=="CDP")
tab.m9[idx,"NuancePolitique"] <- "CENTRE"

idx <- which(tab.m9[,'NuancePolitique']=="UMP")
tab.m9[idx,"NuancePolitique"] <- "DROITE"

idx <- which(tab.m9[,'NuancePolitique']=="LR")
tab.m9[idx,"NuancePolitique"] <- "DROITE"

idx <- which(tab.m9[,'NuancePolitique']=="RPR")
tab.m9[idx,"NuancePolitique"] <- "DROITE"

idx <- which(tab.m9[,'NuancePolitique']=="HORIZONS")
tab.m9[idx,"NuancePolitique"] <- "DROITE"

idx <- which(tab.m9[,'NuancePolitique']=="GE")
tab.m9[idx,"NuancePolitique"] <- "ECO"

idx <- which(tab.m9[,'NuancePolitique']=="CNI")
tab.m9[idx,"NuancePolitique"] <- "IND"

idx <- which(tab.m9[,'NuancePolitique']=="LREM")
tab.m9[idx,"NuancePolitique"] <- "CENTRE"

idx <- which(tab.m9[,'NuancePolitique']=="DVG")
tab.m9[idx,"NuancePolitique"] <- "CENTRE"

idx <- which(tab.m9[,'NuancePolitique']=="DVD")
tab.m9[idx,"NuancePolitique"] <- "DROITE"

idx <- which(tab.m9[,'NuancePolitique']=="UDF")
tab.m9[idx,"NuancePolitique"] <- "DROITE"

idx <- which(tab.m9[,'NuancePolitique']=="UNR")
tab.m9[idx,"NuancePolitique"] <- "DROITE"


#Cb de mandats sur chaque parti : 
table(tab.m9[,"NuancePolitique"])


#est-ce que tout le monde a une etiquette ?
summary(tab.m9[,"NuancePolitique"])
table(tab.m9[,"NuancePolitique"])
idx<- which(is.na(tab.m9[,"NuancePolitique"]))
#il manque 53 etiquettes


#Repartition des partis a chaque legislature : 
tp <-table(tab.m9[,"NuancePolitique"], tab.m9[,"Mandature"])
View(tp)

png(file.path(plots.folder, "partis_mandatures_m9.png"))
barplot(
  tp, 						# donnees a afficher
  legend.text=TRUE,			# afficher la legende des couleurs
  col=topo.colors(8),		# couleurs des barres
  xlim=c(0, 1.5*ncol(tp)),	
  las=2						# labels x verticaux
)
dev.off()





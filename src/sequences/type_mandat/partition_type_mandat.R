#########################################################
##### CREATION DES TABLES PAR TYPE DE MANDATS  #####
#####
#####
##### FEVRIER 2023 #####
#######################################################





#Je cherche a appliquer la methode de l'AS a chaque type de mandats identifie par le projet de loi
# Je cherche donc a couper tab.all en 3 tab : deputes, senateurs et maires des villes de +9k hab.
# Ensuite, je cree une table repr. correspondant aux 3 nouvelles tab creees






############################################
### CREATION DE LA TABLE DES DEPUTE-ES ####
###########################################



#J'identifie dans tab.all toutes les personnes qui ont exerce le mandat de depute
# Sur les bornes : 1958 et 2023

idx <- which(tab.all[,"TypeMandat"]=="DEPUTE"     #Je recup tous les mandats de D
             & tab.all[,"DateDebutMandat"] > "1958-01-01")   #Mais dont le mandat de D est sous la Ve
View(tab.all[idx,])        #545 mandats de D sous la Ve


#Le pb est que j'aimerai aussi tous les autres mandats effectues par les personnes elues sur D, 
#pas que les mandats de D

id_dep <- sort(unique(tab.all[idx,"Id"]))   #Je cherche a recuperer leurs id
idx <- which(tab.all[,"Id"] %in% id_dep)

#Je cree maintenant une table qui reprend tous les mandats des personnes identifiees : 
tab.dep <- tab.all[idx,]   #J'initie la table







##############################################
### CREATION DE LA TABLE DES ELU-ES SENAT ####
##############################################



#J'identifie dans tab.all toutes les personnes qui ont exerce le mandat de Senateur
# Sur les bornes : 1959 et 2023


idx <- which(tab.all[,"TypeMandat"]=="SENATEUR"     #Je recup tous les mandats de S
             & tab.all[,"DateDebutMandat"] > "1959-01-01")   #Mais dont le mandat de S est sous la Ve
View(tab.all[idx,])        #160 mandats de S sous la Ve


#Le pb est que j'aimerai aussi tous les autres mandats effectues par les personnes elues sur S, 
#pas que les mandats de S

id_sen <- sort(unique(tab.all[idx,"Id"]))   #Je cherche a recuperer leurs id, ils sont 95
idx <- which(tab.all[,"Id"] %in% id_sen)

#Je cree maintenant une table qui reprend tous les mandats des personnes identifiees : 
tab.sen <- tab.all[idx,]   #J'initie la table







###############################
### TABLE DES M+9 #############
###############################




#J'identifie dans tab.all toutes les personnes qui ont exerce le mandat de CM avec fonction M
# Sur les bornes : 1958 et 2023
# Sur les villes listees

# liste des villes de plus de 9000 habitants
selected.cities <- c(
  "BOURG EN BRESSE", "AMBERIEU EN BUGEY", "BELLEY", "DIVONNE LES BAINS", 
  "FFERNEY VOLTAIRE", "GEX", "MIRIBEL", "OYONNAX", "SAINT GENIS POUILLY", 
  "DIJON", "BEAUNE", "CHENOVE", "CHEVIGNY SAINT SAUVEUR", "FONTAINE LES DIJON", 
  "LONGVIC", "QUETIGNY", "TALANT", "SAINT DENIS", "MONTREUIL", "AUBERVILLIERS", 
  "NOISY LE GRAND", "PANTIN", "BONDY", "SEVRAN", "LA COURNEUVE", "ROMAINVILLE", 
  "CLICHY SOUS BOIS", "LE BOURGET", "RAINCY","AULNAY SOUS BOIS", "DRANCY", "BOBIGNY",
  "MARSEILLE", "AIX EN PROVENCE", "MARTIGUES", "AUBAGNE", "ARLES", "LA CIOTAT", 
  "MARIGNANE", "PENNES MIRABEAUN", "PORT DE BOUC", "TARASCON", "BOUC BEL AIR", "BERRE L ETANG", 
  "SAINT MARTIN DE CRAU", "CABRIES", "AURIOL"
)



# parmi les mandats, on garde uniquement ceux concernant les M de ces villes
idx <- which(								# numero des lignes de la table concernant ces maires-la
  tab.all[,"TypeTerritoire"]=="Commune" & tab.all[,"NomTerritoire"] %in% selected.cities
  & !is.na(tab.all[,"TypeFonction"]) & tab.all[,"TypeFonction"]=="M")
ids <- sort(unique(tab.all[idx,"Id"]))		# on recupere les ids de ces personnes
keep <- which(tab.all[,"Id"] %in% ids)		# on identifie toutes leurs lignes (mandats) dans la table 
tab.m9 <- tab.all[keep,]				# on ne garde que ces lignes-la

cat("Nombre de mandats total : ",nrow(tab.m9),"\n",sep="")

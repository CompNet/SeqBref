#######################################################
##### DONNEES COTE D OR Ve REP #####
#####
##### Analyses statistiques generales
#####
##### NOVEMBRE 2022
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les mandats attribues sur la periode ?

#Je rappelle que j'ai 74 noms uniques sur toute la periode. 
table(tab.cdo[,"Id"])

# Nb femmes total
table(tab.cdo[,"Sexe"]== "F")
#16 mandats sont occupes par des femmes
table(repr.cdo[,"Sexe"]=="F")


#Nb genre par type de mandat: 
genremandat <- table(tab.cdo[,"Sexe"], tab.cdo[,"TypeMandat"])
View(genremandat)


#Femmes par parti sur les mandats totaux : 
partigenre <- table(tab.cdo[,"NuancePolitique"], tab.cdo[,"Sexe"])
View(partigenre)




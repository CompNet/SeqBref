#######################################################
##### DONNEES SEINE SAINT DENIS Ve REP #####
#####
##### Analyses statistiques generales
#####
##### NOVEMBRE 2022
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les mandats attribues sur la periode ?

#Je rappelle que j'ai 130 noms uniques sur toute la periode. 
table(tab.ssd[,"Id"])

# Nb femmes total
table(tab.ssd[,"Sexe"]== "F")
#127 mandats sont occupes par des femmes
table(repr.ssd[,"Sexe"]=="F")
#23 femmes sur 130 elus

#Nb genre par type de mandat: 
genremandat <- table(tab.ssd[,"Sexe"], tab.ssd[,"TypeMandat"])
View(genremandat)


#Femmes par parti sur les mandats totaux : 
partigenre <- table(tab.ssd[,"NuancePolitique"], tab.ssd[,"Sexe"])
View(partigenre)




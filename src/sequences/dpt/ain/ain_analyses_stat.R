#######################################################
##### DONNEES AIN Ve REP #####
#####
##### Analyses statistiques generales
#####
##### NOVEMBRE 2022
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les mandats attribues sur la periode ?

#Je rappelle que j'ai 63 noms uniques sur toute la periode. 
table(tab.ain[,"Id"])

# Nb femmes total
table(tab.ain[,"Sexe"]== "F")
#16 mandats sont occupes par des femmes
table(repr.ain[,"Sexe"]=="F")


#Nb genre par type de mandat: 
genremandat <- table(tab.ain[,"Sexe"], tab.ain[,"TypeMandat"])
View(genremandat)


#Femmes par parti sur les mandats totaux : 
partigenre <- table(tab.ain[,"NuancePolitique"], tab.ain[,"Sexe"])
View(partigenre)




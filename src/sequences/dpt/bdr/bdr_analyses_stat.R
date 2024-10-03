#######################################################
##### DONNEES BOUCHES DU RHONE Ve REP #####
#####
##### Analyses statistiques generales
#####
##### DECEMBRE 2022
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les mandats attribues sur la periode ?

#Je rappelle que j'ai 179 noms uniques sur toute la periode. 
table(tab.bdr[,"Id"])

# Nb femmes total
table(tab.bdr[,"Sexe"]== "F")
#120 mandats sont occupes par des femmes
table(repr.bdr[,"Sexe"]=="F")
#Les femmes sont 24

#Nb genre par type de mandat: 
genremandat <- table(tab.bdr[,"Sexe"], tab.bdr[,"TypeMandat"])
View(genremandat)


#Femmes par parti sur les mandats totaux : 
partigenre <- table(tab.bdr[,"NuancePolitique"], tab.bdr[,"Sexe"])
View(partigenre)




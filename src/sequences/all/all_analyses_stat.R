#######################################################
##### DONNEES FUSION Ve REP #####
#####
##### Analyses statistiques generales
#####
##### DECEMBRE 2022
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les mandats attribues sur la periode ?

#Je rappelle que j'ai 469 noms uniques sur toute la periode. 
table(tab.all[,"Id"])


#Repartition des mandats
table(tab.all[,"TypeMandat"])


#Si on divise les 2765 mandats par les 446  personnes uniques, on trouve une moyenne de 6.1 mandats par personne unique
# Cela me parait impossible, beaucoup trop... 
cat("Nombre de mandats moyen par elu: ",nrow(tab.all),"/",nrow(repr.all),"=",nrow(tab.all)/nrow(repr.all),"\n",sep="")



#### ANALYSES RELATIVES AU GENRE ###

# Nb femmes total
table(tab.all[,"Sexe"]== "F")
# mandats sont occupes par des femmes
table(repr.all[,"Sexe"]=="F")
# NdV: c'est le contraire: tab.all pour les mandats et repr.all pour les elus

#Nb genre par type de mandat: 
genremandat <- table(tab.all[,"Sexe"], tab.all[,"TypeMandat"])
View(genremandat)

#Femmes par parti sur les mandats totaux : 
partigenre <- table(tab.all[,"NuancePolitique"], tab.all[,"Sexe"])
View(partigenre)


### ANALYSES RELATIVES AU RENOUVELLEMENT ###

types.mandats <- sort(unique(tab.all[,"TypeMandat"]))		# types uniques de mandats
nbre.mandats <- c(table(tab.all[,"TypeMandat"]))			# nombre de mandat par type
nbre.elus <- sapply(types.mandats, function(type.mandat) 	# nombre d'elu par type de mandat
			length(unique(tab.all[tab.all[,"TypeMandat"]==type.mandat, "Id"])))
 
tab <- data.frame(nbre.mandats[types.mandats], nbre.elus[types.mandats], nbre.mandats[types.mandats]/nbre.elus[types.mandats])
colnames(tab) <- c("Nombre de mandats", "Nombre d'elus uniques", "Moyenne")
print(tab)


######### DEBUT ANCIEN CODE ##########
#idx <- unique(tab.all[,"Id"])
#View(table(tab.all[idx,"TypeMandat"]))

#Avec ces lignes, j'identifie parmi tab.all toutes les personnes de manière unique
#Je regarde ensuite combien de fois chaque idx unique apparaît sur chaque type de mandat 
#Les resultats me paraissent vraiment surprenants, par ex je n'ai que 168 idx qui sont CM
#Alors que je decompte en tout 1403 mandats de CM, 
#ça voudrait dire qu'on a en moyenne 12 mandats par idx !!
#La c'est vraiment enorme car ca n'implique que la possibilite de la reelection et pas le cumul
#En effet, on travaille ici en separant chaque mandat par type, donc les resultats ne concernent qu'un type de mandat
######### FIN ANCIEN CODE ##########


### FINS DE MANDATS ###

table(tab.all[,"MotifFinMandat"], tab.all[,"TypeMandat"])

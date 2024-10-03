#########################################################
##### DONNEES BREF  #####
#####
##### CHARGEMENT ET NETTOYAGE SUR LES CUMULS POSSIBLES
#####
##### SEPTEMBRE 2022
#####
#######################################################





#####################################
############ CHARGEMENT #############
#####################################

# CHARGER LES DONNEES

# liste complete des individus de la BREF, avec mandats
file <- file.path(data.folder,"liste_BREF.csv")

tab.elus.mandats <- read.csv(file,
    header=TRUE,		# il y a un en-tete
    sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)

## On convertit les dates de la table principales en vraies dates R
tab.elus.mandats[,"DateDebutMandat"] <- as.Date(tab.elus.mandats[,"DateDebutMandat"], format="%d/%m/%Y")
tab.elus.mandats[,"DateNaissance"] <- as.Date(tab.elus.mandats[,"DateNaissance"], format="%d/%m/%Y")


tab.labels <- c("CONSEILLER EPCI", "CONSEILLER MUNICIPAL", "CONSEILLER DEPARTEMENTAL", "CONSEILLER REGIONAL", "DEPUTE", "SENATEUR")
tab.code <- c("EPCI", "CM", "CD", "CR", "D", "S")
tab.seq <- seqdef(
	tab.elus_mandats, 
	states=tab.code,
	labels=tab.labels,
	xtstep=6
)

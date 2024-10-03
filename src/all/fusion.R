#######################################################
##### DONNEES FRANCE 4 DEPTS #####
#####
##### Fusion des departements pour creation nouvelles tables
#####
##### DECEMBRE 2022
#######################################################



###########################################################
############### FUSION DES QUATRE TABLES ##################
###########################################################

#Je commence par fusionner ensemble toutes les donnees : Ain, CdO et BdR, SSD


# liste des colonnes communes aux trois tables
col.names <- c("Id", "NomDeNaissance", "Prenom", "Sexe", "DateDeNaissance", "TypeMandat",
               "DateDebutMandat", "DateFinMandat", "MotifFinMandat", "TypeFonction",
               "DateDebutFonction", "DateFinFonction", "MotifFinFonction", "NuancePolitique",
               "NomTerritoire", "TypeTerritoire", "Sources")
# on cree la nouvelle table en fusionnant mais seulement sur les colonnes communes
tab.all <- rbind(tab.ain[,col.names], tab.bdr[,col.names], tab.cdo[,col.names], 
                 tab.ssd[,col.names])




######## NETTOYAGE #####
table(tab.all[,"TypeMandat"])
#Pb de coherence dans les appellations des mandats europeens
idx <- which(tab.all[,"TypeMandat"]=="DEPUTE EUROPEEN")
tab.all[idx,"TypeMandat"] <- "REPRESENTANT EUROPEEN"

idx <- which(tab.all[,"TypeMandat"]=="REPRESENTANT EUROPEN")
tab.all[idx,"TypeMandat"] <- "REPRESENTANT EUROPEEN"

idx <- which(tab.all[,"TypeMandat"]=="REPRESENTANT PARLEMENT EUROPEEN")
tab.all[idx,"TypeMandat"] <- "REPRESENTANT EUROPEEN"

# on enregistre la table fusionnee sous forme de CSV
out.file <- file.path(data.folder, "all_mandats_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.all, 			# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	fileEncoding="UTF-8",
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)





#####################################################
############ CREATION TABLE DES ELUS ################
#####################################################


# liste des colonnes communes aux quatre tables
col.names <- c("Id", "NomDeNaissance", "Prenom", "Sexe", "DateNaissance", "CommuneDeNaissance",
               "DepartementDeNaissance", "PaysDeNaissance", "DateDeces", "CommuneDeDeces",
               "Nationalite", "LibelleProfession", "Sources", "Divers")
# on cree la nouvelle table en fusionnant mais seulement sur les colonnes communes
repr.all <- rbind(repr.ain[,col.names], repr.bdr[,col.names], repr.cdo[,col.names], 
                 repr.ssd[,col.names])



# liste des ids unique
id.chars <- sort(unique(repr.all[,"Id"]))


# on recupere tous les noms uniques
# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.all[,"NomDeNaissance"], repr.all[,"Prenom"], sep="_")))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")


#J'harmonise : 
sort(unique(repr.all[,"Divers"]))

idx<- which(repr.all[,"Divers"]=="COMBATTANT WWI")
repr.all[idx,'Divers'] <- "ANCIEN COMBATTANT"

idx<- which(repr.all[,"Divers"]=="Résistance")
repr.all[idx,'Divers'] <- "Resistance"

idx<- which(repr.all[,"Divers"]=="RESISTANT")
repr.all[idx,'Divers'] <- "Resistance"

idx<- which(repr.all[,"Divers"]=="RESPONSABLE RESISTANCE")
repr.all[idx,'Divers'] <- "Resistance"

idx<- which(repr.all[,"Divers"]=="LIBERATION")
repr.all[idx,'Divers'] <- "Resistance"

idx<- which(repr.all[,"Divers"]=="MINISTRE DE LA DEFENSE")
repr.all[idx,'Divers'] <- "Ministre"

idx<- which(repr.all[,"Divers"]=="MINISTRE")
repr.all[idx,'Divers'] <- "Ministre"

idx<- which(repr.all[,"Divers"]=="CONFRERIE DES CHEVALIERS DU TASTEVIN")
repr.all[idx,'Divers'] <- "Confrerie"

idx<- which(repr.all[,"Divers"]=="Franc-maçon")
repr.all[idx,'Divers'] <- "Confrerie"

idx<- which(repr.all[,"Divers"]=="franc-maçon")
repr.all[idx,'Divers'] <- "Confrerie"


# on enregistre la table des elus sous forme de CSV
out.file <- file.path(data.folder, "all_elus_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.all, 		# donnees qu'on veut enregistrer
	file=out.file,	# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	fileEncoding="UTF-8",
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

#######################################################
##### DONNEES COTE D OR Ve REP (2020) #####
#####
##### Chargement et nettoyage des donnees brutes du departement
#####
##### NOVEMBRE 2022
#######################################################





##################################################
############ CHARGEMENT TABLE ELU-ES #############
##################################################


# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
# Ou utiliser la fonction read.csv


file <- file.path(data.folder, "cdo", "CDO_elus.csv")

repr.cdo <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
repr.cdo[repr.cdo==""] <- NA



# A ce stade, la variable tab.cdo contient toutes les donnees : 373 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(repr.cdo)[1],"x",dim(repr.cdo)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
repr.cdo[,"DateNaissance"] <- as.Date(repr.cdo[,"DateNaissance"], format="%d/%m/%Y")
repr.cdo[,"DateDeces"] <- as.Date(repr.cdo[,"DateDeces"], format="%d/%m/%Y")


######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.cdo[,"NomDeNaissance"], repr.cdo[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 74 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie
# Correspond a la table des mandats.


### CORRECTION SUR LES PROFESSIONS ###

table(
	repr.cdo[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.cdo[,"LibelleProfession"])

#Je simplifie en regroupant par CPS
#Je m'appuie en partie sur la nomenclature de 2020
#1. Agriculteurs / exploitants
#2. Artisans / commercants / chefs entreprise
#3. Cadres / profession intellectuelle superieure
#4. Professions intermediaires (fonction publique, enseignement, etc)
#5. Employes
#6. Ouvriers

# Je remobilise la methode de la map : 
job.map <- load.map(file=file.path(data.folder, "map_jobs.csv"))								# fonction dans src/misc/maps.R
repr.cdo[,"LibelleProfession"] <- apply.map(values=repr.cdo[,"LibelleProfession"], map=job.map)	# meme remarque
table(
		repr.cdo[,"LibelleProfession"], 
		useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.cdo[,"LibelleProfession"])



### NETTOYER LES LIEUX DE NAISSANCE ET DECES ###
table(repr.cdo[,"CommuneDeNaissance"])

#J'enleve les tirets pour Saint Didier sur Beaujeu
idx <- which(repr.cdo[,"CommuneDeNaissance"] == "SAINT-DIDIER-sur-BEAUJEU")
repr.cdo[idx,"CommuneDeNaissance"] <- "SAINT DIDIER SUR BEAUJEU"

table(repr.cdo[,"DepartementDeNaissance"])

table(repr.cdo[,"PaysDeNaissance"])




##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on recupere tous les noms uniques
# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.cdo[,"NomDeNaissance"], repr.cdo[,"Prenom"], sep="_")))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")

# on calcule un numero unique pour chaque nom unique
id.vals <- 1:length(unique.names)

# on rajoute des zeros devant les numeros pour qu'ils contiennent tous le meme nombre de chiffres (ici : 3, tu peux ajuster)
id.chars <- sprintf("%03d", id.vals)

# on rajoute un préfixe identifiant le departement
id.chars <- paste0("cdo_",id.chars)

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
repr.cdo <- cbind(rep(NA,nrow(repr.cdo)), repr.cdo)

# on definit le nom de la colonne dans la table
colnames(repr.cdo)[1] <- "Id"

# on traite chaque nom unique un par un
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
	unique.name <- unique.names[i]
	#cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
	# on recupere les lignes contenant ce nom dans la table
	idx <- which(paste(repr.cdo[,"NomDeNaissance"], repr.cdo[,"Prenom"], sep="_")==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	repr.cdo[idx, "Id"] <- id.chars[i]
	
}



# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "cdo", "CDO_elus_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.cdo, 		# donnees qu'on veut enregistrer
	file=out.file, 	# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	fileEncoding="UTF-8",
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)




###################################################
############ CHARGEMENT TABLE MANDATS #############
###################################################

# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
# Ou utiliser la fonction read.csv


file <- file.path(data.folder, "cdo", "CDO_mandats.csv")

tab.cdo <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.cdo[tab.cdo==""] <- NA



# A ce stade, la variable tab.cdo contient toutes les donnees : 373 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(tab.cdo)[1],"x",dim(tab.cdo)[2],"\n", sep="")
# on affiche juste le debut de la table pour controle
cat("Apercu de la table :\n"); print(head(tab.cdo))  





######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(tab.cdo[,"NomDeNaissance"], tab.cdo[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 74 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie




### CORRECTION SUR LES PARTIS ###

summary(tab.cdo[,"NuancePolitique"])
#J'ai bien l'information pour 544 mandats (soit une nuance, soit NA)

table(
	tab.cdo[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)

#J'aimerais simplifier en quelques grandes familles de nuance
pol.map <- load.map(file=file.path(data.folder, "map_parties.csv"))							# fonction dans src/misc/maps.R
tab.cdo[,"NuancePolitique"] <- apply.map(values=tab.cdo[,"NuancePolitique"], map=pol.map)	# meme remarque
table(
	tab.cdo[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)





### CORRECTIONS SUR LE GENRE ###

summary(tab.cdo [,"Sexe"])
table(tab.cdo[,"Sexe"])

#Aucune erreur, je n'ai aucun NA
# J'ai seulement 33 mandats occupes par des femmes contre 511 par des hommes ! 





### CORRECTIONS SUR LES MANDATS ###

table(tab.cdo[,"TypeMandat"])

#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.cdo[,'NomTerritoire'])

table(tab.cdo[,"TypeTerritoire"])
#118 Cantons = 118 CD OK
#79 circo = manque 1
#289 communes = 289 CM OK
#56 departements = 29 CR + 26 S = 1 en trop

#Il manque une circo legis...


tab.cdo[241,"TypeTerritoire"] <- "Circonscription Legislative"


#Verification : 
table(tab.cdo[,"TypeTerritoire"])
#C'est bon, j'ai le compte par rapport aux mandats 




### CORRECTION SUR LES TYPES DE FONCTION ###

table(tab.cdo[,"TypeFonction"])

idx <- which(tab.cdo[,"TypeFonction"] == "1er VP")
tab.cdo[idx,"TypeFonction"] <- "VP"



### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.cdo[,"MotifFinMandat"])

#Je corrige DEM en DM
idx <- which(tab.cdo[,"MotifFinMandat"]=="DEM")
tab.cdo[idx,"MotifFinMandat"] <- "DM"



table(tab.cdo[,"MotifFinFonction"])
#Rien a modifier du cote des motifs de fin de fonction




### CORRECTION SUR LES DATES ###

# on convertit les dates de la table principales en vraies dates R
tab.cdo[,"DateDeNaissance"] <- as.Date(tab.cdo[,"DateDeNaissance"], format="%d/%m/%Y")
tab.cdo[,"DateDebutMandat"] <- as.Date(tab.cdo[,"DateDebutMandat"], format="%d/%m/%Y")
tab.cdo[,"DateFinMandat"] <- as.Date(tab.cdo[,"DateFinMandat"], format="%d/%m/%Y")
tab.cdo[,"DateDebutFonction"] <- as.Date(tab.cdo[,"DateDebutFonction"], format="%d/%m/%Y")
tab.cdo[,"DateFinFonction"] <- as.Date(tab.cdo[,"DateFinFonction"], format="%d/%m/%Y")



#Je commence par les dates de naissance

table(tab.cdo[,"DateDeNaissance"])
sort(unique(tab.cdo[,"DateDeNaissance"]))
#Aucune date de naissance abberante

table(tab.cdo[,"DateDebutMandat"])
sort(unique(tab.cdo[,"DateDebutMandat"]))

table(tab.cdo[,"DateFinMandat"])
sort(unique(tab.cdo[,"DateFinMandat"]))
#Les NA sont pour des mandats toujours en cours


table(tab.cdo[,"DateDebutFonction"])
sort(unique(tab.cdo[,"DateDebutFonction"]))
#Rien a relever


table(tab.cdo[,"DateFinFonction"])
sort(unique(tab.cdo[,"DateFinFonction"]))
#Probleme avec la date 0017-06-10

idx <- which(tab.cdo[,"DateFinFonction"]=="0017-06-10")
tab.cdo[idx,"DateFinFonction"] <- "2017-06-10"





##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.cdo <- cbind(rep(NA,nrow(tab.cdo)), tab.cdo)
# on definit le nom de la colonne dans la table
colnames(tab.cdo)[1] <- "Id"

# on traite chaque elu un par un
for(r in 1:nrow(repr.cdo))
{	# on recupere les informations personnelles communes aux deux tables
	id <- repr.cdo[r,"Id"]
	prenom <- repr.cdo[r,"Prenom"]
	nom <- repr.cdo[r,"NomDeNaissance"]
	sexe <- repr.cdo[r,"Sexe"]
	ddn <- repr.cdo[r,"DateNaissance"]
	cat("Traitement des mandats de ",prenom," ",nom," (",r,"/",nrow(repr.cdo),")\n",sep="")
	
	# on recherche l'elu dans la table des elus
	idx <- which(
			tab.cdo[,"NomDeNaissance"]==nom & tab.cdo[,"Prenom"]==prenom
			& (is.na(sexe) | repr.cdo[r,"Sexe"]==sexe)
			& (is.na(ddn) | repr.cdo[r,"DateNaissance"]==ddn)
	)
	
	# probleme si on ne trouve aucun mandat
	if(length(idx)==0)
		cat(paste0("!!!!!!!!!!! WARNING : aucun mandat trouve pour l'elu ",prenom," ",nom," (",sexe," -- ",ddn,") !!!!!!!!!!!\n"))
	
	# on met l'ID associee a l'elu sur les lignes de ses mandats
	tab.cdo[idx, "Id"] <- id
}

# on verifie si tous les mandats sont bien associes a un elu
idx <- which(is.na(tab.cdo[, "Id"]))
if(length(idx)>0)
{	cat(paste0("!!!!!!!!!!! WARNING : aucun elu trouve pour le(s) mandat(s) numero ",paste0(idx,collapse=", ")," !!!!!!!!!!!\n"))
	print(tab.cdo[idx,])
}




# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "cdo", "CDO_mandats_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.cdo, 			# donnees qu'on veut enregistrer
	file=out.file, 		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	fileEncoding="UTF-8",
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

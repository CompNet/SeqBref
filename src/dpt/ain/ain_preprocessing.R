#######################################################
##### DONNEES AIN Ve REP (2020) #####
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


file <- file.path(data.folder, "ain", "AIN_elus.csv")

repr.ain <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
repr.ain[repr.ain==""] <- NA



# a ce stade, la variable tab.ain contient toutes les donnees : 66 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(repr.ain)[1],"x",dim(repr.ain)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
repr.ain[,"DateNaissance"] <- as.Date(repr.ain[,"DateNaissance"], format="%d/%m/%Y")
repr.ain[,"DateDeces"] <- as.Date(repr.ain[,"DateDeces"], format="%d/%m/%Y")


######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.ain[,"NomDeNaissance"], repr.ain[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 66 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie
# Correspond a la table des mandats.


### CORRECTION SUR LES PROFESSIONS ###

table(
	repr.ain[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.ain[,"LibelleProfession"])

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
repr.ain[,"LibelleProfession"] <- apply.map(values=repr.ain[,"LibelleProfession"], map=job.map)	# meme remarque
table(
	repr.ain[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.ain[,"LibelleProfession"])



### NETTOYER LES LIEUX DE NAISSANCE ET DECES ###
table(repr.ain[,"CommuneDeNaissance"])

#J'enleve les tirets pour Saint Didier sur Beaujeu
idx <- which(repr.ain[,"CommuneDeNaissance"] == "SAINT-DIDIER-sur-BEAUJEU")
repr.ain[idx,"CommuneDeNaissance"] <- "SAINT DIDIER SUR BEAUJEU"

table(repr.ain[,"DepartementDeNaissance"])

table(repr.ain[,"PaysDeNaissance"])




##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.ain[,"NomDeNaissance"], repr.ain[,"Prenom"], sep="_")))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")

# on calcule un numero unique pour chaque nom unique
id.vals <- 1:length(unique.names)

# on rajoute des zeros devant les numeros pour qu'ils contiennent tous le meme nombre de chiffres (ici : 3, tu peux ajuster)
id.chars <- sprintf("%03d", id.vals)

# on rajoute un préfixe identifiant le departement
id.chars <- paste0("ain_",id.chars)

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
repr.ain <- cbind(rep(NA,nrow(repr.ain)), repr.ain)

# on definit le nom de la colonne dans la table
colnames(repr.ain)[1] <- "Id"

# on traite chaque nom unique un par un
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
	unique.name <- unique.names[i]
	#cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
	# on recupere les lignes contenant ce nom dans la table
	idx <- which(paste(repr.ain[,"NomDeNaissance"], repr.ain[,"Prenom"], sep="_")==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	repr.ain[idx, "Id"] <- id.chars[i]
}

# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "ain", "AIN_elus_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.ain, 		# donnees qu'on veut enregistrer
	file=out.file, 	# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	fileEncoding="UTF-8",
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names=TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)




###################################################
############ CHARGEMENT TABLE MANDATS #############
###################################################

# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
# Ou utiliser la fonction read.csv


file <- file.path(data.folder, "ain", "AIN_mandats.csv")

tab.ain <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.ain[tab.ain==""] <- NA



# A ce stade, la variable tab.ain contient toutes les donnees : 383 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(tab.ain)[1],"x",dim(tab.ain)[2],"\n", sep="")
# on affiche juste le debut de la table pour controle
cat("Apercu de la table :\n"); print(head(tab.ain))  





######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(tab.ain[,"NomDeNaissance"], tab.ain[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 66 personnes uniques
# Aucune erreur de doublons dans l'orthographe des noms et prenoms puisque effectif correspond au doc de la saisie




### CORRECTION SUR LES PARTIS ###

summary(tab.ain[,"NuancePolitique"])
#J'ai bien l'information pour 383 mandats (soit une nuance, soit NA)

table(
	tab.ain[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)

#J'aimerais simplifier en quelques grandes familles de nuances
pol.map <- load.map(file=file.path(data.folder, "map_parties.csv"))							# fonction dans src/misc/maps.R
tab.ain[,"NuancePolitique"] <- apply.map(values=tab.ain[,"NuancePolitique"], map=pol.map)	# meme remarque
table(
	tab.ain[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)





### CORRECTIONS SUR LE GENRE ###

summary(tab.ain [,"Sexe"])
table(tab.ain[,"Sexe"])

#Aucune erreur, je n'ai aucun NA
# J'ai seulement 17 mandats occupes par des femmes contre 366 par des hommes ! 





### CORRECTIONS SUR LES MANDATS ###

table(tab.ain[,"TypeMandat"])

#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.ain[,'NomTerritoire'])

#Erreur doublon sur la 4e circo
idx <- which(tab.ain[,"NomTerritoire"]=="4e CIRCONSCRIPTION AIN")
tab.ain[idx,"NomTerritoire"] <- "4EME CIRCONSCRIPTION AIN"

#J'harmonise le nom de la 5e circo
idx <- which(tab.ain[,"NomTerritoire"]=="5e CIRCONSCRIPTION AIN")
tab.ain[idx,"NomTerritoire"] <- "5EME CIRCONSCRIPTION AIN"

#J'enleve le tiret a Rhone Alpes
idx <- which(tab.ain[,"NomTerritoire"]=="RHONE-ALPES")
tab.ain[idx,"NomTerritoire"] <- "RHONE ALPES"

table(tab.ain[,'TypeTerritoire'])

#Je compare les resultats du type de territoire avec les mandats
#Il manquue des indications pour 4 mandats

#178 communes = 182 mandats de conseillers municipaux = OK
#69 circo lefgis > 61 D = 8 en trop = senateurs a transformer en dept = lignes 64, 65, 66, 98, 99, 298, 299, 338 et 339
#90 cantons < 92 CD = manque 2 --> ligne 190 et 225 = OK
#34 Departements < 21 senateurs + 23 CR = manque 10 --> les 2 regions + ligne 205
# 2 regions = lignes qui devraient etre des Dept

tab.ain[190, "TypeTerritoire"] <- "Canton"
tab.ain[225, "TypeTerritoire"] <- "Canton"
tab.ain[339, "TypeTerritoire"] <- "Departement"
tab.ain[64,  "TypeTerritoire"] <- "Departement"
tab.ain[65,  "TypeTerritoire"] <- "Departement"
tab.ain[66,  "TypeTerritoire"] <- "Departement"
tab.ain[98,  "TypeTerritoire"] <- "Departement"
tab.ain[99,  "TypeTerritoire"] <- "Departement"
tab.ain[298, "TypeTerritoire"] <- "Departement"
tab.ain[299, "TypeTerritoire"] <- "Departement"
tab.ain[338, "TypeTerritoire"] <- "Departement"
idx <- which(tab.ain[,"TypeTerritoire"]=="Region")
tab.ain[idx, "TypeTerritoire"] <- "Departement"

#Verification : 
table(tab.ain[,"TypeTerritoire"])
#C'est bon, j'ai le compte par rapport aux mandats 




### CORRECTION SUR LES TYPES DE FONCTION ###

table(tab.ain[,"TypeFonction"])
#Pas d'erreur a relever



### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.ain[,"MotifFinMandat"])

#AM = AU = autre mandat / idem pour DEPUTE
idx <- which(tab.ain[,"MotifFinMandat"]=="AM")
tab.ain[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ain[,"MotifFinMandat"]=="DEPUTE")
tab.ain[idx,"MotifFinMandat"] <- "AU"

#DEMISSION = DM
idx <- which(tab.ain[,"MotifFinMandat"]=="DEMISSION")
tab.ain[idx,"MotifFinMandat"] <- "DM"

#A ce stade je garde les fins de mandats pour gouvernement ou ministre
#Je sais que je ne l'ai pas releve systematiquement pour tous les mandats, ce qui est dommage...
# Peut etre une amelioration a apporter aux bases de donnees francaises sur les 4 depts.

table(tab.ain[,"MotifFinFonction"])

idx<- which(tab.ain[,"MotifFinFonction"]=="AM")
tab.ain[idx,'MotifFinFonction'] <- "AU"

idx<- which(tab.ain[,"MotifFinFonction"]=="VP")
tab.ain[idx,"MotifFinFonction"] <- "AU"




### CORRECTION SUR LES DATES ###

# on convertit les dates de la table principales en vraies dates R
tab.ain[,"DateDeNaissance"] <- as.Date(tab.ain[,"DateDeNaissance"], format="%d/%m/%Y")
tab.ain[,"DateDebutMandat"] <- as.Date(tab.ain[,"DateDebutMandat"], format="%d/%m/%Y")
tab.ain[,"DateFinMandat"] <- as.Date(tab.ain[,"DateFinMandat"], format="%d/%m/%Y")
tab.ain[,"DateDebutFonction"] <- as.Date(tab.ain[,"DateDebutFonction"], format="%d/%m/%Y")
tab.ain[,"DateFinFonction"] <- as.Date(tab.ain[,"DateFinFonction"], format="%d/%m/%Y")



#Je commence par les dates de naissance

table(tab.ain[,"DateDeNaissance"])
sort(unique(tab.ain[,"DateDeNaissance"]))
#Aucune date de naissance abberante

table(tab.ain[,"DateDebutMandat"])

#2 mandats n'ont pas de date de debut : le mandat de CM de Jean Paul Emin, et son mandat de CD (lignes 173 et 174)
#Pas de dates en debors des limites de la base, et les mandats sans date de fin sont bien ceux entames dans les 2020

table(tab.ain[,"DateFinMandat"])
sort(unique(tab.ain[,"DateFinMandat"]))
#Pas de pb, outre les 2 lignes mentionnees pour les mandats locaux de Jean Paul Emin
#Les NA sont pour des mandats toujours en cours


table(tab.ain[,"DateDebutFonction"])
sort(unique(tab.ain[,"DateDebutFonction"]))
#Rien a relever


table(tab.ain[,"DateFinFonction"])
sort(unique(tab.ain[,"DateFinFonction"]))






##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.ain <- cbind(rep(NA,nrow(tab.ain)), tab.ain)
# on definit le nom de la colonne dans la table
colnames(tab.ain)[1] <- "Id"

# on traite chaque elu un par un
for(r in 1:nrow(repr.ain))
{	# on recupere les informations personnelles communes aux deux tables
	id <- repr.ain[r,"Id"]
	prenom <- repr.ain[r,"Prenom"]
	nom <- repr.ain[r,"NomDeNaissance"]
	sexe <- repr.ain[r,"Sexe"]
	ddn <- repr.ain[r,"DateNaissance"]
	cat("Traitement des mandats de ",prenom," ",nom," (",r,"/",nrow(repr.ain),")\n",sep="")
	
	# on recherche l'elu dans la table des elus
	idx <- which(
		tab.ain[,"NomDeNaissance"]==nom & tab.ain[,"Prenom"]==prenom
		& (is.na(sexe) | repr.ain[r,"Sexe"]==sexe)
		& (is.na(ddn) | repr.ain[r,"DateNaissance"]==ddn)
	)
	
	# probleme si on ne trouve aucun mandat
	if(length(idx)==0)
		cat(paste0("!!!!!!!!!!! WARNING : aucun mandat trouve pour l'elu ",prenom," ",nom," (",sexe," -- ",ddn,") !!!!!!!!!!!\n"))
	
	# on met l'ID associee a l'elu sur les lignes de ses mandats
	tab.ain[idx, "Id"] <- id
}

# on verifie si tous les mandats sont bien associes a un elu
idx <- which(is.na(tab.ain[, "Id"]))
if(length(idx)>0)
{	cat(paste0("!!!!!!!!!!! WARNING : aucun elu trouve pour le(s) mandat(s) numero ",paste0(idx,collapse=", ")," !!!!!!!!!!!\n"))
	print(tab.ain[idx,])
}




# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "ain", "AIN_mandats_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
  x=tab.ain, 		# donnees qu'on veut enregistrer
  file=out.file, 	# nom du fichier a creer
  quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
  sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
  fileEncoding="UTF-8",
  row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
  col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

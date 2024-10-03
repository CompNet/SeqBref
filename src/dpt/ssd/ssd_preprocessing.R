#######################################################
##### DONNEES SEINE SAINT DENIS Ve REP (2020) #####
#####
##### Chargement et nettoyage des donnees brutes du departement
#####
##### DECEMBRE 2022
#######################################################





##################################################
############ CHARGEMENT TABLE ELU-ES #############
##################################################


# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
# Ou utiliser la fonction read.csv


file <- file.path(data.folder, "ssd", "SSD_elus.csv")

repr.ssd <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
repr.ssd[repr.ssd==""] <- NA



# a ce stade, la variable tab.ssd contient toutes les donnees : 130 lignes x 13 colonnes
cat("Dimension de la table complete : ",dim(repr.ssd)[1],"x",dim(repr.ssd)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
repr.ssd[,"DateNaissance"] <- as.Date(repr.ssd[,"DateNaissance"], format="%d/%m/%Y")
repr.ssd[,"DateDeces"] <- as.Date(repr.ssd[,"DateDeces"], format="%d/%m/%Y")


######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.ssd[,"NomDeNaissance"], repr.ssd[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 130 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie
# Correspond a la table des mandats.


### CORRECTION SUR LES PROFESSIONS ###

table(
	repr.ssd[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.ssd[,"LibelleProfession"])
#unique(repr.ssd[,"LibelleProfession"])

#Je simplifie

# Je remobilise la methode de la map : 
job.map <- load.map(file=file.path(data.folder, "map_jobs.csv"))								# fonction dans src/misc/maps.R
repr.ssd[,"LibelleProfession"] <- apply.map(values=repr.ssd[,"LibelleProfession"], map=job.map)	# meme remarque
table(
	repr.ssd[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.ssd[,"LibelleProfession"])



### NETTOYER LES LIEUX DE NAISSANCE ET DECES ###
table(repr.ssd[,"CommuneDeNaissance"])

table(repr.ssd[,"DepartementDeNaissance"])

table(repr.ssd[,"PaysDeNaissance"])




##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on recupere tous les noms uniques
# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.ssd[,"NomDeNaissance"], repr.ssd[,"Prenom"], sep="_")))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")

# on calcule un numero unique pour chaque nom unique
id.vals <- 1:length(unique.names)

# on rajoute des zeros devant les numeros pour qu'ils contiennent tous le meme nombre de chiffres (ici : 3, tu peux ajuster)
id.chars <- sprintf("%03d", id.vals)

# on rajoute un préfixe identifiant le departement
id.chars <- paste0("ssd_",id.chars)

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
repr.ssd <- cbind(rep(NA,nrow(repr.ssd)), repr.ssd)

# on definit le nom de la colonne dans la table
colnames(repr.ssd)[1] <- "Id"

# on traite chaque nom unique un par un
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
	unique.name <- unique.names[i]
	#cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
	# on recupere les lignes contenant ce nom dans la table
	idx <- which(paste(repr.ssd[,"NomDeNaissance"], repr.ssd[,"Prenom"], sep="_")==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	repr.ssd[idx, "Id"] <- id.chars[i]
	
}



# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "ssd", "SSD_elus_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.ssd, 			# donnees qu'on veut enregistrer
	file=out.file, 		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	fileEncoding="UTF-8",
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names=TRUE	# par contre on veut un en-tÃÂªte contenant les noms des colonnes
)




###################################################
############ CHARGEMENT TABLE MANDATS #############
###################################################

# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
# Ou utiliser la fonction read.csv


file <- file.path(data.folder, "ssd", "SSD_mandats.csv")

tab.ssd <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.ssd[tab.ssd==""] <- NA



# A ce stade, la variable tab.ssd contient toutes les donnees : 1040 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(tab.ssd)[1],"x",dim(tab.ssd)[2],"\n", sep="")
# on affiche juste le debut de la table pour controle
cat("Apercu de la table :\n"); print(head(tab.ssd))  





######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(tab.ssd[,"NomDeNaissance"], tab.ssd[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 130 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie




### CORRECTION SUR LES PARTIS ###

summary(tab.ssd[,"NuancePolitique"])
#J'ai bien l'information pour 808 mandats (soit une nuance, soit NA)

table(
	tab.ssd[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)

#J'aimerais simplifier en quelques grandes familles de nuance
pol.map <- load.map(file=file.path(data.folder, "map_parties.csv"))							# fonction dans src/misc/maps.R
tab.ssd[,"NuancePolitique"] <- apply.map(values=tab.ssd[,"NuancePolitique"], map=pol.map)	# meme remarque
table(
	tab.ssd[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)





### CORRECTIONS SUR LE GENRE ###

summary(tab.ssd [,"Sexe"])
table(tab.ssd[,"Sexe"])

#Aucune erreur, je n'ai aucun NA
# J'ai seulement 127 mandats occupes par des femmes contre 681 par des hommes ! 





### CORRECTIONS SUR LES MANDATS ###

table(tab.ssd[,"TypeMandat"])

#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.ssd[,'NomTerritoire'])

#des erreurs de saisies : 
idx <- which(tab.ssd[,"NomTerritoire"] == "13e CIRCONSCRIPTION SEINE SAINT GONIS")
tab.ssd[idx,"NomTerritoire"] <- "13e CIRCONSCRIPTION SEINE SAINT DENIS"

idx <- which(tab.ssd[,"NomTerritoire"] == "13e CIRCONSCRIPTION SEINE SAINT GUNIS")
tab.ssd[idx,"NomTerritoire"] <- "13e CIRCONSCRIPTION SEINE SAINT DENIS"


table(tab.ssd[,'TypeTerritoire'])

#Je compare les resultats du type de territoire avec les mandats





### CORRECTION SUR LES TYPES DE FONCTION ###

table(tab.ssd[,"TypeFonction"])
#Pas d'erreur a relever


### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.ssd[,"MotifFinMandat"])


#Je mets les CUMUL dans AU car je n'ai pas systematiquement saisi
idx <- which(tab.ssd[,"MotifFinMandat"]=="CUMUL")
tab.ssd[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ssd[,"MotifFinMandat"]=="MIN")
tab.ssd[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ssd[,"MotifFinMandat"]=="OCCUPATION")
tab.ssd[idx,"MotifFinMandat"] <- "FM"

#A ce stade je garde les fins de mandats pour gouvernement ou ministre
#Je sais que je ne l'ai pas releve systematiquement pour tous les mandats, ce qui est dommage...
# Peut etre une amelioration a apporter aux bases de donnees francaises sur les 4 depts.

table(tab.ssd[,"MotifFinFonction"])

idx<- which(tab.ssd[,"MotifFinFonction"]=="CUMUL")
tab.ssd[idx,'MotifFinFonction'] <- "AU"



### CORRECTION SUR LES DATES ###

# on convertit les dates de la table principales en vraies dates R
tab.ssd[,"DateDeNaissance"] <- as.Date(tab.ssd[,"DateDeNaissance"], format="%d/%m/%Y")
tab.ssd[,"DateDebutMandat"] <- as.Date(tab.ssd[,"DateDebutMandat"], format="%d/%m/%Y")
tab.ssd[,"DateFinMandat"] <- as.Date(tab.ssd[,"DateFinMandat"], format="%d/%m/%Y")
tab.ssd[,"DateDebutFonction"] <- as.Date(tab.ssd[,"DateDebutFonction"], format="%d/%m/%Y")
tab.ssd[,"DateFinFonction"] <- as.Date(tab.ssd[,"DateFinFonction"], format="%d/%m/%Y")



#Je commence par les dates de naissance

table(tab.ssd[,"DateDeNaissance"])
sort(unique(tab.ssd[,"DateDeNaissance"]))
#Aucune date de naissance abberante

table(tab.ssd[,"DateDebutMandat"])


table(tab.ssd[,"DateFinMandat"])
sort(unique(tab.ssd[,"DateFinMandat"]))
#Les NA sont pour des mandats toujours en cours


table(tab.ssd[,"DateDebutFonction"])
sort(unique(tab.ssd[,"DateDebutFonction"]))
#Rien a relever


table(tab.ssd[,"DateFinFonction"])
sort(unique(tab.ssd[,"DateFinFonction"]))



##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.ssd <- cbind(rep(NA,nrow(tab.ssd)), tab.ssd)
# on definit le nom de la colonne dans la table
colnames(tab.ssd)[1] <- "Id"

# on traite chaque elu un par un
for(r in 1:nrow(repr.ssd))
{	# on recupere les informations personnelles communes aux deux tables
	id <- repr.ssd[r,"Id"]
	prenom <- repr.ssd[r,"Prenom"]
	nom <- repr.ssd[r,"NomDeNaissance"]
	sexe <- repr.ssd[r,"Sexe"]
	ddn <- repr.ssd[r,"DateNaissance"]
	cat("Traitement des mandats de ",prenom," ",nom," (",r,"/",nrow(repr.ssd),")\n",sep="")
	
	# on recherche l'elu dans la table des elus
	idx <- which(
			tab.ssd[,"NomDeNaissance"]==nom & tab.ssd[,"Prenom"]==prenom
			& (is.na(sexe) | repr.ssd[r,"Sexe"]==sexe)
			& (is.na(ddn) | repr.ssd[r,"DateNaissance"]==ddn)
	)
	
	# probleme si on ne trouve aucun mandat
	if(length(idx)==0)
		cat(paste0("!!!!!!!!!!! WARNING : aucun mandat trouve pour l'elu ",prenom," ",nom," (",sexe," -- ",ddn,") !!!!!!!!!!!\n"))
	
	# on met l'ID associee a l'elu sur les lignes de ses mandats
	tab.ssd[idx, "Id"] <- id
}

# on verifie si tous les mandats sont bien associes a un elu
idx <- which(is.na(tab.ssd[, "Id"]))
if(length(idx)>0)
{	cat(paste0("!!!!!!!!!!! WARNING : aucun elu trouve pour le(s) mandat(s) numero ",paste0(idx,collapse=", ")," !!!!!!!!!!!\n"))
	print(tab.ssd[idx,])
}




# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "ssd", "SSD_mandats_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
  x=tab.ssd, 		# donnees qu'on veut enregistrer
  file=out.file, 	# nom du fichier a creer
  quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
  sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
  fileEncoding="UTF-8",
  row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
  col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

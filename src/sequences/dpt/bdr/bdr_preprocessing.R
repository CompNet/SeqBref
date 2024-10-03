#######################################################
##### DONNEES BOUCHES DU RHONE Ve REP (2020) #####
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


file <- file.path(data.folder, "bdr", "BDR_elus.csv")

repr.bdr <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
repr.bdr[repr.bdr==""] <- NA



# A ce stade, la variable tab.bdr contient toutes les donnees : 373 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(repr.bdr)[1],"x",dim(repr.bdr)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
repr.bdr[,"DateNaissance"] <- as.Date(repr.bdr[,"DateNaissance"], format="%d/%m/%Y")
repr.bdr[,"DateDeces"] <- as.Date(repr.bdr[,"DateDeces"], format="%d/%m/%Y")


######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.bdr[,"NomDeNaissance"], repr.bdr[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 179 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie
# Correspond a la table des mandats.


### CORRECTION SUR LES PROFESSIONS ###

table(
	repr.bdr[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.bdr[,"LibelleProfession"])
#unique(repr.bdr[,"LibelleProfession"])

#Je simplifie

# Je remobilise la methode de la map : 
job.map <- load.map(file=file.path(data.folder, "map_jobs.csv"))								# fonction dans src/misc/maps.R
repr.bdr[,"LibelleProfession"] <- apply.map(values=repr.bdr[,"LibelleProfession"], map=job.map)	# meme remarque
table(
	repr.bdr[,"LibelleProfession"], 
	useNA="ifany"					# "no" pour ignorer les NA
)
#summary(repr.bdr[,"LibelleProfession"])



### NETTOYER LES LIEUX DE NAISSANCE ET DECES ###
table(repr.bdr[,"CommuneDeNaissance"])

table(repr.bdr[,"DepartementDeNaissance"])

table(repr.bdr[,"PaysDeNaissance"])




##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on recupere tous les noms uniques
# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(repr.bdr[,"NomDeNaissance"], repr.bdr[,"Prenom"], sep="_")))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")

# on calcule un numero unique pour chaque nom unique
id.vals <- 1:length(unique.names)

# on rajoute des zeros devant les numeros pour qu'ils contiennent tous le meme nombre de chiffres (ici : 3, tu peux ajuster)
id.chars <- sprintf("%03d", id.vals)

# on rajoute un préfixe identifiant le departement
id.chars <- paste0("bdr_",id.chars)

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
repr.bdr <- cbind(rep(NA,nrow(repr.bdr)), repr.bdr)

# on definit le nom de la colonne dans la table
colnames(repr.bdr)[1] <- "Id"

# on traite chaque nom unique un par un
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
	unique.name <- unique.names[i]
	#cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
	# on recupere les lignes contenant ce nom dans la table
	idx <- which(paste(repr.bdr[,"NomDeNaissance"], repr.bdr[,"Prenom"], sep="_")==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	repr.bdr[idx, "Id"] <- id.chars[i]
	
}



# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "bdr", "BDR_elus_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.bdr, 		# donnees qu'on veut enregistrer
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


file <- file.path(data.folder, "bdr", "BDR_mandats.csv")

tab.bdr <- read.csv(
	file,
	header=TRUE,
	sep=";",
	fileEncoding="UTF-8",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.bdr[tab.bdr==""] <- NA



# A ce stade, la variable tab.bdr contient toutes les donnees : 1040 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(tab.bdr)[1],"x",dim(tab.bdr)[2],"\n", sep="")
# on affiche juste le debut de la table pour controle
cat("Apercu de la table :\n"); print(head(tab.bdr))  





######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(tab.bdr[,"NomDeNaissance"], tab.bdr[,"Prenom"],sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)


#J'ai 179 personnes uniques
# Aucune erreur de doublons dans l'orthographie des noms et prenoms puisque effectif correspond au doc de la saisie




### CORRECTION SUR LES PARTIS ###

summary(tab.bdr[,"NuancePolitique"])
#J'ai bien l'information pour 1040 mandats (soit une nuance, soit NA)

table(
	tab.bdr[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)

#J'aimerais simplifier en quelques grandes familles de nuance
#Liste des familles : 
#SOC / DROITE / COM / RAD / CENTRE / CNI / LREM  DVD et DVG
#Manque quelques nuances que je ne sais pas cmt classer


#J'aimerais ensuite remplacer le MoDEM par "Centre" par ex, etc
pol.map <- load.map(file=file.path(data.folder, "map_parties.csv"))							# fonction dans src/misc/maps.R
tab.bdr[,"NuancePolitique"] <- apply.map(values=tab.bdr[,"NuancePolitique"], map=pol.map)	# meme remarque
table(
	tab.bdr[,"NuancePolitique"], 
	useNA="ifany"					# "no" pour ignorer les NA
)





### CORRECTIONS SUR LE GENRE ###

summary(tab.bdr [,"Sexe"])
table(tab.bdr[,"Sexe"])

#Aucune erreur, je n'ai aucun NA
# J'ai seulement 120 mandats occupes par des femmes contre 920 par des hommes ! 





### CORRECTIONS SUR LES MANDATS ###

table(tab.bdr[,"TypeMandat"])

#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.bdr[,'NomTerritoire'])


table(tab.bdr[,'TypeTerritoire'])

#Je compare les resultats du type de territoire avec les mandats
#Il manquue un canton


tab.bdr[649,"TypeTerritoire"] <- "Canton"


#Verification : 
table(tab.bdr[,"TypeTerritoire"])
#C'est bon, j'ai le compte par rapport aux mandats 




### CORRECTION SUR LES TYPES DE FONCTION ###

table(tab.bdr[,"TypeFonction"])
#Pas d'erreur a relever
#Par rapport aux autres scripts s'ajoute la fonction Ma = Maire d'arrondissement.


### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.bdr[,"MotifFinMandat"])

#Arrestation et condmnation = "JUSTICE" (nouvelle categorie)
idx <- which(tab.bdr[,"MotifFinMandat"]=="ARRESTATION")
tab.bdr[idx,"MotifFinMandat"] <- "JUSTICE"

idx <- which(tab.bdr[,"MotifFinMandat"]=="CONDAMNATION")
tab.bdr[idx,"MotifFinMandat"] <- "JUSTICE"

#MALADIE = ON VA LES COMPTER DANS DC, C'EST DONC VRAIMENT A PRECISER DANS MES ECRITS !!
idx <- which(tab.bdr[,"MotifFinMandat"]=="MALADIE")
tab.bdr[idx,"MotifFinMandat"] <- "DC"

#Je mets les CUMUL dans AU car je n'ai pas systematiquement saisi
idx <- which(tab.bdr[,"MotifFinMandat"]=="CUMUL")
tab.bdr[idx,"MotifFinMandat"] <- "AU"

#A ce stade je garde les fins de mandats pour gouvernemnt ou ministre
#Je sais que je ne l'ai pas releve systematiquement pour tous les mandats, ce qui est dommage...
# Peut etre une amelioration a apporter aux bases de donnees francaises sur les 4 depts.

table(tab.bdr[,"MotifFinFonction"])

idx<- which(tab.bdr[,"MotifFinFonction"]=="CONDAMNATION")
tab.bdr[idx,'MotifFinFonction'] <- "JUSTICE"

idx<- which(tab.bdr[,"MotifFinFonction"]=="MALADIE")
tab.bdr[idx,"MotifFinFonction"] <- "DC"

idx<- which(tab.bdr[,"MotifFinFonction"]=="M")
tab.bdr[idx,"MotifFinFonction"] <- "AU"


### CORRECTION SUR LES DATES ###

# on convertit les dates de la table principales en vraies dates R
tab.bdr[,"DateDeNaissance"] <- as.Date(tab.bdr[,"DateDeNaissance"], format="%d/%m/%Y")
tab.bdr[,"DateDebutMandat"] <- as.Date(tab.bdr[,"DateDebutMandat"], format="%d/%m/%Y")
tab.bdr[,"DateFinMandat"] <- as.Date(tab.bdr[,"DateFinMandat"], format="%d/%m/%Y")
tab.bdr[,"DateDebutFonction"] <- as.Date(tab.bdr[,"DateDebutFonction"], format="%d/%m/%Y")
tab.bdr[,"DateFinFonction"] <- as.Date(tab.bdr[,"DateFinFonction"], format="%d/%m/%Y")



#Je commence par les dates de naissance

table(tab.bdr[,"DateDeNaissance"])
sort(unique(tab.bdr[,"DateDeNaissance"]))
#Aucune date de naissance abberante

table(tab.bdr[,"DateDebutMandat"])

#5 mandats n'ont pas de date de debut : je sais que ces mandats ont eu lieu mais je ne trouve aucune indication


table(tab.bdr[,"DateFinMandat"])
sort(unique(tab.bdr[,"DateFinMandat"]))

#Pas de pb, outre les 5 lignes mentionnees
#Les autres NA sont pour des mandats toujours en cours


table(tab.bdr[,"DateDebutFonction"])
sort(unique(tab.bdr[,"DateDebutFonction"]))
#Rien a relever


table(tab.bdr[,"DateFinFonction"])
sort(unique(tab.bdr[,"DateFinFonction"]))
#1 date pose pb : 0197 au lieu de 1947

idx <- which(tab.bdr[,"DateFinFonction"] == "0197-10-10")
tab.bdr[idx,"DateFinFonction"] <- "1947-10-10"




##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.bdr <- cbind(rep(NA,nrow(tab.bdr)), tab.bdr)
# on definit le nom de la colonne dans la table
colnames(tab.bdr)[1] <- "Id"

# on traite chaque elu un par un
for(r in 1:nrow(repr.bdr))
{	# on recupere les informations personnelles communes aux deux tables
	id <- repr.bdr[r,"Id"]
	prenom <- repr.bdr[r,"Prenom"]
	nom <- repr.bdr[r,"NomDeNaissance"]
	sexe <- repr.bdr[r,"Sexe"]
	ddn <- repr.bdr[r,"DateNaissance"]
	cat("Traitement des mandats de ",prenom," ",nom," (",r,"/",nrow(repr.bdr),")\n",sep="")
	
	# on recherche l'elu dans la table des elus
	idx <- which(
			tab.bdr[,"NomDeNaissance"]==nom & tab.bdr[,"Prenom"]==prenom
			& (is.na(sexe) | repr.bdr[r,"Sexe"]==sexe)
			& (is.na(ddn) | repr.bdr[r,"DateNaissance"]==ddn)
	)
	
	# probleme si on ne trouve aucun mandat
	if(length(idx)==0)
		cat(paste0("!!!!!!!!!!! WARNING : aucun mandat trouve pour l'elu ",prenom," ",nom," (",sexe," -- ",ddn,") !!!!!!!!!!!\n"))
	
	# on met l'ID associee a l'elu sur les lignes de ses mandats
	tab.bdr[idx, "Id"] <- id
}

# on verifie si tous les mandats sont bien associes a un elu
idx <- which(is.na(tab.bdr[, "Id"]))
if(length(idx)>0)
{	cat(paste0("!!!!!!!!!!! WARNING : aucun elu trouve pour le(s) mandat(s) numero ",paste0(idx,collapse=", ")," !!!!!!!!!!!\n"))
	print(tab.bdr[idx,])
}




# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "bdr", "BDR_mandats_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
  x=tab.bdr, 		# donnees qu'on veut enregistrer
  file=out.file, 	# nom du fichier a creer
  quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
  sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
  fileEncoding="UTF-8",
  row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
  col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

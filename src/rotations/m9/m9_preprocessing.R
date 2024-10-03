#######################################################
##### DONNEES BREF 1958-2020 #####
#####
##### Chargement et nettoyage des donnees brutes MAIRES VILLE +9K
##### 4 departements : Ain, BDR, CDO, et SsD
##### MARS 2023
#######################################################




#####################################
############ CHARGEMENT #############
#####################################

# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
#OU : 


file <- file.path(data.folder, "M9_4depts.csv")

tab.m9 <- read.csv(
  file,
  header=TRUE,
  sep=";",
  check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.m9[tab.m9==""] <- NA


# a ce stade, la variable tab.m9 contient toutes les donnees : 624 lignes x 16 colonnes
cat("Dimension de la table complete : ",dim(tab.m9)[1],"x",dim(tab.m9)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
tab.m9[,"DateDeNaissance"] <- as.Date(tab.m9[,"DateDeNaissance"], format="%d/%m/%Y")
tab.m9[,"DateDebutMandat"] <- as.Date(tab.m9[,"DateDebutMandat"], format="%d/%m/%Y")
tab.m9[,"DateFinMandat"] <- as.Date(tab.m9[,"DateFinMandat"], format="%d/%m/%Y")
tab.m9[,"DateDebutFonction"] <- as.Date(tab.m9[,"DateDebutFonction"], format="%d/%m/%Y")
tab.m9[,"DateFinFonction"] <- as.Date(tab.m9[,"DateFinFonction"], format="%d/%m/%Y")


######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(paste(tab.m9[,"NomDeNaissance"], 
                                  tab.m9[,"Prenom"], 
                                  tab.m9[,"DateDeNaissance"],
                                  tab.m9[,"Sexe"], sep="_")))

cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")
#J'ai 268 personnes uniques pour 624 mandats
#Taux de reelection moyen sur toute la Ve rep : 624 / 268 = 2.3 mandats par pers sous la Ve Rep.
#On voit deja en moyenne qu'on est sous le chiffre de 3 mandats prevus dans la loi




### CORRECTIONS SUR LE GENRE ###

summary(tab.m9[,"Sexe"])
table(tab.m9[,"Sexe"])

#50 F pour 574 H
#Tout le monde a bien un sexe




### CORRECTIONS SUR LES MANDATS ###

table(tab.m9[,"TypeMandat"])
#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.m9[,'NomTerritoire'])
#Je retrouve bien les 47 villes qui composent l'ensemble de la base de donnes





### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.m9[,"MotifFinMandat"])

#Deux irregularites : 

idx <- which(tab.m9[,"MotifFinMandat"]=="JUSTICE")
tab.m9[idx,"MotifFinMandat"] <- "DM"

idx <- which(tab.m9[,"MotifFinMandat"]=="VICHY")
tab.m9[idx,"MotifFinMandat"] <- "FM"

table(tab.m9[,"MotifFinMandat"])




table(tab.m9[,"MotifFinFonction"])
#Je constate les memes irregularites que pour les mandats
idx <- which(tab.m9[,"MotifFinFonction"]=="JUSTICE")
tab.m9[idx,"MotifFinFonction"] <- "DM"

idx <- which(tab.m9[,"MotifFinFonction"]=="VICHY")
tab.m9[idx,"MotifFinFonction"] <- "FM"

idx <- which(tab.m9[,"MotifFinFonction"]=="Internement en camp")
tab.m9[idx,"MotifFinFonction"] <- "DM"


idx <- which(tab.m9[,"MotifFinFonction"]=="AU")
#LIGNES 309, 582
idx <- which(tab.m9[,"MotifFinFonction"]=="CUMUL")
#439, 558, 570, 607, 620, 629







### CORRECTION SUR LES DATES ###

#Je commence par les dates de naissance

table(tab.m9[,"DateDeNaissance"])
sort(unique(tab.m9[,"DateDeNaissance"]))
#Aucune date de naissance aberante

table(tab.m9[,"DateDebutMandat"])
sort(unique(tab.m9[,"DateDebutMandat"]))
#Aucune date aberrante : toutes les dates sont bien comprises dans la periode etudiee a savoir 53-20


table(tab.m9[,"DateFinMandat"])
sort(unique(tab.m9[,"DateFinMandat"]))
#Pas de pb
#Les NA sont pour des mandats toujours en cours


table(tab.m9[,"DateDebutFonction"])
sort(unique(tab.m9[,"DateDebutFonction"]))
#Rien a relever






##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on recupere tous les noms uniques


unique.names <- unique(paste(tab.m9[,"NomDeNaissance"],
                             tab.m9[,"Prenom"]))

cat("Nombre de noms uniques dans la table (3) : ", length(unique.names), "\n", sep="")

# on calcule un numero unique pour chaque nom unique
id.vals <- 1:length(unique.names)

# on rajoute des zeros devant les numeros pour qu'ils contiennent tous le meme nombre de chiffres (ici : 3, tu peux ajuster)
id.chars <- sprintf("%03d", id.vals)

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.m9 <- cbind(rep(NA,nrow(tab.m9)), tab.m9)

# on definit le nom de la colonne dans la table
colnames(tab.m9)[1] <- "Id"

# on traite chaque nom unique un par un
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
  unique.name <- unique.names[i]
  cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
  # on recupere les lignes contenant ce nom dans la table
  idx <- which(paste(tab.m9[,"NomDeNaissance"],
                     tab.m9[,"Prenom"])==unique.name)
  # on met l'ID associe au nom sur ces lignes-ci
  tab.m9[idx, "Id"] <- id.chars[i]
  
}





# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "M9_4depts_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
  x=tab.m9, 		# donnees qu'on veut enregistrer
  file=out.file, 	# nom du fichier a creer
  quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
  sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
  row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
  col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)
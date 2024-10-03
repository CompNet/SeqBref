#######################################################
##### DONNEES BREF 1958-2020 #####
#####
##### Chargement et nettoyage des donnees brutes du Senat
#####
##### DECEMBRE 2022
#######################################################




#####################################
############ CHARGEMENT #############
#####################################

# CHARGER LES DONNEES


file <- file.path(data.folder, "Sen_Ve.csv")

tab.sen1 <- read.csv(
  file,
  header=TRUE,
  sep=";",
  check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.sen1[tab.sen1==""] <- NA
tab.sen1[tab.sen1=="NULL"] <- NA


# a ce stade, la variable tab.sen contient toutes les donnees : 3319 lignes x 13 colonnes
cat("Dimension de la table complete : ",dim(tab.sen1)[1],"x",dim(tab.sen1)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
tab.sen1[,"DateNaissance"] <- as.Date(tab.sen1[,"DateNaissance"], format="%d/%m/%Y")
tab.sen1[,"DateDeces"] <- as.Date(tab.sen1[,"DateDeces"], format="%d/%m/%Y")
tab.sen1[,"DateDebutMandat"] <- as.Date(tab.sen1[,"DateDebutMandat"], format="%d/%m/%Y")
tab.sen1[,"DateFinMandat"] <- as.Date(tab.sen1[,"DateFinMandat"], format="%d/%m/%Y")





######################################
############ CORRECTIONS #############
######################################

#Pour le nettoyage, je dois manipuler les ids :
#But : verifier que chaque mandat qu'enchaine un elu quand il en a plusieurs se succedent bien et n'ont pas des recouvrements de date


ids <- sort(unique(tab.sen1[,"Elu_IdIndividu"]))
cat("Nombre d'elus uniques dans la table : ", length(ids), "\n", sep="")


# NdV: pour tester vraiment les recouvrements de mandats pour la meme personne
tab.sen2 <- tab.sen1
idx <- which(is.na(tab.sen2[,"DateFinMandat"]))
tab.sen2[idx,"DateFinMandat"] <- as.Date("2023/03/24")		# on remplace les dates de fin qui sont NA, pour simplifier le code source
doublons <- tab.sen1[-(1:nrow(tab.sen1)),]					# table vide, destinee a contenir les doublons detectes
rm.list <- c()												# numeros des lignes a supprimer
for(id in ids)												# on boucle sur chaque personne
{	mndts <- which(tab.sen2[,"Elu_IdIndividu"]==id)			# on recupere les mandat de la personne
	if(length(mndts)>1)										# s'il y en a plusieurs, on les compare 2 a 2
	{	cat("id=",id,": ",length(mndts)," mandats a comparer\n",sep="")
		#print(tab.ass2[mndts,])
		for(m1 in 1:(length(mndts)-1))						# boucle du 1er mandat a comparer
		{	for(m2 in (m1+1):length(mndts))					# boucle du 2nd mandat a comparer
			{	#cat(m1," vs. ",m2," (/",length(mndts),")\n",sep="")
				if(tab.sen2[mndts[m1],"DateDebutMandat"]<=tab.sen2[mndts[m2],"DateDebutMandat"] & tab.sen2[mndts[m1],"DateFinMandat"]>=tab.sen2[mndts[m2],"DateDebutMandat"]
						| tab.sen2[mndts[m2],"DateDebutMandat"]<=tab.sen2[mndts[m1],"DateDebutMandat"] & tab.sen2[mndts[m2],"DateFinMandat"]>=tab.sen2[mndts[m1],"DateDebutMandat"])
				{	cat("Recouvrement de mandats:\n")
					print(tab.sen2[mndts[c(m1,m2)],])
					doublons <- rbind(doublons, tab.sen2[mndts[c(m1,m2)],], rep(NA,ncol(tab.sen2)))
					rm.list <- c(rm.list, mndts[m2])
				}
			}
		}
	}
}


#J'ouvre la table doublons et je verifie qu'il s'agit bien de doublons
#Beaucoup de cas de vrais doublons
#Mais aussi des erreurs, correspondant en fait a des pbs de fins de mandats qui n'etaient pas attribuees.
#Egalement des pbs de chevauchements

#Liste des cas a modifier manuellement : 15 cas ne correspondent pas reellement a des doublons mais necessitent nettoyage manuek

#Je traite ces cas directement dans le fichiers CSV
#(ces lignes ne sauront donc plus valables, mais je les laisse pour garder une trace du travail)



# exporte la liste de doublons de mandats
out.file <- file.path(data.folder, "doublons_senat.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
  x=doublons, 		# donnees qu'on veut enregistrer
  file=out.file, 	# nom du fichier a creer
  quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
  sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
  row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
  col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)


# table sans les doublons
tab.sen <- tab.sen1[-rm.list,]
cat("Nombre de mandats moyen par elu : ",nrow(tab.sen)/length(ids),"\n",sep="")
# 1.8 mandats par elu

ids <- sort(unique(tab.sen[,"Elu_IdIndividu"]))
cat("Nombre d'elus uniques dans la table : ", length(ids), "\n", sep="")

############################################################################




#################################
### CORRECTIONS SUR LE GENRE ###
#################################


summary(tab.sen [,"Sexe"])
table(tab.sen[,"Sexe"])

#364 mandats F pour 2837 H





### CORRECTIONS SUR LES MANDATS ###

table(tab.sen[,"TypeMandat"])
#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.sen[,'NomTerritoire'])
#La liste des departements est propre, nettoyage deja effectue sur la BREF




### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.sen[,"MotifFinMandat"])
#Un peu plus propre que l'ass, mais tout de meme quelques nettoyages : 
#Je m'aligne sur les nettoyages effectues sur l'assemblee
#8 FM dans Ass : Annulation - AU - Deces - Demission - Nomination - AM - FM - Gouvernement

idx <- which(tab.sen[,"MotifFinMandat"]== "ANNULE PAR CC")
tab.sen[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.sen[,"MotifFinMandat"]== "CESSATION DE MANDAT 1962")
tab.sen[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.sen[,"MotifFinMandat"]== "DC")
tab.sen[idx,"MotifFinMandat"] <- "Deces"

idx <- which(tab.sen[,"MotifFinMandat"]== "DECEDE")
tab.sen[idx,"MotifFinMandat"] <- "Deces"

idx <- which(tab.sen[,"MotifFinMandat"]== "DECH PAR CC")
tab.sen[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.sen[,"MotifFinMandat"]== "Decheance")
tab.sen[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.sen[,"MotifFinMandat"]== "Demission d'office")
tab.sen[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.sen[,"MotifFinMandat"]== "DEMISSIONNAIRE")
tab.sen[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.sen[,"MotifFinMandat"]== "DO")
tab.sen[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.sen[,"MotifFinMandat"]== "DV")
tab.sen[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.sen[,"MotifFinMandat"]== "ELU DEPUTE")
tab.sen[idx,"MotifFinMandat"] <- "AM"

idx <- which(tab.sen[,"MotifFinMandat"]== "elu depute")
tab.sen[idx,"MotifFinMandat"] <- "AM"

idx <- which(tab.sen[,"MotifFinMandat"]== "ELU DEPUTE EUROPEEN")
tab.sen[idx,"MotifFinMandat"] <- "AM"

idx <- which(tab.sen[,"MotifFinMandat"]== "FIN DE MANDAT")
tab.sen[idx,"MotifFinMandat"] <- "FM"

idx <- which(tab.sen[,"MotifFinMandat"]== "Independance")
tab.sen[idx,"MotifFinMandat"] <- "FM"

idx <- which(tab.sen[,"MotifFinMandat"]== "MEMBRE GOUVERNEMENT")
tab.sen[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.sen[,"MotifFinMandat"]== "Nomme au gouvernement")
tab.sen[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.sen[,"MotifFinMandat"]== "Nommee au gouvernement")
tab.sen[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.sen[,"MotifFinMandat"]== "NOMME MEMBRE CC")
tab.sen[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.sen[,"MotifFinMandat"]== "Nomme prefet")
tab.sen[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.sen[,"MotifFinMandat"]== "PREMIER MINISTRE")
tab.sen[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.sen[,"MotifFinMandat"]== "Nomme au Conseil economique, social et environnemental")
tab.sen[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.sen[,"MotifFinMandat"]== "Nomme au Conseil constitutionnel")
tab.sen[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.sen[,"MotifFinMandat"]== "Retour du titulaire")
tab.sen[idx,"MotifFinMandat"] <- "FM"

idx <- which(tab.sen[,"MotifFinMandat"]== "NE SE REPRESENTE PAS")
tab.sen[idx,"MotifFinMandat"] <- "FM"

idx <- which(tab.sen[,"MotifFinMandat"]== "NON REELU")
tab.sen[idx,"MotifFinMandat"] <- "FM"


table(tab.sen[,"MotifFinMandat"])


### CORRECTION SUR LES DATES ###

#Je commence par les dates de naissance

table(tab.sen[,"DateNaissance"])
sort(unique(tab.sen[,"DateNaissance"]))
#Aucune date de naissance aberante
#Nettoyage deja effectue sur la BREF

table(tab.sen[,"DateDebutMandat"])
sort(unique(tab.sen[,"DateDebutMandat"]))
#Aucune date aberrante : toutes les dates sont bien comprises dans la periode etudiee a savoir 59-19
#Nettoyage deja effectue sur la BREF
#Cependant, les donnees sont tres heterogenes, pour le traitement par la suite, besoin d'une homogeneisation
#Je ne retiens qu'une date par election : 



table(tab.sen[,"DateFinMandat"])
sort(unique(tab.sen[,"DateFinMandat"]))
#Pas de pb




# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "Sen_Ve_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.sen, 		# donnees qu'on veut enregistrer
	file=out.file, 	# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

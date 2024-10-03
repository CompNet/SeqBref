#######################################################
##### DONNEES BREF 1958-2020 #####
#####
##### Chargement et nettoyage des donnees brutes de l'assemblee
#####
##### FEVRIER 2023
#######################################################




#####################################
############ CHARGEMENT #############
#####################################

# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)
#OU : 


file <- file.path(data.folder, "Ass_Ve.csv")

tab.ass1 <- read.csv(
  file,
  header=TRUE,
  sep=";",
  check.names=FALSE	# ne pas modifier les noms des colonnes
)


# on remplace les cellules vides par des NA explicites
tab.ass1[tab.ass1==""] <- NA
tab.ass1[tab.ass1=="NULL"] <- NA



# a ce stade, la variable tab.ass contient toutes les donnees : 9273 lignes x 12 colonnes
cat("Dimension de la table complete : ",dim(tab.ass1)[1],"x",dim(tab.ass1)[2],"\n", sep="")


# on convertit les dates de la table principales en vraies dates R
tab.ass1[,"DateNaissance"] <- as.Date(tab.ass1[,"DateNaissance"], format="%d/%m/%Y")
tab.ass1[,"DateDeces"] <- as.Date(tab.ass1[,"DateDeces"], format="%d/%m/%Y")
tab.ass1[,"DateDebutMandat"] <- as.Date(tab.ass1[,"DateDebutMandat"], format="%d/%m/%Y")
tab.ass1[,"DateFinMandat"] <- as.Date(tab.ass1[,"DateFinMandat"], format="%d/%m/%Y")


######################################
############ CORRECTIONS #############
######################################


### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

############################################################################
# il faut manipuler les id maintenant, plus les combinaisons de noms/ddn
ids <- sort(unique(tab.ass1[,"Elu_IdIndividu"]))
cat("Nombre d'elus uniques dans la table : ", length(ids), "\n", sep="")
############################################################################


#J'ai 4008 personnes uniques pour 9283 mandats




############################################################################
############################################################################
#Je verifie que je n'ai pas des recouvrements de mandats : 

# NdV: pour tester vraiment les recouvrements de mandats pour la meme personne
tab.ass2 <- tab.ass1
idx <- which(is.na(tab.ass2[,"DateFinMandat"]))
tab.ass2[idx,"DateFinMandat"] <- as.Date("2023/03/24")		# on remplace les dates de fin qui sont NA, pour simplifier le code source
doublons <- tab.ass1[-(1:nrow(tab.ass1)),]					# table vide, destinee a contenir les doublons detectes
rm.list <- c()												# numeros des lignes a supprimer
for(id in ids)												# on boucle sur chaque personne
{	mndts <- which(tab.ass2[,"Elu_IdIndividu"]==id)			# on recupere les mandat de la personne
	if(length(mndts)>1)										# s'il y en a plusieurs, on les compare 2 a 2
	{	cat("id=",id,": ",length(mndts)," mandats a comparer\n",sep="")
		#print(tab.ass2[mndts,])
		for(m1 in 1:(length(mndts)-1))						# boucle du 1er mandat a comparer
		{	for(m2 in (m1+1):length(mndts))					# boucle du 2nd mandat a comparer
			{	#cat(m1," vs. ",m2," (/",length(mndts),")\n",sep="")
				if(tab.ass2[mndts[m1],"DateDebutMandat"]<=tab.ass2[mndts[m2],"DateDebutMandat"] & tab.ass2[mndts[m1],"DateFinMandat"]>=tab.ass2[mndts[m2],"DateDebutMandat"]
					| tab.ass2[mndts[m2],"DateDebutMandat"]<=tab.ass2[mndts[m1],"DateDebutMandat"] & tab.ass2[mndts[m2],"DateFinMandat"]>=tab.ass2[mndts[m1],"DateDebutMandat"])
				{	cat("Recouvrement de mandats:\n")
					print(tab.ass2[mndts[c(m1,m2)],])
					doublons <- rbind(doublons, tab.ass2[mndts[c(m1,m2)],], rep(NA,ncol(tab.ass2)))
					rm.list <- c(rm.list, mndts[m2])
				}
			}
		}
	}
}

# nombre de recouvrements :
rm.list <- sort(unique(rm.list))
cat("Nombre de paires de mandats se recouvrant : ",length(rm.list),"\n",sep="")

# exporte la liste de paires de mandats se recouvrant
out.file <- file.path(data.folder, "recouvrements_assemblee.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=doublons, 		# donnees qu'on veut enregistrer
	file=out.file, 		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)



#Avant de supprimer les 66 doublons, je verifie manuellement dans la table des doublons qu'il s'agisse bien de doublons

#Le doublon 1 n'en est pas un : recouvrement d'un mois entre les deux mandats
#Je modifie donc les donnees dans la table principale pour effacer le chevauchement
#Je vais directement modifie la table csv, donc normalement ces doublons n'apparaissent plus

#Liste de ceux qui ne sont pas des doublons : 
#Vouillot Herve : 81-86 puis 85-90 [action realisee sur csv]
#Sarlot Joel : 97-02 puis 02-07 [action realisee sur csv]
#Le Nay Jacques : 97-02 puis 02-07[Action realisee sur csv]
#Warsmann jean luc : idem [action realisee sur csv]
#Suguenot alain : 93-97 puis 97-01 or le mandat de 97 n'existe pas [action realisee sur csv]
#Patriat Fran�ois : 88-93 puis 92-97 (et trois autres cas) [action realisee sur csv]
#De broissia louis : idem [action realisee sur csv]
#douste blazy philippe : 97-01 puis 01-02 [action realisee sur csv]




#Apres ce travail, il ne reste que 48 doublons qui ne sont que des cas de doublons de mandats reels




# table sans les doublons
tab.ass <- tab.ass1
if(length(rm.list)>0)
	tab.ass <- tab.ass[-rm.list,]
cat("Nombre de mandats moyen par elu : ",nrow(tab.ass)/length(ids),"\n",sep="")	
# 2,30 mandats par elu

############################################################################




### CORRECTIONS SUR LE GENRE ###

summary(tab.ass[,"Sexe"])
table(tab.ass[,"Sexe"])

#945 F pour 8280 H
#Tout le monde a bien un sexe




### CORRECTIONS SUR LES MANDATS ###

table(tab.ass[,"TypeMandat"])
#Le compte y est, pas d'erreur particuliere a relever




### CORRECTIONS SUR LES TERRITOIRES ###

table(tab.ass[,'NomTerritoire'])
#Je ne vais pas nettoyer : difficile a faire a l'oeil nu 
#Le travail a deja ete fait dans le nettoyage de la BREF
#Je constate juste que certains mandats sont relies a des circo, et d'autres a des departments





### CORRECTION SUR LES MOTIFS DE FIN DE MANDAT ET FONCTION ###

table(tab.ass[,"MotifFinMandat"])
#Beaucoup d'irregularites : 

idx <- which(tab.ass[,"MotifFinMandat"]=="REPRISE DE L EXERCICE DU MANDAT D UN ANCIEN MEMBRE DU GOUVERNEMENT")
tab.ass[idx,"MotifFinMandat"] <- "Retour du titulaire"

idx <- which(tab.ass[,"MotifFinMandat"]=="Nommee au gouvernement")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="Nomme au gouvernement")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="Nommee au Gouvernement")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="Nommee au gouverment")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="Nomme au Gouvernement")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="Nomme au gouverment")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="NOMINATION COMME MEMBRE DU GOUVERNEMENT")
tab.ass[idx,"MotifFinMandat"] <- "Gouvernement"

idx <- which(tab.ass[,"MotifFinMandat"]=="Annulation ")
tab.ass[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.ass[,"MotifFinMandat"]=="ANNULATION DE L ELECTION SUR DECISION DU CONSEIL CONSTITUTIONNEL")
tab.ass[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.ass[,"MotifFinMandat"]=="Electio annulee")
tab.ass[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.ass[,"MotifFinMandat"]=="Election annulee")
tab.ass[idx,"MotifFinMandat"] <- "Annulation"

idx <- which(tab.ass[,"MotifFinMandat"]=="DC")
tab.ass[idx,"MotifFinMandat"] <- "Deces"

idx <- which(tab.ass[,"MotifFinMandat"]=="DECES")
tab.ass[idx,"MotifFinMandat"] <- "Deces"

idx <- which(tab.ass[,"MotifFinMandat"]=="Deces ")
tab.ass[idx,"MotifFinMandat"] <- "Deces"

idx <- which(tab.ass[,"MotifFinMandat"]=="Demis d'office ")
tab.ass[idx,"MotifFinMandat"] <- "Decheance"

idx <- which(tab.ass[,"MotifFinMandat"]=="Demis d'office")
tab.ass[idx,"MotifFinMandat"] <- "Decheance"

idx <- which(tab.ass[,"MotifFinMandat"]=="Demission ")
tab.ass[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.ass[,"MotifFinMandat"]=="DEMISSION")
tab.ass[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.ass[,"MotifFinMandat"]=="Demission pour cumul des mandats")
tab.ass[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.ass[,"MotifFinMandat"]=="DEMISSION D OFFICE SUR DECISION DU CONSEIL CONSTITUTIONNEL")
tab.ass[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.ass[,"MotifFinMandat"]=="DEMISSION POUR CAUSE D INCOMPATIBILITE PREVUE AUX ARTICLES LO 137 LO 137 1 LO 141 OU LO 141 1 DU CODE ELECTORAL")
tab.ass[idx,"MotifFinMandat"] <- "Demission"

idx <- which(tab.ass[,"MotifFinMandat"]=="ELECTION AU SENAT")
tab.ass[idx,"MotifFinMandat"] <- "Elu Senat"

idx <- which(tab.ass[,"MotifFinMandat"]=="elu au Senat")
tab.ass[idx,"MotifFinMandat"] <- "Elu Senat"

idx <- which(tab.ass[,"MotifFinMandat"]=="Elu au Senat")
tab.ass[idx,"MotifFinMandat"] <- "Elu Senat"

idx <- which(tab.ass[,"MotifFinMandat"]=="Elue au Senat")
tab.ass[idx,"MotifFinMandat"] <- "Elu Senat"

idx <- which(tab.ass[,"MotifFinMandat"]=="Elu au Parlement europeen")
tab.ass[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ass[,"MotifFinMandat"]=="ELECTION COMME REPRESENTANT AU PARLEMENT EUROPEEN")
tab.ass[idx,"MotifFinMandat"] <- "Elu PE"

idx <- which(tab.ass[,"MotifFinMandat"]=="ELECTION A LA PRESIDENCE DE LA REPUBLIQUE")
tab.ass[idx,"MotifFinMandat"] <- "Elu President de la Republique"

idx <- which(tab.ass[,"MotifFinMandat"]=="FIN DE LEGISLATURE")
tab.ass[idx,"MotifFinMandat"] <- "FM"

idx <- which(tab.ass[,"MotifFinMandat"]=="Election dans une autre circonscription (Haute-Garonne)")
tab.ass[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ass[,"MotifFinMandat"]=="Depute de Haute-Garonne")
tab.ass[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ass[,"MotifFinMandat"] == "Nomme a la Commission europeenne")
tab.ass[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.ass[,"MotifFinMandat"] == "Nomme a la Cour des comptes")
tab.ass[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.ass[,"MotifFinMandat"] == "Nomme au Conseil economique et social")
tab.ass[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.ass[,"MotifFinMandat"] == "Nomme au Conseil de la politique monetaire de la banque de France")
tab.ass[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.ass[,"MotifFinMandat"] == "Nomme au Conseil�constitutionnel")
tab.ass[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.ass[,"MotifFinMandat"] == "Nomme mediateur")
tab.ass[idx,"MotifFinMandat"] <- "Nomination"

idx <- which(tab.ass[,"MotifFinMandat"] == "Election partielle")
tab.ass[idx,"MotifFinMandat"] <- "AU"

idx <- which(tab.ass[,"MotifFinMandat"] == "Mission temporaire prolongee")
tab.ass[idx,"MotifFinMandat"] <- "Mission"

idx <- which(tab.ass[,"MotifFinMandat"] == "Cessation de mandat pour mission")
tab.ass[idx,"MotifFinMandat"] <- "Mission"



table(tab.ass[,"MotifFinMandat"])



### CORRECTION SUR LES DATES ###

#Je commence par les dates de naissance

table(tab.ass[,"DateNaissance"])
sort(unique(tab.ass[,"DateNaissance"]))
#Aucune date de naissance aberante
#Nettoyage deja effectue sur la BREF

table(tab.ass[,"DateDebutMandat"])
sort(unique(tab.ass[,"DateDebutMandat"]))
#Aucune date aberrante : toutes les dates sont bien comprises dans la periode etudiee a savoir 58-20
#Nettoyage deja effectue sur la BREF


table(tab.ass[,"DateFinMandat"])
sort(unique(tab.ass[,"DateFinMandat"]))
#Pas de pb
#Les NA sont pour des mandats toujours en cours (en 2020)
#Tous les NA ont commence leur mandat entre 2017 et 2020



# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "Ass_Ve_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.ass, 			# donnees qu'on veut enregistrer
	file=out.file, 		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

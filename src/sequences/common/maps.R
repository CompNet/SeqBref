#########################################################
##### DONNEES BREF  #####
#####
##### Maps de conversion
#####
##### DECEMBRE 2022
#####
#######################################################




#######################################################
# Charge la map de conversion contenue dans le fichier CSV
# specifie en parametre. La fonction renvoie la map.
#
# file: fichier CSV contenant la map requise.
# returns: un vecteur nomme contenant la map.
#######################################################
load.map <- function(file)
{	# on lit le fichier CSV
	data <- read.csv(
		file,
		header=TRUE,
		sep=",",
		fileEncoding="UTF-8",
		check.names=FALSE	# ne pas modifier les noms des colonnes
	)
	
	# on construit la map
	map <- c()
	for(r in 1:nrow(data))
		map[data[r,"oldname"]] <- data[r,"newname"]
	
	# on remplace les chaînes vides de substitution par NA
	map[map==""] <- NA
	
	return(map)
}




#######################################################
# Applique la map specifiee au vecteur de valeurs specifie.
# Ces valeurs sont d'abord normalisees : suppression de la
# casse (transformation en majuscules) et suppression des
# signes diacritiques (accents, cedilles, etc.). Ceci permet
# d'appliquer la map telle quelle. Les valeurs sont alors
# remplacees par leur valeur associee dans la map.
#
# values: les valeurs brutes a convertir.
# map: la map contenant les valeurs associees.
# 
# returns: un vecteur de meme longueur, contenant les
#          valeurs apres conversion.
#######################################################
apply.map <- function(values, map)
{	# on supprime les diacritiques
	values <- fix.encoding(values)
	values <- remove.diacritics(values)
	
	# on supprime la casse
	values <- toupper(values)
	
	# on se concentre sur les valeurs qui ne sont pas NA
	idx.use <- which(!is.na(values))
	
	# on verifie qu'aucune valeur n'echappe a la map
	idx.missed <- which(is.na(match(values[idx.use], names(map))))
	if(length(idx.missed)>0)
		stop("Certaines valeurs specifiees ne sont pas traitees par la map:\n", paste0(sort(unique(values[idx.use][idx.missed])),collapse=", "))
	
	# on applique la map
	values[idx.use] <- map[values[idx.use]]
	return(values)
}

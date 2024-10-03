#######################################################
##### DONNEES BREF Ve REP #####
#####
##### Analyse : turnover
#####
##### FEVRIER 2023
#####
#######################################################

### Combien y a-t-il de nouveaux entrants / anciens membres a chaque legis ?

# initialisation des dates de renouvellement utilisees ensuite
term.starts <- c("09/12/1958", "06/12/1962",
                 "03/04/1967", "11/07/1968",
                 "02/04/1973", "03/04/1978",
                 "02/07/1981", "02/04/1986",
                 "23/06/1988", "28/03/1993",
                 "01/06/1997", "16/06/2002",
                 "17/06/2007", "17/06/2012",
                 "18/06/2017")
term.starts <- as.Date(term.starts, format="%d/%m/%Y")

# on construit la sequence d'annees a traiter
years <- c("1958-12-09", "1962-12-06", 
           "1967-04-03", "1968-07-11",
           "1973-04-02", "1978-04-03",
           "1981-07-02", "1986-04-02",
           "1988-06-23", "1993-03-28",
           "1997-06-01", "2002-06-16",
           "2007-06-17", "2012-06-17",
           "2017-06-18")




#############################################################################################
###############VERSION NUM 2 POUR LE CALCUL DU TURNOVER A L'ASSEMBLEE ###################
#############################################################################################

# data de fin pour le mandat en cours dans les donnees
term.starts <- c(term.starts, as.Date("2022/01/01"))		# on rajoute une date bidon de fin du dernier mandat
idx <- which(is.na(tab.ass[,"DateFinMandat"]))
tab.ass[idx,"DateFinMandat"] <- as.Date("2022/01/01")		# on remplace les dates de fin qui sont NA, pour simplifier le code source

# fonction qui renvoie le numero de legislature sur la base des dates de mandat
get.term.nbr.ass <- function(start.date, end.date)
{	flags <- term.starts[1:(length(term.starts)-1)]<=start.date & term.starts[2:length(term.starts)]>=end.date		# version qui ne marche que si les donnees sont propres
#	flags <- term.starts[1:(length(term.starts)-1)]<=start.date & term.starts[2:length(term.starts)]>=start.date |	# elections encadrent le debut du mandat
#			term.starts[1:(length(term.starts)-1)]<=end.date & term.starts[2:length(term.starts)]>=end.date |		# elections encadrent la fin du mandat
#			term.starts[1:(length(term.starts)-1)]>=start.date & term.starts[2:length(term.starts)]<=end.date		# mandat contient les elections
	result <- which(flags)
	return(result)
}

# on va construire une matrice booleenne contenant les legislatures auxquelles participent chaque elu
# sur les lignes : les elus -- sur les colonnes : les 15 legislatures
# une valeur TRUE a la position (i,j) indiquera que l'elu numero i etait depute durant la legislature numero j
legis.mat <- matrix(FALSE, nrow=length(ids), ncol=length(term.starts)-1)
rownames(legis.mat) <- ids
colnames(legis.mat) <- format(term.starts[-length(term.starts)])

# debug
# tmp <- tab.ass[-(1:nrow(tab.ass)),]

# on fait le decompte
nbr.mndts <- c()											# nombre de mandats distincts pour chaque id
max.consec <- c()											# nombre maximal de mandats consecutifs pour chaque id
nbr.consec.trois <- c()               						# nombre de serie de plus de trois mandats cons�cutifs pour chaque id
for(id in ids)
{	cat("id=",id,"\n",sep="")
	
	# on recupere les mandats de la personne
	mndts <- which(tab.ass[,"Elu_IdIndividu"]==id)
	#print(tab.ass[mndts,])
	
	# on identifie les legislatures sur la base des dates de mandat et des dates d'election
	terms <- sapply(
			# valeurs auxquelles on veut appliquer la fonction (individuellement)
			1:length(mndts),
			# fonction qu'on veut appliquer a ces valeurs
			function(m) get.term.nbr.ass(tab.ass[mndts[m],"DateDebutMandat"], tab.ass[mndts[m],"DateFinMandat"]))
# debug			
#			function(m) 
#			{	res <- get.term.nbr.ass(tab.ass[mndts[m],"DateDebutMandat"], tab.ass[mndts[m],"DateFinMandat"])
#				if(length(res)==0) 
#				{	tmp <<- rbind(tmp,tab.ass[mndts[m],])
#					res <- 1	
#				}
#				return(res)
#			})
	terms <- sort(unique(c(unlist(terms))))
	
	# on met a jour la liste des nombres de mandats distincts
	nbr.mndts <- c(nbr.mndts, length(terms))
	
	# c'est un peu complique a expliquer, mais cette ligne va decouper terms suivant que les valeurs sont consecutives ou pas
	lst <- split(terms, cumsum(c(1, diff(terms) != 1)))
	# on cherche la sequence la plus longue
	longest <- which.max(sapply(lst, length))
	# on met a jour la liste des nombres maximaux de mandats consecutifs
	max.consec <- c(max.consec, length(lst[[longest]]))
	
	# on met a jour la liste des nombres de sequences de plus de trois mandats
	val <- length(which(sapply(lst, length) > 3))
	nbr.consec.trois <- c(nbr.consec.trois, val) 
	
	# on met a jour la matrice des legislatures
	legis.mat[id, terms] <- TRUE
}

# on peut ensuite utiliser la matrice des legislatures pour calculer tes stats
# on initialise une matrice destinee decompter de types d'elus par legislature
cn <- c("Novices", "Nouveaux", "Reelus", "Total")
legis.stats <- matrix(NA, nrow=length(cn), ncol=ncol(legis.mat))
rownames(legis.stats) <- cn
colnames(legis.stats) <- colnames(legis.mat)
# une autre pour decompter les nombre d'elus par nombre de mandats consecutifs, pour chaque legislature
cn <- paste0(1:max(max.consec),"e")
consec.stats <- matrix(NA, nrow=length(cn), ncol=ncol(legis.mat))
rownames(consec.stats) <- cn
colnames(consec.stats) <- colnames(legis.mat)
# une autre pour decompter les nombre d'elus par nombre de mandats consecutifs, pour chaque legislature
cn <- paste0(1:max(nbr.mndts),"e")
cum.stats <- matrix(NA, nrow=length(cn), ncol=ncol(legis.mat))
rownames(cum.stats) <- cn
colnames(cum.stats) <- colnames(legis.mat)
# note : je calcule "nouveau" et "novice" par rapport au debut de la Ve, mais ils ont peut etre eu des mandats avant...
for(i in 1:ncol(legis.stats))
{	cat("Traitement de la legislature n.",i,"\n",sep="")
	
	# nombre total d'elus pour la legislature
	legis.stats["Total",i] <- length(which(legis.mat[,i]))
	# nombre de reelus 
	if(i>1)
		legis.stats["Reelus",i] <- length(which(
				# elu lors de la legislature precedente
				legis.mat[,i-1]
				# ET elu lors de la legislature courante
				& legis.mat[,i]))
	else
		legis.stats["Reelus",i] <- 0

	# nombre de nouveaux
	if(i>2)
		legis.stats["Nouveaux",i] <- length(which(
				# elu lors d'une legislature plus ancienne que la precedente
				sapply(1:length(ids), function(id) any(legis.mat[id,1:(i-2)]))
				# ET PAS elu lors de la legislature precedente
				& !legis.mat[,i-1]
				# ET elu lors de la legislature courante
				& legis.mat[,i]))
	else 
		legis.stats["Nouveaux",i] <- 0
	
	# nombre de novices
	if(i>1)
		legis.stats["Novices",i] <- length(which(
				# jamais elu auparavant
				!sapply(1:length(ids), function(id) any(legis.mat[id,1:(i-1)]))
				# ET elu lors de la legislature courante
				& legis.mat[,i]))
	else
		legis.stats["Novices",i] <- legis.stats["Total",i]
	
	# on complete les matrices des mandats consecutifs
	for(j in 1:min(i,nrow(consec.stats)))
	{	consec.stats[j,i] <- length(which(
			# j mandats consecutifs incluant la legislature courante
			sapply(1:length(ids), function(id) all(legis.mat[id,(i-j+1):i]))
			# ET PAS de mandat a la (j-1)e (sinon on compterait plusieurs fois la meme personne)
			& (if(i<=j) rep(TRUE,length(ids)) else !legis.mat[,i-j]) 
		))
	}

	# on complete les matrices des mandats pas forcement consecutifs
	for(j in 1:min(i,nrow(cum.stats)))
	{	cum.stats[j,i] <- length(which(
			# present dans la legislature courante
			legis.mat[,i]
			# ET a j mandats en incluant la legislature courante
			& sapply(1:length(ids), function(id) length(which(legis.mat[id,1:i]))==j)
		))
	}
}
# on complete la derniere ligne des matrices (pour comparaison/verification)
consec.stats <- rbind(consec.stats, colSums(consec.stats,na.rm=TRUE))
rownames(consec.stats)[nrow(consec.stats)] <- "Total"
cum.stats <- rbind(cum.stats, colSums(cum.stats,na.rm=TRUE))
rownames(cum.stats)[nrow(cum.stats)] <- "Total"

# avec cette matrice de stats, tu peux produire tous tes graphiques, je pense:
print(legis.stats)
# avec celle la, tu as les nombres de mandats consecutifs:
print(consec.stats)
# et avec celle la, les nombres de mandats (pas obligatoirement consecutifs):
print(cum.stats)
#############################################################################################
#############################################################################################



#######################################################
##### DONNEES BREF Ve REP #####
#####
##### Analyse : turnover
#####
##### AVRIL 2023
#####
#######################################################



### Combien y a-t-il de nouveaux entrants / anciens membres a chaque periode ?
# date factice utilisee pour marquer la fin de la derniere legislature
fake.date <- as.Date("2022/01/01")

##########################################################################
#Note sur les dates par series : 
#Serie A : 26/04/1959, 23/09/1962, 26/09/1971, 28/09/1980, 24/09/1989, 27/09/1998, 21/09/2008
#Serie B : 26/04/1959, 26/09/1965, 22/09/1974, 25/09/1983, 27/09/1992, 23/09/2001
#Serie C : 26/04/1959, 22/09/1968, 25/09/1977, 28/09/1986, 24/09/1995, 26/09/2004

#Serie 1 : 25/09/2011, 24/09/2017
# (b+c1)
#Serie 2 : 28/09/2014, 27/09/2020
# (a+c2)

# premiere tentative : periode trop grossieres
#term.starts <- c(
#		"26/04/1959", "22/09/1968", "25/09/1977", "28/09/1986", "24/09/1995", "26/09/2004", "28/09/2014",	# dates unifiees 
#		fake.date																							# date factice pour marquer la fin
#)
#term.starts <- as.Date(term.starts, format="%d/%m/%Y")

# on remplace les NA en date de fin par la date factice
idx <- which(is.na(tab.sen[,"DateFinMandat"]))
tab.sen[idx,"DateFinMandat"] <- fake.date

# chargement des dates d'election par serie 
file <- file.path(data.folder, "Sen_series.csv")
series <- read.csv(
	file,
	header=TRUE,
	sep=",",
	check.names=FALSE	# ne pas modifier les noms des colonnes
)
series[,"Election"] <- as.Date(series[,"Election"], format="%d/%m/%Y")
# on separe les series
series.lst <- list()
series.lst[["a"]] <- series[which(series[,"Serie"]=="a"),"Election"]
series.lst[["b"]] <- series[which(series[,"Serie"]=="b"),"Election"]
series.lst[["c1"]] <- series[which(series[,"Serie"]=="c1"),"Election"]
series.lst[["c2"]] <- series[which(series[,"Serie"]=="c2"),"Election"]
# on rajoute la date factice a la fin
series.lst[["a"]] <- c(series.lst[["a"]], fake.date)
series.lst[["b"]] <- c(series.lst[["b"]], fake.date)
series.lst[["c1"]] <- c(series.lst[["c1"]], fake.date)
series.lst[["c2"]] <- c(series.lst[["c2"]], fake.date)




##########################################################################
# fonction qui renvoie le numero de periode sur la base des dates de mandat
get.term.nbr.sen <- function(start.date, end.date)
{	flags <- term.starts[1:(length(term.starts)-1)]<=start.date & term.starts[2:length(term.starts)]>=end.date		# version qui ne marche que si les donnees sont propres
#	flags <- term.starts[1:(length(term.starts)-1)]<=start.date & term.starts[2:length(term.starts)]>start.date |	# elections encadrent le debut du mandat
#			term.starts[1:(length(term.starts)-1)]<=end.date & term.starts[2:length(term.starts)]>end.date |		# elections encadrent la fin du mandat
#			term.starts[1:(length(term.starts)-1)]>=start.date & term.starts[2:length(term.starts)]<end.date		# mandat contient les elections
	result <- which(flags)
	return(result)
}

# initialisation des listes utilisees pour stocker les resultats
periode.mat.lst <- list()
nbr.mndts.lst <- list()
max.consec.lst <- list()
nbr.consec.trois.lst <- list()
periode.stats.lst <- list()
consec.stats.lst <- list()
cum.stats.lst <- list()

# ce for boucle sur les differentes series
for(s in 1:length(series.lst))
{	sname <- names(series.lst)[s]
	cat("Traitement de la serie '",sname,"'\n",sep="")
	
	# on selectionne uniquement les series concernees
	tab.sen.s <- tab.sen[tab.sen[,"Serie"]==sname,]
	# date correspondant a cette series
	term.starts <- series.lst[[s]]
	# liste des ids concernes
	ids.s <- sort(unique(tab.sen.s[,"Elu_IdIndividu"]))
	
	# on va construire une matrice booleenne contenant les periodes auxquelles participent chaque elu
	# sur les lignes : les elus -- sur les colonnes : les periodes
	# une valeur TRUE a la position (i,j) indiquera que l'elu numero i etait depute durant la periode numero j
	periode.mat <- matrix(FALSE, nrow=length(ids.s), ncol=length(term.starts)-1)
	rownames(periode.mat) <- ids.s
	colnames(periode.mat) <- format(term.starts[-length(term.starts)])
	
# debug
#tmp <- tab.sen.s[-(1:nrow(tab.sen.s)),]
	
	# on fait le decompte
	nbr.mndts <- c()											# nombre de mandats distincts pour chaque id
	max.consec <- c()											# nombre maximal de mandats consecutifs pour chaque id
	nbr.consec.trois <- c()               						# nombre de serie de plus de trois mandats cons�cutifs pour chaque id
	for(id in ids.s)
	{	cat("id=",id,"\n",sep="")
		
		# on recupere les mandats de la personne
		mndts <- which(tab.sen.s[,"Elu_IdIndividu"]==id)
		#print(tab.sen[mndts,])
		
		# on identifie les periodes sur la base des dates de mandat et des dates d'election
		terms <- sapply(
				# valeurs auxquelles on veut appliquer la fonction (individuellement)
				1:length(mndts),
				# fonction qu'on veut appliquer a ces valeurs
				function(m) get.term.nbr.sen(tab.sen.s[mndts[m],"DateDebutMandat"], tab.sen.s[mndts[m],"DateFinMandat"]))
# debug			
#			function(m) 
#			{	res <- get.term.nbr.sen(tab.sen.s[mndts[m],"DateDebutMandat"], tab.sen.s[mndts[m],"DateFinMandat"])
#				if(length(res)!=1) 
#				{	tmp <<- rbind(tmp,tab.sen.s[mndts[m],])
#					res <- 1	
#				}
#				return(res)
#			})
		terms <- sort(unique(c(unlist(terms))))
#print(terms)
		
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
		
		# on met a jour la matrice des periodes
		periode.mat[id, terms] <- TRUE
	}
	
	# on peut ensuite utiliser la matrice des periodes pour calculer tes stats
	# on initialise une matrice destinee decompter de types d'elus par periode
	cn <- c("Novices", "Nouveaux", "Reelus", "Total")
	periode.stats <- matrix(NA, nrow=length(cn), ncol=ncol(periode.mat))
	rownames(periode.stats) <- cn
	colnames(periode.stats) <- colnames(periode.mat)
	# une autre pour decompter les nombre d'elus par nombre de mandats consecutifs, pour chaque periode
	cn <- paste0(1:max(max.consec),"e")
	consec.stats <- matrix(NA, nrow=length(cn), ncol=ncol(periode.mat))
	rownames(consec.stats) <- cn
	colnames(consec.stats) <- colnames(periode.mat)
	# une autre pour decompter les nombre d'elus par nombre de mandats consecutifs, pour chaque periode
	cn <- paste0(1:max(nbr.mndts),"e")
	cum.stats <- matrix(NA, nrow=length(cn), ncol=ncol(periode.mat))
	rownames(cum.stats) <- cn
	colnames(cum.stats) <- colnames(periode.mat)
	# note : je calcule "nouveau" et "novice" par rapport au debut de la Ve, mais ils ont peut etre eu des mandats avant...
	for(i in 1:ncol(periode.stats))
	{	cat("Traitement de la periode n.",i,"\n",sep="")
		
		# nombre total d'elus pour la periode
		periode.stats["Total",i] <- length(which(periode.mat[,i]))
		# nombre de reelus 
		if(i>1)
			periode.stats["Reelus",i] <- length(which(
							# elu lors de la periode precedente
							periode.mat[,i-1]
									# ET elu lors de la periode courante
									& periode.mat[,i]))
		else
			periode.stats["Reelus",i] <- 0
		
		# nombre de nouveaux
		if(i>2)
			periode.stats["Nouveaux",i] <- length(which(
							# elu lors d'une periode plus ancienne que la precedente
							sapply(1:length(ids.s), function(id) any(periode.mat[id,1:(i-2)]))
									# ET PAS elu lors de la periode precedente
									& !periode.mat[,i-1]
									# ET elu lors de la periode courante
									& periode.mat[,i]))
		else 
			periode.stats["Nouveaux",i] <- 0
		
		# nombre de novices
		if(i>1)
			periode.stats["Novices",i] <- length(which(
							# jamais elu auparavant
							!sapply(1:length(ids.s), function(id) any(periode.mat[id,1:(i-1)]))
									# ET elu lors de la periode courante
									& periode.mat[,i]))
		else
			periode.stats["Novices",i] <- periode.stats["Total",i]
		
		# on complete les matrices des mandats consecutifs
		for(j in 1:min(i,nrow(consec.stats)))
		{	consec.stats[j,i] <- length(which(
							# j mandats consecutifs incluant la periode courante
							sapply(1:length(ids.s), function(id) all(periode.mat[id,(i-j+1):i]))
									# ET PAS de mandat a la (j-1)e (sinon on compterait plusieurs fois la meme personne)
									& (if(i<=j) rep(TRUE,length(ids.s)) else !periode.mat[,i-j]) 
					))
		}
		
		# on complete les matrices des mandats pas forcement consecutifs
		for(j in 1:min(i,nrow(cum.stats)))
		{	cum.stats[j,i] <- length(which(
							# present dans la periode courante
						periode.mat[,i]
									# ET a j mandats en incluant la periode courante
									& sapply(1:length(ids.s), function(id) length(which(periode.mat[id,1:i]))==j)
					))
		}
	}
	# on complete la derniere ligne des matrices (pour comparaison/verification)
	consec.stats <- rbind(consec.stats, colSums(consec.stats,na.rm=TRUE))
	rownames(consec.stats)[nrow(consec.stats)] <- "Total"
	cum.stats <- rbind(cum.stats, colSums(cum.stats,na.rm=TRUE))
	rownames(cum.stats)[nrow(cum.stats)] <- "Total"
	
	# on met les matrices dans les listes, pour plus tard
	periode.mat.lst[[sname]] <- periode.mat
	nbr.mndts.lst[[sname]] <- nbr.mndts
	max.consec.lst[[sname]] <- max.consec
	nbr.consec.trois.lst[[sname]] <- nbr.consec.trois
	periode.stats.lst[[sname]] <- periode.stats
	consec.stats.lst[[sname]] <- consec.stats
	cum.stats.lst[[sname]] <- cum.stats
	
	# avec cette matrice de stats, tu peux produire tous tes graphiques, je pense:
	print(periode.stats)
	# avec celle la, tu as les nombres de mandats consecutifs:
	print(consec.stats)
	# et avec celle la, les nombres de mandats (pas obligatoirement consecutifs):
	print(cum.stats)
}

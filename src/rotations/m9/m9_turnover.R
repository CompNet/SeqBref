#######################################################
##### DONNEES BREF Ve REP #####
#####
##### Analyse : turnover Maire villes +9k
#####
##### MARS/AVRIL 2023
#####
#######################################################

### Combien y a-t-il de nouveaux entrants / anciens membres a chaque renouvellement de la fonction ?

# initialisation des dates de renouvellement utilisees ensuite
term.starts <- c('26/04/1953','08/03/1959','14/03/1965',
                 '14/03/1971','13/03/1977', '06/03/1983', 
                 '12/03/1989','11/06/1995', '11/03/2001',
                 '09/03/2008','23/03/2014','28/06/2020')
term.starts <- as.Date(term.starts, format="%d/%m/%Y")

# on construit la sequence d'annees a traiter
years <- c('1953-04-26','1959-03-08', '1965-03-14', '1971-03-14',
           '1977-03-13', '1983-03-06', '1989-03-12',
           '1995-06-11', '2001-03-11','2008-03-09',
           '2014-03-23','2020-06-28')


#Je n'ai retenu que les dates du premier tour, sauf 2020 2nd tour avec delai COVID
#la distinction n'est pas retenue entre election au 1er ou 2nd dans m9



###############################################################################
###############################################################################




# data de fin pour le mandat en cours dans les donnees
term.starts <- c(term.starts, as.Date("2022/01/01"))		# on rajoute une date bidon de fin du dernier mandat
idx <- which(is.na(tab.m9[,"DateFinMandat"]))
tab.m9[idx,"DateFinMandat"] <- as.Date("2022/01/01")		# on remplace les dates de fin qui sont NA, pour simplifier le code source


#J'identifie de maniere unique chaque elu dans un objet
ids <- sort(unique(tab.m9[,"Id"]))


# fonction qui renvoie le numero de mandature sur la base des dates de mandat
get.term.nbr <- function(start.date, end.date)
{	flags <- term.starts[1:(length(term.starts)-1)]<=start.date & term.starts[2:length(term.starts)]>=end.date		# version qui ne marche que si les donnees sont propres
#	flags <- term.starts[1:(length(term.starts)-1)]<=start.date & term.starts[2:length(term.starts)]>=start.date |	# elections encadrent le debut du mandat
#			term.starts[1:(length(term.starts)-1)]<=end.date & term.starts[2:length(term.starts)]>=end.date |		# elections encadre la fin du mandat
#			term.starts[1:(length(term.starts)-1)]>=start.date & term.starts[2:length(term.starts)]<=end.date		# mandat contient les elections
result <- which(flags)
return(result)
}

# on va construire une matrice booleenne contenant les renouvellements auxquels participent chaque elu
# sur les lignes : les elus -- sur les colonnes : les 12 mandatures
# une valeur TRUE a la position (i,j) indiquera que l'elu numero i etait maire durant la mandature numero j
legis.mat <- matrix(FALSE, nrow=length(ids), ncol=length(term.starts)-1)
rownames(legis.mat) <- ids
colnames(legis.mat) <- format(term.starts[-length(term.starts)])


# on fait le decompte
nbr.mndts <- c()											# nombre de mandats distincts pour chaque id
max.consec <- c()											# nombre maximal de mandats consecutifs pour chaque id
nbr.consec.trois <- c()               						# nombre de serie de plus de trois mandats cons�cutifs pour chaque id

for(id in ids)
{	cat("id=",id,"\n",sep="")
  
  # on recupere les mandats de la personne
  mndts <- which(tab.m9[,"Id"]==id)
  #print(tab.m9[mndts,])
  
  # on identifie les legislatures sur la base des dates de mandat et des dates d'election
  terms <- sapply(
    # valeurs auxquelles on veut appliquer la fonction (individuellement)
    1:length(mndts),
    # fonction qu'on veut appliquer a ces valeurs
    function(m) get.term.nbr(tab.m9[mndts[m],"DateDebutMandat"], tab.m9[mndts[m],"DateFinMandat"]))

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





#Ici, j'initialise trois nouvelles matrices pour contenir les decomptes a realiser
# on initialise une matrice destinee decompter de types d'elus par legislature

cn <- c("Novices", "Revenants", "Reelus", "Total")
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
    legis.stats["Revenants",i] <- length(which(
      # elu lors d'une legislature plus ancienne que la precedente
      sapply(1:length(ids), function(id) any(legis.mat[id,1:(i-2)]))
      # ET PAS elu lors de la legislature precedente
      & !legis.mat[,i-1]
      # ET elu lors de la legislature courante
      & legis.mat[,i]))
  else 
    legis.stats["Revenants",i] <- 0
  
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


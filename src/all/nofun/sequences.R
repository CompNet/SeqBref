#######################################################
##### DONNEES FUSION Ve REP #####
#####
##### Analyse de sequences : Typologie 1 - Fonctions ignorees
#####
##### DECEMBRE 2022
#####
#######################################################





#######################################################
# Conversion
#######################################################

## conversion des donnees en sequences
#sd0 <- convert.to.sequences(
#	# table contenant les donnees
#	tab.mandates=tab.all,
#	# date de debut de la periode a couvrir, au format annee/mois/jour
#	start.date=as.Date("1925/01/01"),
#	# date de fin de cette periode, au meme format
#	end.date=as.Date("2023/01/01"),
#	# option permettant de ne *pas* aligner les sequences a gauche
#	left=NA,
#	# couleurs (TRUE) vs niveaux de gris (FALSE)
#	colors=TRUE
#)
#summary(sd0)
#
## conversion des donnees en sequences, version a-historique
#sd.left0 <- convert.to.sequences(
#	# table contenant les donnees
#	tab.mandates=tab.all,
#	# date de debut de la periode a couvrir, au format annee/mois/jour
#	start.date=as.Date("1925/01/01"),
#	# date de fin de cette periode, au meme format
#	end.date=as.Date("2023/01/01"),
#	# option permettant d'aligner les sequences a gauche
#	left="DEL",
#	# couleurs (TRUE) vs niveaux de gris (FALSE)
#	colors=TRUE
#)
#summary(sd.left0)

#####

# utilisation des nouvelles fonctions de conversion
sd <- convert.to.sequences.fun(
	# table contenant les donnees
	tab.mandates=tab.all,
	# date de debut de la periode a couvrir, au format annee/mois/jour
	start.date=as.Date("1925/01/01"),
	# date de fin de cette periode, au meme format
	end.date=as.Date("2023/01/01"),
	# option permettant de ne *pas* aligner les sequences a gauche
	left=NA,
	# couleurs (TRUE) vs niveaux de gris (FALSE)
	colors=TRUE,
	# mandats/fonctions a ignorer
	ignored.mandates=NOFUN_IGNORED_MDT, 
	# comment gerer les cumuls
	joint.mandates=NOFUN_JOINT_MDT,
	# ordre des mandats finaux
	mandate.names=NOFUN_ORDER_MDT
)
# verification qu'on a bien exactement les memes sequences qu'avec l'ancienne fonction
#print(all(sd0==sd))

# utilisation des nouvelles fonctions de conversion
sd.left <- convert.to.sequences.fun(
	# table contenant les donnees
	tab.mandates=tab.all,
	# date de debut de la periode a couvrir, au format annee/mois/jour
	start.date=as.Date("1925/01/01"),
	# date de fin de cette periode, au meme format
	end.date=as.Date("2023/01/01"),
	# option permettant d'aligner les sequences a gauche
	left="DEL",
	# couleurs (TRUE) vs niveaux de gris (FALSE)
	colors=TRUE,
	# mandats/fonctions a ignorer
	ignored.mandates=NOFUN_IGNORED_MDT, 
	# comment gerer les cumuls
	joint.mandates=NOFUN_JOINT_MDT,
	# ordre des mandats finaux
	mandate.names=NOFUN_ORDER_MDT
)
# verification qu'on a bien exactement les memes sequences qu'avec l'ancienne fonction
#print(all(sd.left0==sd.left))





#######################################################
# Tapis de sequences
#######################################################

# legende
png(file.path(plots.folder, "sequences", "nofun", "legende.png"))
seqlegend(sd)
dev.off()

pdf(file.path(plots.folder, "sequences", "nofun", "legende.pdf"))
seqlegend(sd)
dev.off()

png(file.path(plots.folder, "sequences", "nofun", "seqIplot.png"), width=1024)
#  on cree le graphique, mais sans l'axe x
seqIplot(
	sd, 
	sortv="from.start", 
	ylas=2,
	with.legend=FALSE,
	xlab=NA,							# n'affiche pas le nom de l'axe x
	xtlab=FALSE							# n'affiche pas les valeurs sur l'axe x
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
	1,									# 1 pour x et 2 pour y 
	at=1:ncol(sd), 						# position des labels sur l'axe
	labels=substr(colnames(sd),1,4), 	# texte des labels : seulement l'annee
	las=2								# rotation de 90 du texte pour qu'il soit vertical (plus facile a lire)
)
dev.off()

png(file.path(plots.folder, "sequences", "nofun", "seqIplot_ahisto.png"), width=1024)
#  on cree le graphique, mais sans l'axe x
seqIplot(
	sd.left, 
	sortv="from.start", 
	ylas=2,
	with.legend=FALSE,
	xlab=NA,							# n'affiche pas le nom de l'axe x
	xtlab=FALSE							# n'affiche pas les valeurs sur l'axe x
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
	1,									# 1 pour x et 2 pour y 
	at=1:ncol(sd), 						# position des labels sur l'axe
	labels=1:ncol(sd), 					# texte des labels : numero des etats
)
dev.off()




########################################################
### Distribution des etats
########################################################

# Distribution de tous les etats sur toute la periode, en frequence : 
pdf(file.path(plots.folder, "sequences", "nofun", "seqdplot.pdf"), width=25, height=12)
#png(file.path(plots.folder, "sequences", "nofun", "seqdplot.png"), width=1024, height=480)
seqdplot(
	sd,						# donnees a representer
	border=NA,				# avec ou sans bordure autour des barres verticales
	with.legend=FALSE,		# avec ou sans legende
#	with.missing=TRUE		# afficher ou pas les etats manquants
)
count.states(sd=sd, file.name=file.path(plots.folder, "sequences", "nofun", "seqdplot"))
dev.off()


#creation d'un seqdplot dans sa version ahistorique : 
pdf(file.path(plots.folder, "sequences", "nofun", "seqdplot2.pdf"), width=25, height=12)
seqdplot(
	sd.left,				# donnees a representer
  	border=NA,				# avec ou sans bordure autour des barres verticales
	with.legend=FALSE,		# avec ou sans legende
#	with.missing=TRUE		# afficher ou pas les etats manquants
xlab=NA,							# n'affiche pas le nom de l'axe x
xtlab=FALSE							# n'affiche pas les valeurs sur l'axe x
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
  1,									# 1 pour x et 2 pour y 
  at=1:ncol(sd), 						# position des labels sur l'axe
  labels=1:ncol(sd), 					# texte des labels : numero des etats
)
count.states(sd=sd.left, file.name=file.path(plots.folder, "sequences", "nofun", "seqdplot2"))
dev.off()


# Afficher les valeurs des frequences du plot precedent, annee par annee: 
seqstatd(sd)




########################################################
### Quelques premieres sequences
########################################################

# Les 10 premieres sequences visibles dans mon objet : 
png(file.path(plots.folder, "sequences", "nofun", "10_first_freq.png"))
seqiplot(
	sd,
	with.legend=FALSE,
	with.missing=FALSE
)
dev.off()

# Idem dans la version a-historique :
png(file.path(plots.folder, "sequences", "nofun", "10_first_freq_ahisto.png"))
seqiplot(
	sd.left,
	with.legend=FALSE,
	with.missing=FALSE
)
dev.off()




########################################################
### Sequences les plus frequentes
########################################################

# Sequences les plus frequentes : 
png(file.path(plots.folder, "sequences", "nofun", "seq_more_freq.png"))
seqfplot(
	sd,
	with.legend=FALSE,
	with.missing=FALSE
)
dev.off()
# cela permet de les calculer et de les exporter sous forme de csv
freqs <- seqtab(
	sd, 
	idxs=0, 	# on veut toutes les sequences (possibilite de ne garder que les 10 premieres, ou autres)
	weighted=TRUE
)
write.table(
	x=attr(freqs,"freq"),
	file=file.path(plots.folder, "sequences", "nofun", "seq_more_freq.csv"),
	quote=FALSE,
	sep=",",
	fileEncoding="UTF-8",
	row.names=TRUE,
	col.names=TRUE
)

# Idem, missing du debut a retirer, donc version "a-historique" : 
png(file.path(plots.folder, "sequences", "nofun", "seq_more_freq_ahisto.png"))
seqfplot(
	sd.left,
	with.legend=FALSE,
	with.missing=FALSE
)
dev.off()
# cela permet de les calculer et de les exporter sous forme de csv
freqs <- seqtab(
	sd, 
	idxs=0, 	# on veut toutes les sequences (possibilite de ne garder que les 10 premieres, ou autres)
	weighted=TRUE
)
write.table(
	x=attr(freqs,"freq"),
	file=file.path(plots.folder, "sequences", "nofun", "seq_more_freq_ahisto.csv"),
	quote=FALSE,
	sep=",",
	fileEncoding="UTF-8",
	row.names=TRUE,
	col.names=TRUE
)




########################################################
### Stats sur les transitions
########################################################

#Les taux de transition / passage d'un etat a l'autre :
trans <- seqtrate(sd)
round(trans, 2)
# plus proche de 1, plus la probabilite est importante
# peut m'aider a definir les couts pour l'Optimal Matching ??

# Verification (je ne devrais avoir que 1 partout) :
rowSums(trans)
# Verification ok.

# on exporte la matrice de transition sous forme d'image, avec un code couleur pour les valeurs
compute.transition.rates(
	sd=sd,																# sequences a representer 
	file.name=file.path(plots.folder, "sequences", "nofun", "all_")		# nom du fichier
)
# meme chose pour le graphe de transitions
plot.transition.graph(
	sd=sd,																# sequences a representer 
	file.name=file.path(plots.folder, "sequences", "nofun", "all_")		# nom du fichier
)

# pas la peine de le faire pour ahisto : ca sera exactement pareil, ce parametre n'a aucun effet ici



##############################################
##### VISUALISER LES SEQUENCES ##############
#############################################


# Visualiser toutes les sequences avec les successions d'etats codes : 
seqdef(sd)

# Visualiser les sequences au format SPS : 
#Ex ligne 1 : (NA,51) - (RE, 1) - (CuV, 13)
print(sd, "SPS")

# Je peux a partir du format SPS compter la liste des etats uniques dans la sequence : 
#(enleve les NA)
seqdss(sd)

# Puis je peux compter le nombre de transition dans chaque sequence : 
sd.dss <- seqdss(sd)
seqlength(sd.dss)

# Le temps passe dans chaque etat pour chacune des sequences : 
stats <- seqistatd(
	sd, 
	with.missing=TRUE
)
print(stats)
# on calcule quelques stats la dessus : temps moyen, ecart-type, median
tab.stats <- t(apply(stats, 2, function(col) c("Total"=sum(col),"Mean"=mean(col), "St-dv"=sd(col), "Median"=median(col))))
cat("Statistiques sur le temps passe dans chaque etat :\n")
print(tab.stats)
# on peut enregistrer ça sous forme de fichier CSV
write.table(
	x=tab.stats,
	file=file.path(plots.folder, "sequences", "nofun", "stats_time_by_state.csv"),
	quote=FALSE,
	sep=",",
	fileEncoding="UTF-8",
	row.names=TRUE,
	col.names=TRUE
)
# et on peut creer un histogramme, en procedant comme tu l'as deja fait pour la Californie
png(file.path(plots.folder, "sequences", "nofun", "stats_time_by_state_miss=TRUE.png"))
barplot(tab.stats[,"Total"], col=c(attr(sd,"cpal"),attr(sd,"missing.color")))
dev.off()
# la meme sans les etats inconnus
png(file.path(plots.folder, "sequences", "nofun", "stats_time_by_state_miss=FALSE.png"))
barplot(tab.stats[rownames(tab.stats)!="*","Total"], col=attr(sd,"cpal"))
dev.off()


#Le temps moyen passe dans chaque etat : 
seqmtplot(sd,
           with.legend = FALSE,
           )

# Calculer l'entropie : plus proche de 1, plus la sequence contient des etats divers / plus proche 0 moins il y a d'etats differents : 
sd.ent <- seqient(sd)
summary(sd.ent)

# Repartition de l'entropie en histogramme : 
png(file.path(plots.folder, "sequences", "nofun", "entropy.png"))
hist(sd.ent,
	col="orange",
	main=NULL,
	xlab="Entropy")
dev.off()

# Je recherche qui a la plus forte entropie : 
index <- which(sd.ent== max(sd.ent))
# ligne 186 = PETEL ANNE LAURENCE
sd[sd.ent==max(sd.ent),] #Selon le guide TraMineR, cette commande fonctionne un peu mysterieusement
# = Jean Luc Melenchon

# Je calcule l'entropie selon le sexe : 
# Je dois utiliser boxplot



############################
##### Optimal Matching #####
############################

# Pour un premier essai, je calcule tous les couts de maniere constante (=2)
couts <- seqsubm(
  sd,
  method="CONSTANT",
  cval=2,
  with.missing=TRUE
)
# Je fixe ensuite les matrices de distance / dissimilarites en repartant de mon cout et en attribuant un cout de 1 aux indel:
seq.om <- seqdist(
  sd,
  method="OM",
  indel=1,
  sm=couts,
  with.missing=TRUE
)
seq.om

# Je refais ces 2 etapes mais en attribuant des couts differents
couts <- seqsubm(
  sd,
  method="TRATE",
  with.missing=TRUE
)

seq.om <- seqdist(
  sd,
  method="OM",
  indel=1,
  sm=couts,
  with.missing=TRUE
)
seq.om

# NB : j'ai tout mis au hasard pour tester, pas de reflexion sur les couts et substitution



# Enfin, je cree les classes / groupes de regroupement pour permettre une representation visuelle des matrices de distance: 
seq.dist <- hclust(
  as.dist(seq.om),
  method="ward.D2"
)


png(file.path(plots.folder, "sequences", "nofun", "dendrogram.png"))
plot(as.dendrogram(seq.dist), leaflab="none")
abline(	# trace une ligne droite (horizontale ici, mais elle peut etre quelconque)
  #	a=...,		# coefficient directeur (pour les lignes ni horizontales ni verticales)
  #	b=...,		# ordonnee a l'origine (ni horizontal ni vertical)
  h=305,		# hauteur, si c'est une ligne horizontale	
  #	v=...,		# position, si c'est une ligne verticale
  col="BLUE",	# couleur de la ligne
  lty=2		# type de ligne (trait plein, pointilles, etc. voir : http://www.sthda.com/english/wiki/line-types-in-r-lty
)
dev.off()
# Je trace une ligne verticale legermenet au dessus de 175 et coupe ainsi 8 fois une ligne verticale
#Une autre au dessus de 250 pour couper 4 fois
# note de VL : tu peux carremment tracer la ligne dans le graphique, je t'ai rajoute l'instruction ci-dessus

# Une autre methode me permettant de decider du nb de classes est le mix dendrogramme + index plot
# Je commence par creer ma propre fonction "seq_heatmap"

seq_heatmap <- function (seq, tree, with.missing=FALSE, ...) {
  if (class(tree)!="dendrogram") tree <- as.dendrogram(tree)
  mat <- seq
  for (i in 1:length(seq)){
    mat[mat[,i]=="%",i] <- NA
    mat[,i] <- as.numeric(mat[,i])
  }
  mat <- as.matrix(mat)
  col <- attr(seq,"cpal")
  if(with.missing) col <- c(col,attr(seq,"missing.color"))
  heatmap(mat, tree, NA,  na.rm=FALSE, col=col, scale="none", labRow=NA, ...)	
}

# Puis j'applique la fonction nouvellement cree dans une version historique
# Et egalement dans une version a-historique : 
# Je reprends ma fonction et lui applique le tableau de sequence sd + le dendrogramme obtenu avec hclust (seq.dist)

png(file.path(plots.folder, "sequences", "nofun", "tapis_dendro.png"))
seq_heatmap(sd, seq.dist)
dev.off()

# Idem pour la version a-historique
png(file.path(plots.folder, "sequences", "nofun", "tapis_dendro_a-histo.png"))
seq_heatmap(sd.left, seq.dist)
dev.off()

# PB : je n'ai plus les memes couleurs qu'utilisees dans la legende... 

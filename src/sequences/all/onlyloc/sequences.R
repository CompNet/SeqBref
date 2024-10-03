#######################################################
##### DONNEES FUSION Ve REP #####
#####
##### Analyse de sequences : Typologie 3 : local uniquement
#####
##### FEVRIER 2023
#####
#######################################################




#######################################################
# Filtrage des mandats
#######################################################

# liste des villes de plus de 9000 habitants
#selected.cities <- c(
		#"BOURG EN BRESSE", "AMBERIEU EN BUGEY", "BELLEY", "DIVONNE LES BAINS", 
		#"FFERNEY VOLTAIRE", "GEX", "MIRIBEL", "OYONNAX", "SAINT GENIS POUILLY", 
		#"DIJON", "BEAUNE", "CHENOVE", "CHEVIGNY SAINT SAUVEUR", "FONTAINE LES DIJON", 
		#"LONGVIC", "QUETIGNY", "TALANT", "SAINT DENIS", "MONTREUIL", "AUBERVILLIERS", 
		#"NOISY LE GRAND", "PANTIN", "BONDY", "SEVRAN", "LA COURNEUVE", "ROMAINVILLE", 
		#"CLICHY SOUS BOIS", "LE BOURGET", "RAINCY", "MARSEILLE", "AIX EN PROVENCE", 
		#"ARLES", "LA CIOTAT", "MARIGNANE", "PENNES MIRABEAUN", "PORT DE BOUC", "TARASCON", 
		#"BOUC BEL AIR", "BERRE L ETANG", "SAINT MARTIN DE CRAU", "CABRIES"
#)

# parmi les mandats , on garde uniquement ceux concernant les M de ces villes
#idx <- which(								# numero des lignes de la table concernant ces maires-la
#	tab.all[,"TypeTerritoire"]=="Commune" & tab.all[,"NomTerritoire"] %in% selected.cities
#	& !is.na(tab.all[,"TypeFonction"]) & tab.all[,"TypeFonction"]=="M")
#ids <- sort(unique(tab.all[idx,"Id"]))		# on recupere les ids de ces personnes
#keep <- which(tab.all[,"Id"] %in% ids)		# on identifie toutes leurs lignes (mandats) dans la table 
#tab.all.filt <- tab.all[keep,]				# on ne garde que ces lignes-la

#cat("Nombre de mandats total : ",nrow(tab.all),"\n",sep="")
#cat("Nombre de mandats apres filtrage (maire des villes de >9000 habs) : ",nrow(tab.all.filt),"\n",sep="")


#Sans filtrage : 
#je remplace tab.all.filt par tab.all dans les scripts qui suivent (et inversement si je veux filtrer)



#######################################################
# Conversion
#######################################################

## conversion des donnees en sequences, version historique
sd <- convert.to.sequences.fun(
	# table contenant les donnees (version filtree)
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
	ignored.mandates=ONLYLOC_IGNORED_MDT, 
	# comment gerer les cumuls
	joint.mandates=ONLYLOC_JOINT_MDT,
	# ordre des mandats finaux
	mandate.names=ONLYLOC_ORDER_MDT
)
summary(sd)

## conversion des donnees en sequences, version a-historique
sd.left <- convert.to.sequences.fun(
	# table contenant les donnees (version filtree)
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
	ignored.mandates=ONLYLOC_IGNORED_MDT, 
	# comment gerer les cumuls
	joint.mandates=ONLYLOC_JOINT_MDT,
	# ordre des mandats finaux
	mandate.names=ONLYLOC_ORDER_MDT
)
summary(sd.left)





#######################################################
# Tapis de sequences
#######################################################

# legende
pdf(file.path(plots.folder, "sequences", "onlyloc", "legende.pdf"))
seqlegend(sd)
dev.off()

png(file.path(plots.folder, "sequences", "onlyloc", "seqIplot.png"), width=1024)
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

png(file.path(plots.folder, "sequences", "onlyloc", "seqIplot_ahisto.png"), width=1024)
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
png(file.path(plots.folder, "sequences", "onlyloc", "seqdplot.png"), width=480, height=480)
#png(file.path(plots.folder, "sequences", "nofun", "seqdplot.png"), width=1024, height=480)
seqdplot(
  sd,						# donnees a representer
  border=NA,				# avec ou sans bordure autour des barres verticales
  with.legend=FALSE,		# avec ou sans legende
  #	with.missing=TRUE		# afficher ou pas les etats manquants
)
dev.off()
count.states(sd=sd, file.name=file.path(plots.folder, "sequences", "onlyloc", "seqdplot"))

#creation d'un seqdplot dans sa version ahistorique : 
png(file.path(plots.folder, "sequences", "onlyloc", "seqdplot2.png"), width=1200, height=480)
seqdplot(
  sd.left,				# donnees a representer
  border=NA,				# avec ou sans bordure autour des barres verticales
  with.legend=FALSE,
  # avec ou sans legende
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
dev.off()

count.states(sd=sd.left, file.name=file.path(plots.folder, "sequences", "onlyloc", "seqdplot2"))




########
###TEMPS MOYEN DANS CHAQUE ETAT 
########

png(file.path(plots.folder, "sequences", "onlyloc", "seqmtplot.png"))
seqmtplot(
  sd,						# donnees a representer
  border=NA,				# avec ou sans bordure autour des barres verticales
  with.legend=FALSE,		# avec ou sans legende
  ylim=c(0,15)
  #	with.missing=TRUE		# afficher ou pas les etats manquants
)
dev.off()




########################################################
### Sequences les plus frequentes
########################################################

# Sequences les plus frequentes : 
png(file.path(plots.folder, "sequences", "onlyloc", "seq_more_freq.png"))
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
  file=file.path(plots.folder, "sequences", "onlyloc", "seq_more_freq.csv"),
  quote=FALSE,
  sep=",",
  fileEncoding="UTF-8",
  row.names=TRUE,
  col.names=TRUE
)

# Idem, missing du debut a retirer, donc version "a-historique" : 
png(file.path(plots.folder, "sequences", "onlyloc", "seq_more_freq_ahisto.png"))
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
  file=file.path(plots.folder, "sequences", "onlyloc", "seq_more_freq_ahisto.csv"),
  quote=FALSE,
  sep=",",
  fileEncoding="UTF-8",
  row.names=TRUE,
  col.names=TRUE
)






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


png(file.path(plots.folder, "sequences", "onlyloc", "dendrogram.png"))
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






#################################################
########### PARTITION EN 5 CLASSES ##############
################################################



# D'abord, je coupe en 5 classes
nbcl <- 5
seq.part5 <- cutree(seq.dist, nbcl)
seq.part5 <- factor(
  seq.part5,
  labels=c("1.carri�res courtes", "2.mono-maire", "3.local vers maire (d�but Ve)","4. Local sans maire", "5.local vers maire (fin Ve)" )
)

# 1ere tentative de typologie - sequences en tapis :
png(file.path(plots.folder, "sequences", "onlyloc", "Typologie_5.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd,
  group=seq.part5,
  border=NA,
)
dev.off()

png(file.path(plots.folder, "sequences", "onlyloc", "sequences_classes_5.png"))
seqIplot(
  sd,
  group=seq.part5,
)
dev.off()


# Je peux ensuite reproduire ce tapis de sequences en triant les sequences
# Je trie par multidimensional scaling
# Permet d'apporter plus de lisibilite : 

ordre <- cmdscale(as.dist(seq.om), k=1)

png(file.path(plots.folder, "sequences", "onlyloc", "sequences_classes_cmdscale_5.png"))
seqIplot(
  sd,
  group=seq.part5,
  sortv=ordre,
)
dev.off()

# Trop de NA... Il faudrait aussi pouvoir creer le seqIplot non pas par date de debut de la periode mais date de debut du 1er mandat de la sequence
# J'ajoute donc la mm version de ces tapis (typo et tapis de sequences) en utilisant la version "a-historique" des sequences
# J'utilise donc "sd.left", j'aligne les sequences non pas sur une DATE mais sur un EVENEMENT
# Cet evenement est le premier mandat connu pour chacune des sequences

png(file.path(plots.folder, "sequences", "onlyloc", "Typologie_a-histo_5.png"), width = 1024)	# cree un fichier contenant le graphique
seqdplot(
  sd.left,
  group=seq.part5,
  border=NA,
  xlab=NA,
  xtlab=FALSE
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
  1,									# 1 pour x et 2 pour y 
  at=1:ncol(sd), 						# position des labels sur l'axe
  labels=1:ncol(sd), 					# texte des labels : numero des etats
)
dev.off()




png(file.path(plots.folder, "sequences", "onlyloc", "sequences_classes_a-histo_5.png"))
seqIplot(
  sd.left,
  group=seq.part5,
  sortv="from.start",
  ylas=2,
  with.legend=FALSE,
  xlab=NA,
  xtlab=FALSE
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
  1,									# 1 pour x et 2 pour y 
  at=1:ncol(sd), 						# position des labels sur l'axe
  labels=1:ncol(sd), 					# texte des labels : numero des etats
)
dev.off()



# Autre visualisation : la repartition des Etats par classes
# Je pense que la repartition est la meme qu'on soit en version DATE ou version AHISTORIQUE
# Oui, idem. 

png(file.path(plots.folder, "sequences", "onlyloc", "repartition_etats_classes_5.png"))
seqmtplot(sd,
          group=seq.part5,
          border=NA)
dev.off()

png(file.path(plots.folder, "sequences", "onlyloc", "repartition_etats_classes_a-histo_5.png"))
seqmtplot(
  sd.left,
  group=seq.part5,
  border=NA,
  ylim=c(0,25))
dev.off()



########### MEDOIDES ###########

#Cette methode permet de trouver l'element d'un groupe qui est le plus proche des autres en moyenne
#J'utilise la fonction seqrplot

png(file.path(plots.folder, "sequences", "onlyloc", "medoidesmultiples_5.png"))
seqrplot(
  sd, 						# objet contenant les sequences
  group=seq.part5, 			# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,				# matrice de dissimilaritï¿½s, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
)
dev.off()


medoid.ids <- disscenter(as.dist(seq.om), group=seq.part5, medoids.index="first")
print(medoid.ids)


#Je cherche maintenant a n'avoir qu'un seul parangon par classe : 

png(file.path(plots.folder, "sequences", "onlyloc", "medoidesimple_5.png"))
seqrplot(
  sd, 						# objet contenant les sequences
  group=seq.part5, 			# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,				# matrice de dissimilaritï¿½s, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
  nrep=1					# si on veut forcer a n'avoir qu'un seul medoide
)
dev.off()



#######################################################
##### DONNEES FUSION Ve REP #####
#####
##### Analyse de sequences : Typologie 2 : local & national
#####
##### FEVRIER 2023
#####
#######################################################




#######################################################
# Filtrage des mandats
#######################################################

# parmi les mandats , on garde uniquement ceux qui ont ete D ou S apres 1958

#idx <- which(								# numero des lignes de la table concernant ces maires-la
#		(tab.all[,"TypeMandat"]=="SENATEUR" | tab.all[,"TypeMandat"]=="DEPUTE")
#				& (is.na(tab.all[,"DateFinMandat"]) | tab.all[,"DateFinMandat"]>=as.Date("1958-01-01")))
#ids <- sort(unique(tab.all[idx,"Id"]))		# on recupere les ids de ces personnes
#keep <- which(tab.all[,"Id"] %in% ids)		# on identifie toutes leurs lignes (mandats) dans la table 
#tab.all.filt <- tab.all[keep,]				# on ne garde que ces lignes-la

#cat("Nombre de mandats total : ",nrow(tab.all),"\n",sep="")
#cat("Nombre de mandats apres filtrage (parlementaires) : ",nrow(tab.all.filt),"\n",sep="")





#######################################################
# Conversion
#######################################################

## conversion des donnees en sequences, version historique
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
	ignored.mandates=LOCNAT_IGNORED_MDT, 
	# comment gerer les cumuls
	joint.mandates=LOCNAT_JOINT_MDT,
	# ordre des mandats finaux
	mandate.names=LOCNAT_ORDER_MDT
)
summary(sd)

## conversion des donnees en sequences, version a-historique
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
	ignored.mandates=LOCNAT_IGNORED_MDT, 
	# comment gerer les cumuls
	joint.mandates=LOCNAT_JOINT_MDT,
	# ordre des mandats finaux
	mandate.names=LOCNAT_ORDER_MDT
)
summary(sd.left)





#######################################################
# Tapis de sequences
#######################################################

# legende
png(file.path(plots.folder, "sequences", "locnat", "legende.png"))
seqlegend(sd)
dev.off()

png(file.path(plots.folder, "sequences", "locnat", "seqIplot.png"), width=1024)
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

png(file.path(plots.folder, "sequences", "locnat", "seqIplot_ahisto.png"), width=1024)
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
png(file.path(plots.folder, "sequences", "locnat", "seqdplot.png"), width=480, height=480)
#png(file.path(plots.folder, "sequences", "nofun", "seqdplot.png"), width=1024, height=480)
seqdplot(
  sd,						# donnees a representer
  border=NA,				# avec ou sans bordure autour des barres verticales
  with.legend=FALSE,		# avec ou sans legende
  #	with.missing=TRUE		# afficher ou pas les etats manquants
)
dev.off()
count.states(sd=sd, file.name=file.path(plots.folder, "sequences", "locnat", "seqdplot"))

#creation d'un seqdplot dans sa version ahistorique : 
png(file.path(plots.folder, "sequences", "locnat", "seqdplot2.png"), width=1200, height=480)
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

count.states(sd=sd.left, file.name=file.path(plots.folder, "sequences", "locnat", "seqdplot2"))





############################
##### Optimal Matching #####
############################


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


png(file.path(plots.folder, "sequences", "locnat", "transition.png"))
plot.transition.graph(
  sd=sd, # sequences a representer ,
  file.name=file.path(plots.folder, "sequences", "locnat", "all_")		# nom du fichier
)
dev.off()



# Enfin, je cree les classes / groupes de regroupement pour permettre une representation visuelle des matrices de distance: 
seq.dist <- hclust(
  as.dist(seq.om),
  method="ward.D2"
)


png(file.path(plots.folder, "sequences", "locnat", "dendrogram.png"))
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
  labels=c("1.carrières occasionnelles","2.divers", "3.local vers maire", "4.maire vers sénat","5.local vers député" )
)


###tapis de sequences

ordre <- cmdscale(as.dist(seq.om), k=1)

png(file.path(plots.folder, "sequences", "locnat", "sequences_classes_cmdscale_5.png"))
seqIplot(
  sd,
  group=seq.part5,
  sortv=ordre,
)
dev.off()



png(file.path(plots.folder, "sequences", "locnat", "sequences_classes_a-histo_5.png"))
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


# Distribution de tous les etats sur toute la periode, en frequence : 


png(file.path(plots.folder, "sequences", "locnat", "seqdplot_classesahisto_5.png"), width=750, height=480)
#png(file.path(plots.folder, "sequences", "nofun", "seqdplot.png"), width=1024, height=480)
seqdplot(
  sd.left,		# donnees a representer
  group=seq.part5,
  border=NA,				# avec ou sans bordure autour des barres verticales
  with.legend=FALSE,		# avec ou sans legende
  xtlab=FALSE
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
  1,									# 1 pour x et 2 pour y 
  at=1:ncol(sd), 						# position des labels sur l'axe
  labels=1:ncol(sd), 					# texte des labels : numero des etats
)
dev.off()



########### MEDOIDES ###########

#Cette methode permet de trouver l'element d'un groupe qui est le plus proche des autres en moyenne
#J'utilise la fonction seqrplot

png(file.path(plots.folder, "sequences", "locnat", "medoidesmultiples_5.png"), width=750, height = 520)
seqrplot(
  sd, 						# objet contenant les sequences
  group=seq.part5, 			# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,				# matrice de dissimilarites, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
)
dev.off()


medoid.ids <- disscenter(as.dist(seq.om), group=seq.part5, medoids.index="all")
print(medoid.ids)


#Je cherche maintenant a n'avoir qu'un seul parangon par classe : 

png(file.path(plots.folder, "sequences", "locnat", "medoidesimple_5.png"), width=1024, height = 520)
seqrplot(
  sd, 						# objet contenant les sequences
  group=seq.part5, 			# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,				# matrice de dissimilarites, comparant chaque paire de sequences
  criteria="frequency", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
  nrep=1					# si on veut forcer a n'avoir qu'un seul medoide
)
dev.off()



png(file.path(plots.folder, "sequences", "locnat", "repartition_etats_classes_a-histo_5.png"))
seqmtplot(
  sd.left,
  group=seq.part5,
  border=NA,
  ylim=c(0,15))
dev.off()

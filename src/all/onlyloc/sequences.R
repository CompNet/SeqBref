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
png(file.path(plots.folder, "sequences", "onlyloc", "legende.png"))
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

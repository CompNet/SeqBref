#######################################################
##### DONNEES FRANCE ECHANTILLON COMPLET #####
#####
##### DESCRIPTION 1 - TYPOLOGIES EN 8 CLASSES
#####
##### FEVRIER 2023
#######################################################




#################################################
########### PARTITION EN 8 CLASSES ##############
################################################

# Je vais essayer plusieurs decoupages afin de determiner le niveau le plus pertinent pour moi
# Je tente le decoupage en 8 dans ce script.


# D'abord, je coupe en 8 classes
nbcl <- 8
seq.part8 <- cutree(seq.dist, nbcl)
seq.part8 <- factor(
  seq.part8,
  labels=paste("classe", 1:nbcl, sep="." )
)

# 1ere tentative de typologie - sequences en tapis :
png(file.path(plots.folder, "sequences", "nofun", "Typologie_8.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd,
  group=seq.part8,
  border=NA,
)
dev.off()

png(file.path(plots.folder, "sequences", "nofun", "sequences_classes_8.png"))
seqIplot(
  sd,
  group=seq.part8,
)
dev.off()


# Je peux ensuite reproduire ce tapis de sequences en triant les sequences
# Je trie par multidimensional scaling
# Permet d'apporter plus de lisibilite : 

ordre <- cmdscale(as.dist(seq.om), k=1)

png(file.path(plots.folder, "sequences", "nofun", "sequences_classes_cmdscale_8.png"))
seqIplot(
  sd,
  group=seq.part8,
  sortv=ordre,
)
dev.off()

# Trop de NA... Il faudrait aussi pouvoir creer le seqIplot non pas par date de debut de la periode mais date de debut du 1er mandat de la sequence
# J'ajoute donc la mm version de ces tapis (typo et tapis de sequences) en utilisant la version "a-historique" des sequences
# J'utilise donc "sd.left", j'aligne les sequences non pas sur une DATE mais sur un EVENEMENT
# Cet evenement est le premier mandat connu pour chacune des sequences

png(file.path(plots.folder, "sequences", "nofun", "Typologie_a-histo_7.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd.left,
  group=seq.part8,
  border=NA,
)
dev.off()


png(file.path(plots.folder, "sequences", "nofun", "sequences_classes_a-histo_8.png"))
seqIplot(
  sd.left,
  group=seq.part8,
)
dev.off()


# Autre visualisation : la repartision des Etats par classes
# Je pense que la repartition est la meme qu'on soit en version DATE ou version AHISTORIQUE
# Oui, idem. 

png(file.path(plots.folder, "sequences", "nofun", "repartition_etats_classes_8.png"))
seqmtplot(sd,
          group=seq.part8,
          border=NA)
dev.off()

png(file.path(plots.folder, "sequences", "nofun", "repartition_etats_classes_a-histo_8.png"))
seqmtplot(
  sd.left,
  group=seq.part8,
  border=NA)
dev.off()



##############################################################
################# DESCRIPTION DE LA TYPOLOGIE ################
##############################################################

# Le poids des classes
table(seq.part8)

# Pourcentage
100*(table (seq.part8))/length(seq.part8)

# Homogeneite des classes
# La distance moyenne des sequences d'une classe au centre de cette classe permet de mesurer plus precisement l'homogeneite: 
dist.to.centers <- disscenter(as.dist(seq.om), group=seq.part8)
round(aggregate(dist.to.centers, list(seq.part8),mean)[,-1],1)

# Un autre indicateur de l'homogeneite interne a une classe est l'entropie transversale
# Elle decrit l'evolution de l'homogeneite de la classe pour chaque temps t
# Plus proche de 0, plus l'entreopie est faible, plus les individus de la classe sont tous dans la meme situation

png(file.path(plots.folder, "sequences", "nofun", "entropie_transversale_8.png"))
seqHtplot(
  sd, 
  group=seq.part8
)
dev.off()




##############################################
##### CROISEMENT AVEC ATTRIBUTS SOCIAUX ##### 
##############################################

# Je poursuis l'exploration de mes donnes et je vais au-dela des "simples" typologies
# Bien que la typologie soit deja un resultat en soi
# Je vais aller plus loin en cherchant les facteurs d'appartenance a telle ou telle classe
# La classe devient le facteur a expliquer. 



# Le nombre de femmes dans les classes : 
assoc.twocat(factor(seq.part8),
             factor(repr.all[,"Sexe"]))


# Le nombre de professions dans les classes : 
assoc.twocat(factor(seq.part8),
             factor(repr.all[,"LibelleProfession"]))


# Le nombre de divers dans les classes : 
assoc.twocat(factor(seq.part8),
             factor(repr.all[,"Divers"]))


# Le nombre de divers dans les classes : 
assoc.twocat(factor(seq.part8),
             factor(repr.all[,"DepartementDeNaissance"]))




###############################################################################
################### MATRICE DE DISTANCES ###################################
##############################################################################




# La matrice des distances entre trajectoire resumee graphiquement
png(file.path(plots.folder, "sequences", "nofun", "distance_class_8.png"))

# projete les donnees dans le plan : chaque point represente une sequence
# les coordonnees des points sont exprimees dans des unites arbitraires :
# elles n'ont pas de signification particuliere, si ce n'est de positionner les points de facon relative.
# la projection est faite en essayant de respecter les distances,
# i.e. la distance entre 2 points reflete la distance entre les sequences correspondantes
mds <- cmdscale(seq.om, k=2)

# initialise le graphique
plot(
  mds, 										# les points a dessiner
  type="n", 								# on choisit de ne pas les dessiner (=graphique vide)
  xlab="Dimension 1", ylab="Dimension 2"	# labels places sur les axes du graphique
)

# on rajoute les points dans le graphique auparavant vide
points(
  mds, 										# coordonnees des points a dessiner
  pch=20, 									# symbole(s) utilise pour representer chaque point (cf. http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
  col=seq.part8								# couleur(s) utilisee pour dessiner les points
  # note: on peut preciser un seul symbole ou couleur pour tous les points c'est le meme symbole/couleur qui sera utilise partout
  # on peut aussi preciser un symbole/couleur different pour chaque point
  # en l'occurrence, ci-dessus on a un seul symbole (20) mais chaque point a sa propre couleur (vecteur seq.part)
)

# on rajoute une ligne droite dans le graphique
abline(
  h=0, 										# ceci indique qu'il s'agit d'une ligne horizontale placee en y=0
  v=0, 										# ceci specifie une ligne verticale placee en x=0
  lty=2, 									# type de ligne (ici des pointilles, cf. http://www.sthda.com/english/wiki/line-types-in-r-lty
  col="lightgray"							# couleur de la ligne tracee
)

# on rajoute une legende dans le graphique
legend(
  "topright", 								# position de la legende dans le graphique
  legend=paste("Classe",1:nbcl), 			# texte a inserer dans la legende
  pch=20, 									# forme des points a dessiner dans la legende (cf. "points")
  col=1:nbcl, 								# couleur de ces points
  cex=0.8									# controle la taille du texte dans la legende
)

# on finalise le graphique
dev.off()
###############






###############
# Version modifiee, avec le sexe a la place de la classe:
png(file.path(plots.folder, "sequences", "nofun", "distance_sex_8.png"))
plot(
  mds,
  type="n",
  xlab="Dimension 1", ylab="Dimension 2"
)
# j'utilise le symbole rose pour les femmes et le bleu pour les hommes
# (cliche, je sais, mais ca permet de comprendre tout de suite le graphique !)
cols <- rep("GREEN", length(repr.all[,"Sexe"]))
cols[repr.all[,"Sexe"]=="F"] <- "ORANGE"
points(
  mds,
  pch=20,
  col=cols
)
legend(
  "topright",
  title="Sexe",
  legend=c("Homme","Femme"),
  pch=20,
  col=c("GREEN","ORANGE"),
  cex=0.8
)
dev.off()
###############




############################
############################
#il faudra realiser la meme chose avec profession et dept de naissance quand sera simplifies
############################
############################










########### MEDOIDES ###########

#Cette methode permet de trouver l'element d'un groupe qui est le plus proche des autres en moyenne
#J'utilise la fonction seqrplot

png(file.path(plots.folder, "sequences", "nofun", "medoidesmultiples_8.png"))
seqrplot(
  sd, 						# objet contenant les sequences
  group=seq.part8, 			# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,				# matrice de dissimilarites, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
)
dev.off()


medoid.ids <- disscenter(as.dist(seq.om), group=seq.part8, medoids.index="first")
print(medoid.ids)


#Je cherche maintenant a n'avoir qu'un seul parangon par classe : 

png(file.path(plots.folder, "sequences", "nofun", "medoidesimple_8.png"))
seqrplot(
  sd, 						# objet contenant les sequences
  group=seq.part8, 			# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,				# matrice de dissimilarites, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
  nrep=1					# si on veut forcer a n'avoir qu'un seul medoide
)
dev.off()



#######################################################
##### DONNEES maires Ve REP #####
#####
##### Analyse de sequences
#####
##### FEVRIER 2023
#####
#######################################################





#######################################################
# Conversion
#######################################################

# conversion des donnees en sequences
sd <- convert.to.sequences(
  # table contenant les donnees
  tab.mandates=tab.m9,
  # date de debut de la periode a couvrir, au format annee/mois/jour
  start.date=as.Date("1935/01/01"),
  # date de fin de cette periode, au meme format
  end.date=as.Date("2023/01/01"),
  # option permettant de ne *pas* aligner les sequences a gauche
  left=NA,
  # couleurs ÃÂ  utiliser
  colors=get.color.palette(9)		# vraies couleurs
  #	colors=get.gray.palette(9)		# 9 niveaux de gris
)
summary(sd)

# conversion des donnees en sequences, version a-historique
sd.left <- convert.to.sequences(
  # table contenant les donnees
  tab.mandates=tab.m9,
  # date de debut de la periode a couvrir, au format annee/mois/jour
  start.date=as.Date("1935/01/01"),
  # date de fin de cette periode, au meme format
  end.date=as.Date("2023/01/01"),
  # option permettant d'aligner les sequences a gauche
  left="DEL",
  # couleurs ÃÂ  utiliser
  colors=get.color.palette(9)		# vraies couleurs
  #	colors=get.gray.palette(9)		# 9 niveaux de gris
)
summary(sd.left)




#######################################################
# Graphiques
#######################################################


png(file.path(plots.folder, "maires" ,"m9_seqIplot.png"))
seqIplot(
  sd, 
  sortv="from.start", 
  ylas=2,
  with.legend=FALSE
)
dev.off()

png(file.path(plots.folder, "maires" ,"m9_seqIplot_ahisto.png"))
seqIplot(
  sd.left, 
  sortv="from.start", 
  ylas=2,
  with.legend=FALSE
)
dev.off()



########################################################
### J'explore les autres representations visuelles : ###
########################################################


# Distribution de tous les etats sur toute la periode, en frequence : 
png(file.path(plots.folder, "maires" ,"m9_seqdplot.png"))
seqdplot(
  sd,
  border=NA,
  with.legend=FALSE
)
dev.off()

#creation d'un seqdplot dans sa version ahistorique : 
png(file.path(plots.folder, "maires" ,"m9_seqdplot2.png"))
seqdplot(
  sd.left,
  border=NA,
  with.legend=FALSE
)
dev.off()

# Afficher les valeurs des frequences du plot precedent, annee par annee: 
seqstatd(sd)



# Sequences les plus frequentes : 
png(file.path(plots.folder, "maires" ,"m9_seq_more_freq.png"))
seqfplot(
  sd,
  with.legend=FALSE,
  with.missing=FALSE
)
dev.off()


# Idem, missing du debut a retirer, donc version "a-historique" : 
png(file.path(plots.folder, "maires" ,"m9_seq_more_freq_ahisto.png"))
seqfplot(
  sd.left,
  with.legend=FALSE,
  with.missing=FALSE
)
dev.off()




#Les taux de transition / passage d'un etat a l'autre :
trans <- seqtrate(sd)
round(trans, 2)
# plus proche de 1, plus la probabilite est importante
# peut m'aider a definir les couts pour l'Optimal Matching ??

# Verification (je ne devrais avoir que 1 partout) :
rowSums(trans)
# Verification ok.



##############################################
##### VISUALISER LES SEQUENCES ##############
#############################################


# Calculer l'entropy : plus proche de 1, plus la sequence contient des etats divers / plus proche 0 moins il y a d'etats differents : 
sd.ent <- seqient(sd)
summary(sd.ent)

# Repartition de l'entropie en histogramme : 
png(file.path(plots.folder, "maires" ,"m9_entropy.png"))
hist(sd.ent,
     col="orange",
     main=NULL,
     xlab="Entropy")
dev.off()

# Je recherche qui a la plus forte entropie : 
index <- which(sd.ent== max(sd.ent))
sd[sd.ent==max(sd.ent),] #Selon le guide TraMineR, cette commande fonctionne un peu mysterieusement



############################
##### Optimal Matching #####
############################


# Je calcule les coÃ»ts 
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


png(file.path(plots.folder, "maires" ,"m9_dendrogram.png"))
plot(as.dendrogram(seq.dist), leaflab="none")
abline(	# trace une ligne droite (horizontale ici, mais elle peut etre quelconque)
  #	a=...,		# coefficient directeur (pour les lignes ni horizontales ni verticales)
  #	b=...,		# ordonnee a l'origine (ni horizontal ni vertical)
  h=175,		# hauteur, si c'est une ligne horizontale	
  #	v=...,		# position, si c'est une ligne verticale
  col="RED",	# couleur de la ligne
  lty=2		# type de ligne (trait plein, pointilles, etc. voir : http://www.sthda.com/english/wiki/line-types-in-r-lty
)
abline(	# trace une ligne droite (horizontale ici, mais elle peut etre quelconque)
  #	a=...,		# coefficient directeur (pour les lignes ni horizontales ni verticales)
  #	b=...,		# ordonnee a l'origine (ni horizontal ni vertical)
  h=250,		# hauteur, si c'est une ligne horizontale	
  #	v=...,		# position, si c'est une ligne verticale
  col="BLUE",	# couleur de la ligne
  lty=2		# type de ligne (trait plein, pointilles, etc. voir : http://www.sthda.com/english/wiki/line-types-in-r-lty
)
dev.off()
# Je trace une ligne verticale legermenet au dessus de 175 et coupe ainsi 5 fois une ligne verticale
#Une autre au dessus de 250 pour couper 3 fois
# note de VL : tu peux carremment tracer la ligne dans le graphique, je t'ai rajoute l'instruction ci-dessus




#################################################
########### PARTITION EN 3 CLASSES ##############
################################################


# D'abord, je coupe en 3 classes
nbcl <- 3
seq.part3 <- cutree(seq.dist, nbcl)
seq.part3 <- factor(
  seq.part3,
  labels=paste("classe", 1:nbcl, sep="." )
)

# 1ere tentative de typologie - sequences en tapis :
png(file.path(plots.folder, "maires" ,"m9_Typologie_3.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd,
  group=seq.part3,
  border=NA,
)
dev.off()

png(file.path(plots.folder, "maires" ,"m9_sequences_classes_3.png"))
seqIplot(
  sd,
  group=seq.part3,
)
dev.off()


# Je peux ensuite reproduire ce tapis de sequences en triant les sequences
# Je trie par multidimensional scaling
# Permet d'apporter plus de lisibilite : 

ordre <- cmdscale(as.dist(seq.om), k=1)

png(file.path(plots.folder, "maires" ,"m9_sequences_classes_cmdscale_3.png"))
seqIplot(
  sd,
  group=seq.part3,
  sortv=ordre,
)
dev.off()

# Trop de NA... Il faudrait aussi pouvoir creer le seqIplot non pas par date de debut de la periode mais date de debut du 1er mandat de la sequence
# J'ajoute donc la mm version de ces tapis (typo et tapis de sequences) en utilisant la version "a-historique" des sequences
# J'utilise donc "sd.left", j'aligne les sequences non pas sur une DATE mais sur un EVENEMENT
# Cet evenement est le premier mandat connu pour chacune des sequences

png(file.path(plots.folder, "maires" ,"Typologie_a-histo_3.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd.left,
  group=seq.part3,
  border=NA,
)
dev.off()


png(file.path(plots.folder, "maires" ,"m9_sequences_classes_a-histo_3.png"))
seqIplot(
  sd.left,
  group=seq.part3,
)
dev.off()


# Autre visualisation : la repartision des Etats par classes
# Je pense que la repartition est la meme qu'on soit en version DATE ou version AHISTORIQUE
# Oui, idem. 

png(file.path(plots.folder, "maires" ,"m9_repartition_etats_classes_3.png"))
seqmtplot(sd,
          group=seq.part3,
          border=NA)
dev.off()

png(file.path(plots.folder, "maires" ,"m9_repartition_etats_classes_a-histo_3.png"))
seqmtplot(
  sd.left,
  group=seq.part3,
  border=NA)
dev.off()



##############################################################
################# DESCRIPTION DE LA TYPOLOGIE ################
##############################################################

# Le poids des classes
table(seq.part3)

# Pourcentage
100*(table (seq.part3))/length(seq.part4)

# Homogeneite des classes
# La distance moyenne des sequences d'une classe au centre de cette classe permet de mesurer plus precisement l'homogeneite: 
dist.to.centers <- disscenter(as.dist(seq.om), group=seq.part3)
round(aggregate(dist.to.centers, list(seq.part3),mean)[,-1],1)

# Un autre indicateur de l'homogeneite interne a une classe est l'entropie transversale
# Elle decrit l'evolution de l'homogeneite de la classe pour chaque temps t
# Plus proche de 0, plus l'entreopie est faible, plus les individus de la classe sont tous dans la meme situation

png(file.path(plots.folder, "maires" ,"m9_entropie_transversale_3.png"))
seqHtplot(
  sd, 
  group=seq.part3
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
assoc.twocat(factor(seq.part3),
             factor(repr.all[,"Sexe"]))

# Le nombre de professions dans les classes : 
assoc.twocat(factor(seq.part3),
             factor(repr.all[,"LibelleProfession"]))

# Le nombre de divers dans les classes : 
assoc.twocat(factor(seq.part3),
             factor(repr.all[,"Divers"]))

# Le nombre de divers dans les classes : 
assoc.twocat(factor(seq.part3),
             factor(repr.all[,"m9artementDeNaissance"]))





#################################################
########### PARTITION EN 5 CLASSES ##############
################################################


# D'abord, je coupe en 5 classes
nbcl <- 5
seq.part5 <- cutree(seq.dist, nbcl)
seq.part5 <- factor(
  seq.part5,
  labels=paste("classe", 1:nbcl, sep="." )
)

# 1ere tentative de typologie - sequences en tapis :
png(file.path(plots.folder, "maires" ,"m9_Typologie_5.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd,
  group=seq.part5,
  border=NA,
)
dev.off()

png(file.path(plots.folder, "maires" ,"m9_sequences_classes_5.png"))
seqIplot(
  sd,
  group=seq.part5,
)
dev.off()


#################################################################
# A EVENTUELLEMENT POURSUIVRE EN REPRENANT LES AUTRES SCRIPTS ###
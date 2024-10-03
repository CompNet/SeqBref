#######################################################
##### DONNEES CDO Ve REP #####
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
  tab.mandates=tab.cdo,
  # date de debut de la periode a couvrir, au format annee/mois/jour
  start.date=as.Date("1931/01/01"),
  # date de fin de cette periode, au meme format
  end.date=as.Date("2023/01/01"),
  # option permettant de ne *pas* aligner les sequences a gauche
  left=NA,
  # couleurs Ã  utiliser
  colors=get.color.palette(9)		# vraies couleurs
  #	colors=get.gray.palette(9)		# 9 niveaux de gris
)
summary(sd)

# conversion des donnees en sequences, version a-historique
sd.left <- convert.to.sequences(
  # table contenant les donnees
  tab.mandates=tab.cdo,
  # date de debut de la periode a couvrir, au format annee/mois/jour
  start.date=as.Date("1931/01/01"),
  # date de fin de cette periode, au meme format
  end.date=as.Date("2023/01/01"),
  # option permettant d'aligner les sequences a gauche
  left="DEL",
  # couleurs Ã  utiliser
  colors=get.color.palette(9)		# vraies couleurs
  #	colors=get.gray.palette(9)		# 9 niveaux de gris
)
summary(sd.left)




#######################################################
# Graphiques
#######################################################

# legende
png(file.path(plots.folder, "departements" ,"legende.png"))
seqlegend(sd)
dev.off()

png(file.path(plots.folder, "departements" ,"cdo_seqIplot.png"))
seqIplot(
  sd, 
  sortv="from.start", 
  ylas=2,
  with.legend=FALSE
)
dev.off()

png(file.path(plots.folder, "departements" ,"cdo_seqIplot_ahisto.png"))
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
png(file.path(plots.folder, "departements" ,"cdo_seqdplot.png"))
seqdplot(
  sd,
  border=NA,
  with.legend=FALSE
)
dev.off()

#creation d'un seqdplot dans sa version ahistorique : 
png(file.path(plots.folder, "departements" ,"cdo_seqdplot2.png"))
seqdplot(
  sd.left,
  border=NA,
  with.legend=FALSE
)
dev.off()

# Afficher les valeurs des frequences du plot precedent, annee par annee: 
seqstatd(sd)



# Sequences les plus frequentes : 
png(file.path(plots.folder, "departements" ,"cdo_seq_more_freq.png"))
seqfplot(
  sd,
  with.legend=FALSE,
  with.missing=FALSE
)
dev.off()


# Idem, missing du debut a retirer, donc version "a-historique" : 
png(file.path(plots.folder, "departements" ,"cdo_seq_more_freq_ahisto.png"))
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



# Le temps passe dans chaque etat pour chacune des sequences : 
seqistatd(sd)

# Calculer l'entropy : plus proche de 1, plus la sequence contient des etats divers / plus proche 0 moins il y a d'etats differents : 
sd.ent <- seqient(sd)
summary(sd.ent)

# Repartition de l'entropie en histogramme : 
png(file.path(plots.folder, "departements" ,"cdo_entropy.png"))
hist(sd.ent,
     col="orange",
     mcdo=NULL,
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


png(file.path(plots.folder, "departements" ,"cdo_dendrogram.png"))
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
  h=260,		# hauteur, si c'est une ligne horizontale	
  #	v=...,		# position, si c'est une ligne verticale
  col="BLUE",	# couleur de la ligne
  lty=2		# type de ligne (trait plein, pointilles, etc. voir : http://www.sthda.com/english/wiki/line-types-in-r-lty
)
dev.off()
# Je trace une ligne verticale legermenet au dessus de 175 et coupe cdosi 8 fois une ligne verticale
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




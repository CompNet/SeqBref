#######################################################
##### DONNEES BREF Ve REP #####
#####
##### Analyse : ROTATION a partir des matrices
#####
##### AVRIL 2023
#####
#######################################################


#Dans ce script, je travaille a partir des matrices calculees dans le script turnover_matrices


################################################
#### ROTATION GENERALE SUR TOUTE LA PERIODE ###
################################################


cat("Nombre de mandats moyen par elu : ",nrow(tab.ass)/length(ids),"\n",sep="")
# 2,30 mandats par elu


#Combien d'elus par legis ?
table(tab.ass[,"Legislature"])



# LES MOTIFS DE DEPART EXPLIQUENT LE GONFLEMENT DES EXPLOSIONS : beaucoup de courts passages par l'AS
table(tab.ass[,"MotifFinMandat"])
idx <- which(tab.ass[,"MotifFinMandat"]!= "FM") #je ne garde que les différents de FM
motif <- table(tab.ass[idx,"MotifFinMandat"], tab.ass[idx,"Legislature"])
View(motif)

# on represente les decomptes sous forme de graphique a barres
png(file.path(plots.folder, "motif_legis_ass.png"))
barplot(
  motif, 					# decomptes a afficher
  col=rainbow(7),
  legend.text = TRUE,
  xlim=c(0, 1.5*ncol(motif)),
  las=2					# labels x verticaux
)
dev.off()


#Je realise le meme graph mais en pourcentage pour chaque legis
motif <- table(tab.ass[idx,"MotifFinMandat"], tab.ass[idx,"Legislature"])
motif2 <- apply(X=motif, MARGIN=2, function(col) col/sum(col)*100)
View(motif2)

# on represente les decomptes sous forme de graphique a barres
png(file.path(plots.folder, "motif2_legis_ass.png"))
barplot(
  motif2, 					# decomptes a afficher
  col=rainbow(7),
  legend.text = TRUE,
  args.legend = list(x="topright", inset = c(-0.09,0)),
  xlim=c(0, 1.5*ncol(motif2)),
  las=2					# labels x verticaux
)
dev.off()




#############################################################
### CREATION DE GRAPHS SUR REELECTION / TURNOVER ###########
############################################################


# D'abord le graph sur les novices / newish / reelus

png(file.path(plots.folder, "rotation_noviciat_ass.png"))
barplot(
  legis.stats, 					# decomptes a afficher
  legend.text=c("Novices", "Nouv. relatifs", "Reelus", "Total"),
  col=topo.colors(4), 
  xlim=c(0, 1.75*ncol(legis.stats)),
  las=2					# labels x verticaux
)
dev.off()

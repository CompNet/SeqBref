#######################################################
##### DONNEES BREF 1958-2020 #####
#####
##### Programme principal : ETUDE DU RENOUVELLEMENT PAR INSTITUTION
#####
##### FEVRIER 2023
#####
#######################################################




########### GENERAL ###########

# chargement des bibliotheques
library("readr")
library("dplyr")
library("TraMineR")
library("GDAtools")
library("igraph")

# chemin du dossier contenant les donnees et les scripts
#folder <- "U:/Thèse/AS et BREF/France/RotationBref_V4"
#folder <- "C:/Users/Vincent/eclipse/workspaces/Networks/RotationBref"
#folder <- "D:/Users/Vincent/eclipse/workspaces/Extraction/RotationBref"
folder <- "."

# autres dossiers
src.folder <- file.path(folder,"src", "rotations")
data.folder <- file.path(folder, "data", "rotations")
plots.folder <- file.path(folder, "plots", "rotations")



########### ASSEMBLEE ###########
# chargement et nettoyage des donnees de l'assemblee
source(file.path(src.folder, "ass", "ass_preprocessing.R"))
source(file.path(src.folder, "ass", "ass_turnover.R"))
source(file.path(src.folder, "ass", "ass_rotation.R"))


########### SENAT ###########
# chargement et nettoyage des donnees du Senat
source(file.path(src.folder, "sen", "sen_preprocessing.R"))
source(file.path(src.folder, "sen", "sen_turnover.R"))


########### MAIRES +9 ###########
# chargement et nettoyage des donnees du Senat
source(file.path(src.folder, "m9", "m9_preprocessing.R"))
source(file.path(src.folder, "m9", "m9_turnover.R"))
source(file.path(src.folder, "m9", "m9_parties.R"))

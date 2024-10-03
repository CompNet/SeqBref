#########################################################
##### DONNEES BREF  #####
#####
##### Programme principal V1
#####
##### SEPTEMBRE 2022 - FEVRIER 2023
#####
#######################################################

########### GENERAL ###########

# chargement des bibliotheques
library("readr")
library("dplyr")
library("TraMineR")
library("GDAtools")
library("plot.matrix")
library("viridis")
library("scales")
library("igraph")

# chemin du dossier contenant les donnees et les scripts
folder <- "U:/Thèse/AS et BREF/France/SeqBref"
# <- "C:/Users/Vincent/eclipse/workspaces/Networks/SeqBref"
#folder <- "D:/Users/Vincent/eclipse/workspaces/Extraction/SeqBref"

# autres dossiers
src.folder <- file.path(folder,"src")
data.folder <- file.path(folder,"data")
plots.folder <- file.path(folder,"plots")

# chargement des fonctions communes
source(file.path(src.folder, "common", "strings.R"))
source(file.path(src.folder, "common", "maps.R"))
source(file.path(src.folder, "common", "colors.R"))
source(file.path(src.folder, "common", "conversion.R"))
source(file.path(src.folder, "common", "transmat.R"))


# traitement : 

# chargement et nettoyage des donnees Ain
source(file.path(src.folder, "dpt", "ain", "ain_preprocessing.R"))
source(file.path(src.folder, "dpt", "ain", "ain_analyses_stat.R"))
source(file.path(src.folder, "dpt/ain", "ain_sequences.R"))

# chargement et nettoyage des donnees CdO
source(file.path(src.folder, "dpt", "cdo", "cdo_preprocessing.R"))
source(file.path(src.folder, "dpt", "cdo", "cdo_analyses_stat.R"))
source(file.path(src.folder, "dpt/cdo", "cdo_sequences.R"))

# chargement et nettoyage des donnees BdR
source(file.path(src.folder, "dpt", "bdr", "bdr_preprocessing.R"))
source(file.path(src.folder, "dpt", "bdr", "bdr_analyses_stat.R"))
source(file.path(src.folder, "dpt/bdr", "bdr_sequences.R"))

# chargement et nettoyage des donnes SsD
source(file.path(src.folder, "dpt", "ssd", "ssd_preprocessing.R"))
source(file.path(src.folder, "dpt", "ssd", "ssd_analyses_stat.R"))
source(file.path(src.folder, "dpt/ssd", "ssd_sequences.R"))

# fusion de toutes les donnees des 4 departements
source(file.path(src.folder, "all", "fusion.R"))
source(file.path(src.folder, "all", "all_analyses_stat.R"))

# analyse de sequences echantillon total
source(file.path(src.folder, "all", "nofun", "sequences.R"))
source(file.path(src.folder, "all", "nofun", "4classes_sequences.R"))
source(file.path(src.folder, "all", "nofun", "8classes_sequences.R"))

# par type de mandat
source(file.path(src.folder, "type_mandat", "partition_type_mandat.R"))
source(file.path(src.folder, "type_mandat", "dep_sequences.R"))
source(file.path(src.folder, "type_mandat", "sen_sequences.R"))
source(file.path(src.folder, "type_mandat", "m9_sequences.R"))

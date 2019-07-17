##########################################################
# Scripts for producing Figures and Supplementary Figures
##########################################################

#Change figures png -> jpg

system("mkdir analysis/Figs")
system("mkdir analysis/Figs/Res_simulation")
system("mkdir analysis/Figs/Res_norm")
system("mkdir analysis/Figs/Res_raw")
system("mkdir analysis/SuppFigs")
system("mkdir analysis/SuppFigs/Res_simulation")
system("mkdir analysis/SuppFigs/Res_norm")
system("mkdir analysis/SuppFigs/Res_raw")


library(ggplot2)
library(ggfortify)
library(GGally)
library(dplyr)
library(ComplexHeatmap)

################################################################################
# SIMULATE DATA ANALYSIS
################################################################################

source("analysis/functions.R")

folder="Results_simulate_data"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)
Tab$Cell_groups <- factor(sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 4), levels = c("gr4", "gr8", "gr16"))


Tab0=Tab[which(Tab$Name=="setup1"),]
Tab1=Tab[which(Tab$Name=="setup2"),]
Tab2=Tab[which(Tab$Name=="setup3"),]

#Heatmap
ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateHeatmap.png") , plot = plotARIheatmap_fun(Tab), width = 30, height = 30, units = "cm")

#Accuracy
ggsave(filename=paste0("analysis/Figs/Res_simulation/SimulateAccuracySetup1balanced.png") , plot = plotAccuracy_fun(Tab0, "balanced"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1unbalanced.png") , plot = plotAccuracy_fun(Tab0, "unbalanced"), width = 47, height = 23, units = "cm")

Tab0gr4=Tab0[which(Tab0$Cell_groups=="gr4"),]
Tab0gr8=Tab0[which(Tab0$Cell_groups=="gr8"),]
Tab0gr16=Tab0[which(Tab0$Cell_groups=="gr16"),]

# Balanced split by nr groups: 4, 8, 16
ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1Gr4balanced.png") , plot = plotAccuracy_fun(Tab0gr4, "balanced"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1Gr8balanced.png") , plot = plotAccuracy_fun(Tab0gr8, "balanced"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1Gr16balanced.png") , plot = plotAccuracy_fun(Tab0gr16, "balanced"), width = 47, height = 23, units = "cm")

# Unbalanced split by nr groups: 4, 8, 16
# ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1Gr4unbalanced.png") , plot = plotAccuracy_fun(Tab0gr4, "unbalanced"), width = 47, height = 23, units = "cm")
# ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1Gr8unbalanced.png") , plot = plotAccuracy_fun(Tab0gr8, "unbalanced"), width = 47, height = 23, units = "cm")
# ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateAccuracySetup1Gr16unbalanced.png") , plot = plotAccuracy_fun(Tab0gr16, "unbalanced"), width = 47, height = 23, units = "cm")

# Setup 2 and 3
ggsave(filename=paste0("analysis/Figs/Res_simulation/SimulateAccuracySetup2.png") , plot = plotAccuracy1_fun(Tab1, "setup2"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/Figs/Res_simulation/SimulateAccuracySetup3.png") , plot = plotAccuracy1_fun(Tab2, "setup3"), width = 47, height = 23, units = "cm")

# Run time
ggsave(filename=paste0("analysis/SuppFigs/Res_simulation/SimulateRunTimeSetup1.png") , plot = plotTime_fun(Tab0), width = 60, height = 35, units = "cm")


################################################################################
# RAW DATA ANALYSIS
################################################################################

#######################################
# Methods preprocessing: NONE
#######################################
rm(list=ls())

source("analysis/functions.R")


folder="Results_raw"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

Tab$preproc=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
Tab=Tab[which(Tab$preproc=="none"),] 

# Complex Heatmap
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawComplexHeatmapNone.png") , plot = plotARIComplexHeatmap_fun(Tab, "all"), width = 60, height = 30, units = "cm", device="png")
ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawComplexHeatmapNone_A.png") , plot = plotARIComplexHeatmap_fun(Tab, "A"), width = 35, height = 30, units = "cm", device="png")
ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawComplexHeatmapNone_B.png") , plot = plotARIComplexHeatmap_fun(Tab, "B"), width = 35, height = 30, units = "cm", device="png")

Tab$dim_red=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
Tab=Tab[-which(Tab$Method=="sincell" & Tab$dim_red =="ICA"),] 

# Accuracy 
ggsave(filename=paste0("analysis/Figs/Res_raw/RawAccuracyNone.png") , plot = plotAccuracy2_fun(Tab), width = 39, height = 26, units = "cm")

# P-values
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawPvalNone.png") , plot = plotPval_fun(Tab), width = 36, height = 18, units = "cm")

# Estimation of nr clusters
ggsave(filename=paste0("analysis/Figs/Res_raw/RawEstimClustNone.png") , plot = plotEstimation_fun(Tab), width = 36, height = 22, units = "cm")


#######################################
# Methods preprocessing: METHOD SPECIFIC
#######################################
rm(list=ls())

source("analysis/functions.R")

folder="Results_raw"
files=list.files(folder, pattern = ".Rdata",  recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

Tab$preproc=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
Tab=Tab[which(Tab$preproc=="methodspecific"),] #only method specific

# Complex Heatmap
ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawComplexHeatmapMetSpec.png") , plot = plotARIComplexHeatmap_fun(Tab, "all"), width = 60, height = 30, units = "cm", device="png")

# Accuracy
ggsave(filename=paste0("analysis/Figs/Res_raw/RawAccuracyMetSpec.png") , plot = plotAccuracy2_fun(Tab), width = 40, height = 19, units = "cm")

# P-values
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawPvalMetSpec.png") , plot = plotPval1_fun(Tab), width = 36, height = 18, units = "cm")

# Estimation of nr clusters
ggsave(filename=paste0("analysis/Figs/Res_raw/RawEstimClustMetSpec.png") , plot = plotEstimation_fun(Tab), width = 36, height = 22, units = "cm")

#######################################
# Methods preprocessing: ALL
#######################################
rm(list=ls())

source("analysis/functions.R")


folder="Results_raw"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

Tab$dim_red=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
Tab=Tab[-which(Tab$Method=="sincell" & Tab$dim_red =="ICA"),] 

Tab0=Tab[which(Tab$Name=="qc"),]
Tab1=Tab[which(Tab$Name=="qc_filt"),]
Tab2=Tab[which(Tab$Name=="qc_filt_norm"),]

# PCA
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawSummaryPCAQC.png") , plot = plotSummaryPCA_fun(Tab0, 2), width = 28, height = 10, units = "cm")
ggsave(filename=paste0("analysis/Figs/Res_raw/RawSummaryPCAQCFILT.png") , plot = plotSummaryPCA_fun(Tab1, 2), width = 28, height = 10, units = "cm")

# Correlation 
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawCorrelationPCAQC.png") , plot = plotCorrelation_fun(Tab0), width = 20, height = 15, units = "cm")
ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawCorrelationPCAQCFILT.png") , plot = plotCorrelation_fun(Tab1), width = 20, height = 15, units = "cm")

# Run time
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawRunTimeQC.png") , plot = plotTime1_fun(Tab0), width = 40, height = 20, units = "cm")
ggsave(filename=paste0("analysis/Figs/Res_raw/RawRunTimeQCFILT.png") , plot = plotTime1_fun(Tab1), width = 40, height = 20, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_raw/RawRunTimeQCFILTNORM.png") , plot = plotTime1_fun(Tab2), width = 40, height = 20, units = "cm")

################################################################################
# NORM DATA ANALYSIS
################################################################################
rm(list=ls())

source("analysis/functions.R")

folder="Results_norm"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

# ComplexHeatmap
ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormComplexHeatmap.png") , plot = plotARIComplexHeatmap1_fun(Tab), width = 50, height = 20, units = "cm")

Tab$dim_red=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
Tab=Tab[-which(Tab$Method=="Linnorm" & Tab$dim_red=="tSNE"),]
Tab=Tab[-which(Tab$Method=="monocle3" & Tab$dim_red=="tSNE"),]
Tab=Tab[-which(Tab$Method=="sincell" & Tab$dim_red=="tSNE"),]
Tab=Tab[-which(Tab$Method=="Linnorm" & Tab$dim_red=="PCA"),]
Tab$dims <- paste0("dims_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4))

# Accuracy
ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormAccuracy.png") , plot = plotAccuracyPerMethod2_fun(Tab), width = 40, height = 27, units = "cm")

# P-values
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormPval.png") , plot = plotPval2_fun(Tab), width = 36, height = 18, units = "cm")

# Estimation of nr clusters
ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormEstimClust.png") , plot = plotEstimation1_fun(Tab), width = 36, height = 26, units = "cm")

Tab0=Tab[which(Tab$dims=="dims_3"),]
Tab1=Tab[which(Tab$dims=="dims_5"),]
Tab2=Tab[which(Tab$dims=="dims_10"),]
Tab3=Tab[which(Tab$dims=="dims_15"),]

# PCA , enforce positive correlation with ARI
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormSummaryPCAdims3.png") , plot = plotSummaryPCA1_fun(Tab0), width = 35, height = 10, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormSummaryPCAdims5.png") , plot = plotSummaryPCA1_fun(Tab1), width = 35, height = 10, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormSummaryPCAdims10.png") , plot = plotSummaryPCA1_fun(Tab2), width = 35, height = 10, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormSummaryPCAdims15.png") , plot = plotSummaryPCA1_fun(Tab3), width = 35, height = 10, units = "cm")

# Correlation
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormCorrelationPCAdims3.png") , plot = plotCorrelation1_fun(Tab0), width = 20, height = 15, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormCorrelationPCAdims5.png") , plot = plotCorrelation1_fun(Tab1), width = 20, height = 15, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormCorrelationPCAdims10.png") , plot = plotCorrelation1_fun(Tab2), width = 20, height = 15, units = "cm")
#ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormCorrelationPCAdims15.png") , plot = plotCorrelation1_fun(Tab3), width = 20, height = 15, units = "cm")

# Run time
ggsave(filename=paste0("analysis/SuppFigs/Res_norm/NormRunTimedims3.png") , plot = plotTime1_fun(Tab0), width = 40, height = 20, units = "cm")
# ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims5.png") , plot = plotTime1_fun(Tab1), width = 40, height = 20, units = "cm")
# ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims10.png") , plot = plotTime1_fun(Tab2), width = 40, height = 20, units = "cm")
# ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims15.png") , plot = plotTime1_fun(Tab3), width = 40, height = 20, units = "cm")









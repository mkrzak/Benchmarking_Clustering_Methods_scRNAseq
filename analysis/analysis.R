#Change figures png -> jpg

system("mkdir analysis/plots")
system("mkdir analysis/plots/ARI")
system("mkdir analysis/plots/ARI/simulate_data")
system("mkdir analysis/plots/ARI/raw_data")
system("mkdir analysis/plots/ARI/norm_data")
system("mkdir analysis/plots/Run_time")
system("mkdir analysis/plots/Run_time/simulate_data")
system("mkdir analysis/plots/Run_time/raw_data")
system("mkdir analysis/plots/Run_time/norm_data")

library(ggplot2)
library(ggfortify)
library(GGally)
library(dplyr)
library(ComplexHeatmap)

################################################################################
# SIMULATE DATA ANALYSIS
################################################################################

source("analysis/doTable.R")
source("analysis/plotARI.R")
source("analysis/plotTime.R")


folder="Results_simulate_data"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)
Tab$Cell_groups <- factor(sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 4), levels = c("gr4", "gr8", "gr16"))


Tab0=Tab[which(Tab$Name=="setup1"),]
Tab1=Tab[which(Tab$Name=="setup2"),]
Tab2=Tab[which(Tab$Name=="setup3"),]

#Heatmap
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateHeatmap.png") , plot = plotARIheatmap_fun(Tab), width = 30, height = 30, units = "cm")

#Accuracy
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1balanced.png") , plot = plotAccuracy_fun(Tab0, "balanced"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1unbalanced.png") , plot = plotAccuracy_fun(Tab0, "unbalanced"), width = 47, height = 23, units = "cm")

Tab0gr4=Tab0[which(Tab0$Cell_groups=="gr4"),]
Tab0gr8=Tab0[which(Tab0$Cell_groups=="gr8"),]
Tab0gr16=Tab0[which(Tab0$Cell_groups=="gr16"),]

# Balanced split by nr groups: 4, 8, 16
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1Gr4balanced.png") , plot = plotAccuracy_fun(Tab0gr4, "balanced"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1Gr8balanced.png") , plot = plotAccuracy_fun(Tab0gr8, "balanced"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1Gr16balanced.png") , plot = plotAccuracy_fun(Tab0gr16, "balanced"), width = 47, height = 23, units = "cm")

# Unbalanced split by nr groups: 4, 8, 16
# ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1Gr4unbalanced.png") , plot = plotAccuracy_fun(Tab0gr4, "unbalanced"), width = 47, height = 23, units = "cm")
# ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1Gr8unbalanced.png") , plot = plotAccuracy_fun(Tab0gr8, "unbalanced"), width = 47, height = 23, units = "cm")
# ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup1Gr16unbalanced.png") , plot = plotAccuracy_fun(Tab0gr16, "unbalanced"), width = 47, height = 23, units = "cm")

# Setup 2 and 3
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup2.png") , plot = plotAccuracy1_fun(Tab1, "setup2"), width = 47, height = 23, units = "cm")
ggsave(filename=paste0("analysis/plots/ARI/simulate_data/SimulateAccuracySetup3.png") , plot = plotAccuracy1_fun(Tab2, "setup3"), width = 47, height = 23, units = "cm")

# Run time
ggsave(filename=paste0("analysis/plots/Run_time/simulate_data/SimulateRunTimeSetup1.png") , plot = plotTime_fun(Tab0), width = 60, height = 35, units = "cm")


################################################################################
# RAW DATA ANALYSIS
################################################################################

#######################################
# Methods preprocessing: NONE
#######################################
rm(list=ls())
source("analysis/doTable.R")
source("analysis/plotARI.R")
source("analysis/plotTime.R")


folder="Results_raw"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

Tab$preproc=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
Tab=Tab[which(Tab$preproc=="none"),] 

# Complex Heatmap
#ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawComplexHeatmapNone.png") , plot = plotARIComplexHeatmap_fun(Tab, "all"), width = 60, height = 30, units = "cm", device="png")
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawComplexHeatmapNone_A.png") , plot = plotARIComplexHeatmap_fun(Tab, "A"), width = 35, height = 30, units = "cm", device="png")
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawComplexHeatmapNone_B.png") , plot = plotARIComplexHeatmap_fun(Tab, "B"), width = 35, height = 30, units = "cm", device="png")

Tab$dim_red=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
Tab=Tab[-which(Tab$Method=="sincell" & Tab$dim_red =="ICA"),] 

# Accuracy 
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawAccuracyNone.png") , plot = plotAccuracy2_fun(Tab), width = 39, height = 26, units = "cm")

# P-values
#ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawPvalNone.png") , plot = plotPval_fun(Tab), width = 36, height = 18, units = "cm")

# Estimation of nr clusters
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawEstimClustNone.png") , plot = plotEstimation_fun(Tab), width = 36, height = 22, units = "cm")


#######################################
# Methods preprocessing: METHOD SPECIFIC
#######################################
rm(list=ls())
source("analysis/doTable.R")
source("analysis/plotARI.R")
source("analysis/plotTime.R")

folder="Results_raw"
files=list.files(folder, pattern = ".Rdata",  recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

Tab$preproc=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
Tab=Tab[which(Tab$preproc=="methodspecific"),] #only method specific

# Complex Heatmap
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawComplexHeatmapMetSpec.png") , plot = plotARIComplexHeatmap_fun(Tab, "all"), width = 60, height = 30, units = "cm", device="png")

# Accuracy
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawAccuracyMetSpec.png") , plot = plotAccuracy2_fun(Tab), width = 40, height = 19, units = "cm")

# P-values
#ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawPvalMetSpec.png") , plot = plotPval1_fun(Tab), width = 36, height = 18, units = "cm")

# Estimation of nr clusters
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawEstimClustMetSpec.png") , plot = plotEstimation_fun(Tab), width = 36, height = 22, units = "cm")

#######################################
# Methods preprocessing: ALL
#######################################
rm(list=ls())
source("analysis/doTable.R")
source("analysis/plotARI.R")
source("analysis/plotTime.R")


folder="Results_raw"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

Tab$dim_red=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
Tab=Tab[-which(Tab$Method=="sincell" & Tab$dim_red =="ICA"),] 

Tab0=Tab[which(Tab$Name=="qc"),]
Tab1=Tab[which(Tab$Name=="qc_filt"),]
Tab2=Tab[which(Tab$Name=="qc_filt_norm"),]

# PCA
#ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawSummaryPCAQC.png") , plot = plotSummaryPCA_fun(Tab0, 2), width = 28, height = 10, units = "cm")
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawSummaryPCAQCFILT.png") , plot = plotSummaryPCA_fun(Tab1, 2), width = 28, height = 10, units = "cm")

# Correlation 
#ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawCorrelationPCAQC.png") , plot = plotCorrelation_fun(Tab0), width = 20, height = 15, units = "cm")
ggsave(filename=paste0("analysis/plots/ARI/raw_data/RawCorrelationPCAQCFILT.png") , plot = plotCorrelation_fun(Tab1), width = 20, height = 15, units = "cm")

source("analysis/plotTime.R")

# Run time
#ggsave(filename=paste0("analysis/plots/Run_time/raw_data/RawRunTimeQC.png") , plot = plotTime1_fun(Tab0), width = 40, height = 20, units = "cm")
ggsave(filename=paste0("analysis/plots/Run_time/raw_data/RawRunTimeQCFILT.png") , plot = plotTime1_fun(Tab1), width = 40, height = 20, units = "cm")
#ggsave(filename=paste0("analysis/plots/Run_time/raw_data/RawRunTimeQCFILTNORM.png") , plot = plotTime1_fun(Tab2), width = 40, height = 20, units = "cm")

################################################################################
# NORM DATA ANALYSIS
################################################################################
rm(list=ls())
source("analysis/doTable.R")
source("analysis/plotARI.R")
source("analysis/plotTime.R")

folder="Results_norm"
files=list.files(folder, pattern = ".Rdata", recursive = TRUE, full.names = TRUE)
Tab=doTable(files)

# ComplexHeatmap
ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormComplexHeatmap.png") , plot = plotARIComplexHeatmap1_fun(Tab), width = 50, height = 20, units = "cm")

Tab$dim_red=sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
Tab=Tab[-which(Tab$Method=="Linnorm" & Tab$dim_red=="tSNE"),]
Tab=Tab[-which(Tab$Method=="monocle3" & Tab$dim_red=="tSNE"),]
Tab=Tab[-which(Tab$Method=="sincell" & Tab$dim_red=="tSNE"),]
Tab=Tab[-which(Tab$Method=="Linnorm" & Tab$dim_red=="PCA"),]
Tab$dims <- paste0("dims_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4))

# Accuracy
ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormAccuracy.png") , plot = plotAccuracyPerMethod2_fun(Tab), width = 40, height = 27, units = "cm")

# P-values
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormPval.png") , plot = plotPval2_fun(Tab), width = 36, height = 18, units = "cm")

# Estimation of nr clusters
ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormEstimClust.png") , plot = plotEstimation1_fun(Tab), width = 36, height = 26, units = "cm")

Tab0=Tab[which(Tab$dims=="dims_3"),]
Tab1=Tab[which(Tab$dims=="dims_5"),]
Tab2=Tab[which(Tab$dims=="dims_10"),]
Tab3=Tab[which(Tab$dims=="dims_15"),]

# PCA , enforce positive correlation with ARI
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormSummaryPCAdims3.png") , plot = plotSummaryPCA1_fun(Tab0), width = 35, height = 10, units = "cm")
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormSummaryPCAdims5.png") , plot = plotSummaryPCA1_fun(Tab1), width = 35, height = 10, units = "cm")
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormSummaryPCAdims10.png") , plot = plotSummaryPCA1_fun(Tab2), width = 35, height = 10, units = "cm")
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormSummaryPCAdims15.png") , plot = plotSummaryPCA1_fun(Tab3), width = 35, height = 10, units = "cm")

# Correlation
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormCorrelationPCAdims3.png") , plot = plotCorrelation1_fun(Tab0), width = 20, height = 15, units = "cm")
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormCorrelationPCAdims5.png") , plot = plotCorrelation1_fun(Tab1), width = 20, height = 15, units = "cm")
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormCorrelationPCAdims10.png") , plot = plotCorrelation1_fun(Tab2), width = 20, height = 15, units = "cm")
#ggsave(filename=paste0("analysis/plots/ARI/norm_data/NormCorrelationPCAdims15.png") , plot = plotCorrelation1_fun(Tab3), width = 20, height = 15, units = "cm")

# Run time
ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims3.png") , plot = plotTime1_fun(Tab0), width = 40, height = 20, units = "cm")
# ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims5.png") , plot = plotTime1_fun(Tab1), width = 40, height = 20, units = "cm")
# ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims10.png") , plot = plotTime1_fun(Tab2), width = 40, height = 20, units = "cm")
# ggsave(filename=paste0("analysis/plots/Run_time/norm_data/NormRunTimedims15.png") , plot = plotTime1_fun(Tab3), width = 40, height = 20, units = "cm")









#####################################################
# GENERAL PURPOSE
#####################################################
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

install.packages("devtools")
library(devtools)

#####################################################
# CLUSTERING METHODS
#####################################################

#ascend
########################
cran_packages <- c("reshape2", "fields", "ggbeeswarm", "gridExtra",
                   "dynamicTreeCut", "dendextend", "RColorBrewer",
                   "locfit", "KernSmooth")
install.packages(cran_packages)
source("https://bioconductor.org/biocLite.R")
bioconductor_packages <- c("Biobase", "BiocGenerics", "BiocParallel",
                           "SingleCellExperiment", "GenomeInfoDb", "GenomeInfoDbData")
biocLite(bioconductor_packages)
install_github("IMB-Computational-Genomics-Lab/ascend")

#CIDR
########################
devtools::install_github("VCCRI/CIDR")

#Linnorm
########################
BiocManager::install("Linnorm", version = "3.8") 

#monocle3
########################
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("monocle") 
devtools::install_github("cole-trapnell-lab/DDRTree", ref="simple-ppt-like")
devtools::install_github("cole-trapnell-lab/L1-graph")
install.packages("reticulate")
library(reticulate)
py_install('umap-learn')  
py_install("louvain") 
devtools::install_github("cole-trapnell-lab/monocle-release", ref="monocle3_alpha")

#pcaReduce
########################
BiocManager::install("pcaMethods", version = "3.8")
install.packages("mnormt")
install.packages("mclust")
devtools::install_github("JustinaZ/pcaReduce")

#RaceID3
########################
install.packages("RaceID")

#SC3
########################
BiocManager::install("SC3", version = "3.8") 

#Seurat
########################
#sudo apt-get install libhdf5-dev
devtools::install_version(package = "Seurat", version = package_version('2.3.4')) 

#SIMLR
########################
BiocManager::install("SIMLR", version = "3.8")

#sincell
########################
BiocManager::install("sincell", version = "3.8")

#sscClust
########################
##You may need additional GNU GSL package
##To install it call: sudo apt install libgsl-dev
BiocManager::install("ComplexHeatmap", version = "3.8")
BiocManager::install("scran", version = "3.8")
BiocManager::install("zinbwave", version = "3.8")
devtools::install_github("Japrin/sscClust")

#TSCAN
########################
BiocManager::install("TSCAN", version = "3.8")

#DIMMSC 
########################
install.packages('roxygen2')
library(devtools)
library(roxygen2)
##DIMMSC require manual installation of cellrangerRkit
##Download zip and extract in home directory: https://github.com/hb-gitified/cellrangerRkit
##Then run -> install('/yourdirectory/cellrangerRkit-master')
devtools::install_github("wt2015-github/DIMMSC")

#####################################################
# DATA MANIPULATION & PLOTTING
#####################################################
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("GGally")
install.packages("dplyr")
install.packages("evaluate")
install.packages("mclust")
install.packages("tidyr")
BiocManager::install("ComplexHeatmap")
BiocManager::install("scater")
BiocManager::install("SingleCellExperiment")
BiocManager::install("splatter")
BiocManager::install("scran")




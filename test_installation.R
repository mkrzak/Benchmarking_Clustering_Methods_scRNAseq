
#Packages
packages <- c("ascend", "cidr", "Linnorm", "monocle", "reticulate", "pcaReduce", "RaceID", "SC3", "Seurat", "SIMLR", 
              "sincell", "sscClust", "TSCAN", "DIMMSC", "ggplot2", "ggfortify", "GGally", "dplyr", "ComplexHeatmap", 
              "evaluate", "scater", "mclust", "SingleCellExperiment", "splatter", "tidyr", "scran", "WriteXLS")

for(i in 1:length(packages)){
if (length(setdiff(packages[i], rownames(installed.packages()))) > 0) {
  print(paste0("You dont have package: ", packages[i])) 
  } else {
    print(paste0("Everything is fine for package: ", packages[i]))
}
}

#Modules
modules <- c("louvain")
for(i in 1:length(modules)){
  library(reticulate)
  if (py_module_available(modules[i])==TRUE) {
    print(paste0("You have module: ", modules[i])) 
  } else {
    print(paste0("You dont have module: ", modules[i]))
  }
}



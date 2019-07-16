### DATA
data <- read.csv("raw_data/temp/data.csv")

### ANNOTATIONS
genes <- unlist(lapply(strsplit(as.character(data[,1]), "_"), "[[", 2))
data <- data[!duplicated(genes), ]
rownames(data) <- genes[!duplicated(genes)]
data <- data[,2:ncol(data)]
# metadata
ann <- data.frame(Group = unlist(lapply(strsplit(colnames(data), "__"), "[[", 2)))
rownames(ann) <- colnames(data)

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce<- create_sce(data, ann)
save(sce, file="raw_data/Rdata/Li2017.Rdata")

system("rm raw_data/temp/*")

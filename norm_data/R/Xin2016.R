### DATA
d <- read.table("norm_data/temp/data.txt", header = T)
genes <- read.csv("norm_data/temp/human_gene_annotation.csv", header = T)
rownames(d) <- genes[,2]
d <- d[,2:ncol(d)]

### ANNOTATIONS
ann <- read.table("norm_data/temp/human_islet_cell_identity.txt", header = T, sep = "\t", stringsAsFactors = F)
rownames(ann) <- ann[,1]
rownames(ann) <- gsub(" ", "_", rownames(ann))
ann <- ann[,9:ncol(ann)]
colnames(ann)[length(colnames(ann))] <- "Group"
# format cell type names
ann$Group[ann$Group == "PP"] <- "gamma"
ann$Group[ann$Group == "PP.contaminated"] <- "gamma.contaminated"

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(d, ann) #norm
save(sce, file="norm_data/Rdata/Xin2016.Rdata")

system("rm norm_data/temp/*")
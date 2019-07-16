### DATA
rpkm <- read.csv("norm_data/temp/genes_rpkm.csv")
raw <- read.csv("norm_data/temp/genes_counts.csv")

rownames(rpkm) <- rpkm[,1]
rownames(raw) <- raw[,1]

rpkm <- rpkm[,2:ncol(rpkm)]
raw <- raw[,2:ncol(raw)]

erccs <- read.csv("norm_data/temp/ercc_counts.csv")
rownames(erccs) <- erccs[,1]
erccs <- erccs[,2:ncol(erccs)]
raw <- rbind(raw, erccs)

### ANNOTATIONS
ann <- read.csv("norm_data/temp/cell_metadata.csv")
cell_classification <- read.csv("norm_data/temp/cell_classification.csv")
cluster_metadata <- read.csv("norm_data/temp/cluster_metadata.csv")
# process metadata
cluster_metadata <- cluster_metadata[,1:(ncol(cluster_metadata) - 1)]
ann <- cbind(ann, cell_classification[,2:3])
colnames(ann)[ncol(ann)] <- "cluster_id"
ann <- merge(ann, cluster_metadata, by = "cluster_id")
ann <- ann[order(ann$long_name), ]
rownames(ann) <- ann$long_name
ann <- ann[,c(1, 3:ncol(ann))]
# manually define cell types
colnames(ann)[grep("group", colnames(ann))] <- "Group"
colnames(ann)[grep("Tasic_et_al_2016_label", colnames(ann))] <- "cell_type2"

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(rpkm, ann)
save(sce, file="norm_data/Rdata/Tasic2016_rpkm.Rdata")

system("rm norm_data/temp/*")




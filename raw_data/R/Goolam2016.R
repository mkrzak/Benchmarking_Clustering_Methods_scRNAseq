### DATA
data <- read.table("raw_data/temp/Goolam_et_al_2015_count_table.tsv", header = T)

### ANNOTATIONS
labs <- colnames(data)
labs[grep("4cell",labs)] = "4cell"
labs[grep("8cell",labs)] = "8cell"
labs[grep("16cell",labs)] = "16cell"
labs[grep("32cell",labs)] = "32cell"
labs[grep("2cell",labs)] = "2cell"
ann <- data.frame(Group = labs)
rownames(ann) <- colnames(data)
colnames(data) <- rownames(ann)

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data, ann)
save(sce, file="raw_data/Rdata/Goolam2016.Rdata")

system("rm raw_data/temp/*")

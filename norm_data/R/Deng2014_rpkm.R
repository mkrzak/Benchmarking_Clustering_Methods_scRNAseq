### DATA
data_rpkm <- read.table("norm_data/temp/deng-rpkms.txt", check.names = F, header = T)
genes <- data_rpkm[ , 1]
data_rpkm <- as.matrix(data_rpkm[ , 2:ncol(data_rpkm)])
rownames(data_rpkm) <- genes
cell_id <- colnames(data_rpkm)

### ANNOTATIONS
labs <- unlist(lapply(strsplit(cell_id, "\\."), "[[", 1))
ann <- data.frame(cell_type2 = labs)

#annotate cell populations from early , mid and late as the same type 
labs[labs == "zy" | labs == "early2cell"] = "zygote"
labs[labs == "mid2cell" | labs == "late2cell"] = "2cell"
labs[labs == "earlyblast" | labs == "midblast" | labs == "lateblast"] = "blast"
ann$Group <- labs
rownames(ann) <- cell_id

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data_rpkm, ann)
save(sce, file="norm_data/Rdata/Deng2014_rpkm.Rdata")
system("rm norm_data/temp/*")



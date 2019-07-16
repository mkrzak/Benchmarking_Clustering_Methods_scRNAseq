data_raw <- read.table("raw_data/temp/deng-reads.txt", check.names = F, header = T)
genes <- data_raw[ , 1]
data_raw <- as.matrix(data_raw[ , 2:ncol(data_raw)])
rownames(data_raw) <- genes
cell_id <- colnames(data_raw)

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

sce <- create_sce(data_raw, ann)
save(sce, file="raw_data/Rdata/Deng2014_raw.Rdata")

system("rm raw_data/temp/*")

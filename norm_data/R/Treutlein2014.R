### DATA
d <- read.table("norm_data/temp/nature13173-s4.txt")
d <- t(d)
genes <- d[,1][5:nrow(d)]
# remove genes and bulk samples
d <- d[,2:(ncol(d) - 2)]
exprs_data <- as.data.frame(matrix(as.numeric(d[5:nrow(d),]), ncol = ncol(d)))
rownames(exprs_data) <- genes
colnames(exprs_data) <- d[1,]

### ANNOTATIONS
ann <- data.frame(Group = d[4,])
rownames(ann) <- d[1,]

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(exprs_data, ann)
save(sce, file="norm_data/Rdata/Treutlein2014.Rdata")

system("rm norm_data/temp/*")
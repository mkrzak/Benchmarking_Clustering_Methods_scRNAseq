### DATA
data <- read.table("raw_data/temp/counttable_es.csv")
data <- data[1:(nrow(data) - 5), ]

### ANNOTATIONS
ann <- data.frame(
    Group = unlist(lapply(strsplit(colnames(data), "_"), "[[", 3)),
    batch = paste(
        unlist(lapply(strsplit(colnames(data), "_"), "[[", 3)),
        unlist(lapply(strsplit(colnames(data), "_"), "[[", 4)),
        sep = "_"
    )
)
rownames(ann) <- colnames(data)
colnames(data) <- rownames(ann)

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data, ann)
save(sce, file="raw_data/Rdata/Kolodziejczyk2015.Rdata")

system("rm raw_data/temp/*")

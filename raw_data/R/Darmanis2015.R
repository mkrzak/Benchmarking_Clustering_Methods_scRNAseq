### DATA
data1 <- read.table("raw_data/temp/ExprMat1.txt", header=T)
ann1 <- read.table("raw_data/temp/Ann_part1.txt", header=T)
data2 <- read.table("raw_data/temp/ExprMat2.txt", header=T)
ann2 <- read.table("raw_data/temp/Ann_part2.txt", header=T)
data <- cbind(data1, data2)
ann <- rbind(t(ann1), t(ann2))

### ANNOTATIONS
rownames(ann) <- NULL
colnames(ann) <- c("Source", "Species", "Tissue", "Group", "age", "plate", "individual", "File")

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data, ann)
save(sce, file="raw_data/Rdata/Darmanis2015.Rdata")

system("rm raw_data/temp/*")

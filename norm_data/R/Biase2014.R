### DATA
data <- read.table("norm_data/temp/data.txt", header = T)
data <- data[!duplicated(data$ID),]
rownames(data) <- data$ID
data <- data[,2:ncol(data)]

### ANNOTATIONS
ann <- read.table("norm_data/temp/biase_cell_types.txt", stringsAsFactors = F)
names(ann)[names(ann) == "cell_type1"] <- "Group"

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data=data,colData=ann)
save(sce, file="norm_data/Rdata/Biase2014.Rdata")

system("rm norm_data/temp/*")
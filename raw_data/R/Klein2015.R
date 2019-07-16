### DATA
d0 <- read.csv("raw_data/temp/new/GSM1599494_ES_d0_main.csv", header = FALSE)
d2 <- read.csv("raw_data/temp/new/GSM1599497_ES_d2_LIFminus.csv", header = FALSE)
d4 <- read.csv("raw_data/temp/new/GSM1599498_ES_d4_LIFminus.csv", header = FALSE)
d7 <- read.csv("raw_data/temp/new/GSM1599499_ES_d7_LIFminus.csv", header = FALSE)
data <- cbind(d0, d2[,2:ncol(d2)], d4[,2:ncol(d4)], d7[,2:ncol(d7)])
rownames(data) <- data[,1]
data <- data[,2:ncol(data)]
colnames(data) <- paste0("cell", 1:ncol(data))

### ANNOTATIONS
ann <- data.frame(
    Group = c(rep("d0", ncol(d0) - 1), rep("d2", ncol(d2) - 1),rep("d4", ncol(d4) - 1),rep("d7", ncol(d7) - 1)))

rownames(ann) <- colnames(data)

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce<- create_sce(data, ann)
save(sce, file="raw_data/Rdata/Klein2015.Rdata")

system("rm -r raw_data/temp/new")
system("rm raw_data/temp/*")




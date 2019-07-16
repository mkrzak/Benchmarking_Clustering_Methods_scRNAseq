### DATA
data <- read.table("norm_data/temp/pancreas_refseq_rpkms_counts_3514sc.txt", stringsAsFactors = F)
data <- data[!duplicated(data[,1]), ]
rownames(data) <- data[,1]
data <- data[,3:ncol(data)]
data <- data[,3515:7028]
labs <- read.table("norm_data/temp/labels.txt", stringsAsFactors = F, header = F)
labs <- as.character(labs)
colnames(data) <- labs
data <- data[,order(colnames(data))]
# remove eGFP row
data <- data[1:(nrow(data) - 1), ]

### ANNOTATIONS
ann <- read.table("norm_data/temp/E-MTAB-5061.sdrf.txt", stringsAsFactors = F, header = T, sep = "\t")
rownames(ann) <- ann$Extract.Name
ann <- ann[order(rownames(ann)), ]
ann <- ann[,7:11]
colnames(ann) <- c("cell_quality", "Group", "disease", "sex", "age")
# format cell type names
ann$Group <- unlist(lapply(strsplit(ann$Group, " cell"), "[[", 1))

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data, ann)
save(sce, file="norm_data/Rdata/Segerstolpe2016.Rdata")

system("rm norm_data/temp/*")



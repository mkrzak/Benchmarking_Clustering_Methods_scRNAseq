### DATA
x1 = read.delim("raw_data/temp/GSE74672_expressed_mols_with_classes.csv", ",", header=T, stringsAsFactors=FALSE)
info <-x1[1:11,]
x1 <- x1[-1*(1:11),]
rownames(x1) <- x1[,1];
x1 <- x1[,-1];
tmp <- info[,1]
info <- info[,-1]

### ANNOTATIONS
SOURCE <- rep("hypothalamus", times=length(x1[1,]))
#BATCH <- as.character(unlist(ann[,4]))
TYPE1 <- as.character(unlist(info[1,]))
TYPE2 <- as.character(unlist(info[2,]))
TYPE3 <- as.character(unlist(info[3,]))
AGE <- as.character(unlist(info[4,]))
STATE <- as.character(unlist(info[7,]))
SEX <- as.character(unlist(info[5,]))
SEX[SEX == -1] = "M"
SEX[SEX == 1] = "F"
SEX[SEX == 0] = "?"
STATE[STATE==1] = "Stressed"
STATE[STATE==0] = "Normal"
stuff <- matrix(unlist(strsplit(colnames(info), "_")), ncol=2, byrow=T)
WELL <- stuff[,2]
PLATE=stuff[,1]
data = apply(x1,2,as.numeric);
rownames(data) = rownames(x1)
colnames(data) = colnames(x1);
ann <- data.frame(Species=rep("Mus musculus", times=length(TYPE1)), Group=TYPE1, cell_type2=TYPE2, clusterID=TYPE3, Source=SOURCE, age=AGE, WellID=WELL, State=STATE, sex=SEX, Plate=PLATE, WellID=WELL)
rownames(ann) <- colnames(x1)

### SINGLECELLEXPERIMENT
source("utils/create_sce.R")
sce <- create_sce(data, ann)
save(sce, file="raw_data/Rdata/Romanov2016.Rdata")

system("rm raw_data/temp/*")

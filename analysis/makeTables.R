system("mkdir analysis/Tabs")
system("mkdir analysis/SuppTabs")

install.packages("xlsx")
library("xlsx")

###################################################
# MAIN TABLES
###################################################

#Datasets_main_information

T1=t(matrix(data=c("Single cell dataset" , "Organism" , "Cells under study" , "Protocol" , "Accession "        ,
"Baron2016_m"         , "Mouse"             , "Pancreas"                   , "inDrop"            , "GSE84133" ,          
"Klein2015"           , "Mouse"             , "Embryonic stem cells"       , "inDrop"            , "GSE65525"  ,         
"Zeisel2015"          , "Mouse"             , "Cerebral cortex"            , "STRT/C1 UMI"        , "GSE60361"    ,     
"Darmanis2015"        , "Human"             , "Brain"                      , "SMARTer"           , "GSE67835"    ,        
"Deng2014_raw"        , "Mouse"             , "Preimplantation embryos"    , "Smart-Seq"         , "GSE45719"     ,       
"Goolam2016"          , "Mouse"             , "Early embryos"              , "Smart-Seq2"        , "E-MTAB-3321"  ,       
"Kolodziejczyk2015"   , "Mouse"             , "Stem cells"                 , "SMARTer"           , "E-MTAB-2600"  ,       
"Li2017"              , "Human"             , "Colorectal tumors"          , "SMARTer"           , "GSE81861"     ,       
"Romanov2016"         , "Mouse"             , "Hypothalamus"               , "C1"                , "GSE74672"      ,      
"Tasic2016_raw"       , "Mouse"             , "Brain"                      , "SMARTer"           , "GSE71585"     ,       
"Deng2014_rpkm"       , "Mouse"             , "Preimplantation embryos"    , "Smart-Seq"         , "GSE45719"     ,       
"Segerstolpe2016"     , "Human"             , "Pancreas"                   , "Smart-Seq2"        , "E-MTAB-5061"  ,       
"Tasic2016_rpkm"      , "Mouse"             , "Brain"                      , "SMARTer"           , "GSE71585"     ,       
"Xin2016"             , "Human"             , "Pancreas"                   , "SMARTer"           , "GSE81608"     ,       
"Yan2013"             , "Human"             , "Preimplantation embryos"    , "Tang"              , "GSE36552"     ,       
"Biase2014"           , "Mouse"             , "Embryos"                    , "SMARTer"           , "GSE57249"     ,       
"Treutlein2014"       , "Mouse"             , "Lung epithelial cells"      , "SMARTer"           , "GSE52583"), nrow = 5, ncol=18))          

write.table(T1, file="Datasets_main_information.xls")



#Datasets_types_main_features
#Methods_general_information
#Valid_parameter_settings







###################################################
# SUPPLEMENTARY TABLES
###################################################



raw=list.files("raw_data/Rdata", full.names = TRUE)
raw_qc=list.files("raw_data/Rdata_qc", full.names = TRUE)
raw_qc_filt=list.files("raw_data/Rdata_qc_filt", full.names = TRUE)

data=c("raw"=unlist(raw), "raw_qc"=unlist(raw_qc), "raw_qc_filt"=unlist(raw_qc_filt))

y=list()
nr_cells=list()
nr_genes=list()
for(i in 1:length(data)){
  load(data[i])
  y[i]=data[i]
  nr_cells[i]=dim(sce)[2]
  nr_genes[i]=dim(sce)[1]
  
}

y2=sapply(strsplit(as.character(y), "/"), "[[", 2)
y3=gsub(".Rdata", "", sapply(strsplit(as.character(y), "/"), "[[", 3))

t1=data.frame(y2,y3, "nr_cells"=unlist(nr_cells))
t2=data.frame(y2,y3, "nr_genes"=unlist(nr_genes))

library(reshape2)
t1 <- acast(t1, y3~y2)
t2 <- acast(t2, y3~y2)

t=merge(t1,t2,by=0)
names(t) <- c("Dataset", "Nr cells", "Nr cells after QC", "Nr cells after QC & FILT", "Nr genes", "Nr genes after QC", "Nr genes after QC & FILT")

order=read.csv("analysis/Datasets.csv")[1:11,1]
write.csv(t[match(order, t$Dataset),], file="analysis/Tabs/datasets_sizes_raw.csv", row.names = FALSE)

rm(list=ls())

###############Norm counts
norm=list.files("norm_data/Rdata", full.names = TRUE)
norm_qc_filt_hvg=list.files("norm_data/Rdata_qc_filt_hvg", full.names = TRUE)
data=c("norm"=unlist(norm), "norm_qc_filt_hvg"=unlist(norm_qc_filt_hvg))

y=list()
nr_cells=list()
nr_genes=list()
for(i in 1:length(data)){
  load(data[i])
  y[i]=data[i]
  nr_cells[i]=dim(sce)[2]
  nr_genes[i]=dim(sce)[1]
  
}

y2=sapply(strsplit(as.character(y), "/"), "[[", 2)
y3=gsub(".Rdata", "", sapply(strsplit(as.character(y), "/"), "[[", 3))

t1=data.frame(y2,y3, "nr_cells"=unlist(nr_cells))
t2=data.frame(y2,y3, "nr_genes"=unlist(nr_genes))

library(reshape2)
t1 <- acast(t1, y3~y2)
t2 <- acast(t2, y3~y2)

t=merge(t1,t2,by=0)
names(t) <- c("Dataset", "Nr cells", "Nr cells after QC & FILT & HVG", "Nr genes", "Nr genes after QC & FILT & HVG")

order=read.csv("analysis/Datasets.csv")[12:18,1]
write.csv(t[match(order, t$Dataset),], file="analysis/Tabs/datasets_sizes_norm.csv", row.names = FALSE)


###############Raw counts, Raw UMI counts
#Datasets main features

raw=list.files("raw_data/Rdata", full.names = TRUE)
norm=list.files("norm_data/Rdata", full.names = TRUE)

data=c("raw"=unlist(raw), "norm"= unlist(norm))

y=list()
nr_cells=list()
nr_groups=list()
for(i in 1:length(data)){
  load(data[i])
  y[i]=data[i]
  nr_cells[i]=dim(sce)[2]
  nr_groups[i]=length(unique(sce$Group))
}


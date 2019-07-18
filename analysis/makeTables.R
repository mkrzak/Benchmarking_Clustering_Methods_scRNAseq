system("mkdir analysis/SuppTabs")

library(WriteXLS)

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

t=merge(t1,t2,by=0)[-3]
names(t) <- c("Dataset", "Nr cells", "Nr cells after QC", "Nr genes", "Nr genes after QC", "Nr genes after QC & FILT")

order <- c("Baron2016_m", "Klein2015", "Zeisel2015", "Darmanis2015" , "Deng2014_raw", "Goolam2016", "Kolodziejczyk2015", "Li2017", "Romanov2016", "Tasic2016_raw")
t_ordered <- t[match(order, t$Dataset),]
t_ordered$Dataset <- c(paste0(t_ordered$Dataset[1], "*"), paste0(t_ordered$Dataset[2], "*"), paste0(t_ordered$Dataset[3], "*"), 
t_ordered$Dataset[4:8], paste0(t_ordered$Dataset[9], "*"), t_ordered$Dataset[10])
WriteXLS(t_ordered, ExcelFileName = "analysis/SuppTabs/Datasets_sizes_raw.xls", row.names = FALSE)

rm(list=ls())

###################################################

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

order <- c("Deng2014_rpkm", "Segerstolpe2016", "Tasic2016_rpkm", "Xin2016", "Yan2013", "Biase2014", "Treutlein2014")
t_ordered <- t[match(order, t$Dataset),]
WriteXLS(t_ordered, ExcelFileName = "analysis/SuppTabs/Datasets_sizes_norm.xls", row.names = FALSE)


###################################################
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      

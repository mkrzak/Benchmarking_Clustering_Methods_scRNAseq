
# combine counts from two mouse
m1 <- read.csv("raw_data/temp/GSM2230761_mouse1_umifm_counts.csv", header = T)
m2 <- read.csv("raw_data/temp/GSM2230762_mouse2_umifm_counts.csv", header = T)

rownames(m1) <- m1[,1]
rownames(m2) <- m2[,1]

labels_m1 <- as.character(m1$assigned_cluster)
labels_m2 <- as.character(m2$assigned_cluster)

m1 <- m1[,4:ncol(m1)]
m2 <- m2[,4:ncol(m2)]

m1 <- t(m1)
m2 <- t(m2)

data <- cbind(m1, m2)

ann <- data.frame(mouse = c(rep(1, length(labels_m1)), rep(2, length(labels_m2))),
                  Group = c(labels_m1, labels_m2))

rownames(ann) <- colnames(data)

source("utils/create_sce.R")
sce <- create_sce(data=data,colData=ann )
save(sce, file="raw_data/Rdata/Baron2016_m.Rdata")
rm(list=ls())

system("rm raw_data/temp/*")



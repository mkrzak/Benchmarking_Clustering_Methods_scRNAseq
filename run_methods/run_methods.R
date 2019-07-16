#data - provide matrix of counts
#method - select method: "ascend", "CIDR", "DIMMSC", "Linnorm", "monocle2", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR", "sincell", "sscClust", "TSCAN" 
#preproc - select among: "none", "method_specific"
#dim_red - select dimension reduction technique: "default", "none", "tSNE", "PCA", "UMAP", "ICA", "classical-MDS", "nonmetric-MDS", "iCor"
#use_dims_input - select among: "FALSE", "TRUE", "default"
#dims_input - input number of reduced dimensions (if dim_red is one of the options except "none")
#clust_tech - select clustering technique: "default", "hclust", "densityPeak", "louvain", "k-medoids", "k-means", "max.distance", "percent", "knn", "ward.D", "SNN", "ADPclust"
#clust - select among: "set", "estimate" 
#k_input - input number of desired clusters (if clust is "set") 


run_methods <- function(data, method, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){

set.seed(seed=100)    
    
source(paste0("run_methods/methods/run_", method ,".R"))
switch(method, "ascend"= run_ascend(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "CIDR"=run_CIDR(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "DIMMSC"=run_DIMMSC(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "Linnorm"=run_Linnorm(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "monocle3"=run_monocle3(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "pcaReduce"=run_pcaReduce(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "RaceID3"=run_RaceID3(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "SAFE"=run_SAFE(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "SC3"=run_SC3(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "Seurat"=run_Seurat(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "SIMLR"=run_SIMLR(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "sincell"=run_sincell(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "sscClust"=run_sscClust(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input),
       "TSCAN"=run_TSCAN(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input)
       )
}





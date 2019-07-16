run_Seurat <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){
    library(Seurat)
  
    
    if( preproc != "method_specific" || (dim_red!="PCA" && dim_red!="ICA") || (use_dims_input!="TRUE" && use_dims_input!="internal") ||  clust_tech!="fixed" || clust!="estimate"){
        stop("Provide correct parameters !")
    }
    
   if(preproc=="method_specific"){
        cso <- Seurat::CreateSeuratObject(raw.data = data, min.cells = 5)  #filter
        cso <- Seurat::NormalizeData(cso, normalization.method = "LogNormalize") #normalize
    }
    
    cso <- Seurat::ScaleData(object = cso, vars.to.regress = c("nUMI")) #scaling data is a must
    
    if(dim_red=="PCA" && use_dims_input=="TRUE"){
        cso <- Seurat::RunPCA(object = cso, pc.genes=rownames(cso@raw.data))
        cso <- Seurat::FindClusters(object = cso, reduction.type="pca", dims.use = dims_input)
    }
    
    else if(dim_red=="PCA" && use_dims_input=="internal"){
        cso <- Seurat::RunPCA(object = cso, pc.genes=rownames(cso@raw.data))
        cso <- Seurat::FindClusters(object = cso, reduction.type="pca")
    }
    
    else if(dim_red=="ICA" && use_dims_input=="TRUE"){
        cso <- Seurat::RunICA(object = cso, ic.genes=rownames(cso@raw.data))
        cso <- Seurat::FindClusters(object = cso, reduction.type="ica", dims.use = dims_input) 
    }
    
    else if(dim_red=="ICA" && use_dims_input=="internal"){
        cso <- Seurat::RunICA(object = cso, ic.genes=rownames(cso@raw.data))
        cso <- Seurat::FindClusters(object = cso, reduction.type="ica") 
    }

    clusters <- as.numeric(cso@ident)
    detach(package:Seurat)
    return(clusters)
}

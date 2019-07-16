run_monocle3 <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){

    
    library(monocle)
    library(reticulate)
    import("louvain")
    
    if( (preproc != "none" && preproc != "method_specific")|| (dim_red!="UMAP" && dim_red!="tSNE") || (use_dims_input!="TRUE" &&  use_dims_input!="internal") || (clust_tech!="densityPeak" && clust_tech!="louvain") || clust != "estimate"){
        stop("Provide correct parameters !")
    }
    
    col_info=data.frame(colnames(data))
    rownames(col_info)<- colnames(data)
    pd <- new("AnnotatedDataFrame", data = col_info)
    gene_info=data.frame(rownames(data))
    names(gene_info)="gene_short_name"
    rownames(gene_info)<- rownames(data)
    fd <- new("AnnotatedDataFrame", data = gene_info)
    cds <- monocle::newCellDataSet(cellData = data, phenoData = pd, featureData = fd)

    cds <- estimateSizeFactors(cds)
    cds <- estimateDispersions(cds)
    
    if(preproc=="none"){
        cds <- monocle::preprocessCDS(cds, method = "none",  norm_method = "none", relative_expr=FALSE, scaling=FALSE) #Log normalize dataset
    }
    
    else if(preproc=="method_specific"){
        cds <- monocle::preprocessCDS(cds, method = "none",  norm_method = "log") #Log normalize dataset
    }

    if(use_dims_input=="TRUE"){
        cds <- monocle::reduceDimension(cds, reduction_method = dim_red, max_components=dims_input)
    }
    
    else if(use_dims_input=="internal"){
        cds <- monocle::reduceDimension(cds, reduction_method = dim_red)
    }
    
    
    cds <- monocle::clusterCells(cds, num_clusters = NULL, method = clust_tech) 

    clusters <- as.numeric(levels(cds$Cluster))[cds$Cluster]
    detach(package:monocle)
    return(clusters)
}
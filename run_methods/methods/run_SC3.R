run_SC3 <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){
    library(SC3)
 
    
    if( (preproc != "none" && preproc != "method_specific") || dim_red!="internal" || use_dims_input!="internal" ||  clust_tech!="fixed" || (clust != "set" && clust!="estimate")){
        stop("Provide correct parameters !")
    }
    
    sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = data))
    rowData(sce)$feature_symbol <- rownames(data)
    assay(sce, "logcounts") <- counts(sce) #you just put counts in the slot of log counts, without method doesnt work
    
    if(preproc=="none"){
        sce <- SC3::sc3_prepare(sce, gene_filter=FALSE) 
    }
    
    else if(preproc=="method_specific"){
        sce <- SC3::sc3_prepare(sce, gene_filter=TRUE) #filter 
    }
     
    if(clust=="estimate"){
        sce <- SC3::sc3_estimate_k(sce)  
        k_input <- metadata(sce)$sc3$k_estimation
    }
   
    sce <- SC3::sc3_calc_dists(sce)
    sce <- SC3::sc3_calc_transfs(sce)
    sce <- SC3::sc3_kmeans(sce, ks = k_input)
    sce <- SC3::sc3_calc_consens(sce)
    
    clusters=as.numeric(colData(sce)[paste0("sc3_",k_input, "_clusters")][[1]])

    # detach(package:SC3)
    return(clusters)
}

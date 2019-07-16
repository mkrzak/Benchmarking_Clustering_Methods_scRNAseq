run_TSCAN <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input, use_k_range, k_range){

    library(TSCAN)
    
    if( (preproc != "method_specific") || dim_red!="internal" || use_dims_input!="internal" ||  clust_tech!="fixed" || (clust!="set" && clust!="estimate")){
        stop("Provide correct parameters !")
    }
  
    
    if(preproc=="method_specific"){
        data <- TSCAN::preprocess(data, takelog = TRUE, logbase = 2, cvcutoff = 0.1) #filter and normalize
    }
    
    if(clust=="set"){
        tsc <- TSCAN::exprmclust(data, clusternum = k_input, reduce = T)
    }
    
    else if(clust=="estimate"){
        tsc <- TSCAN::exprmclust(data, reduce = T) #PCA
    }
    
    clusters <- as.vector(tsc$clusterid)
    detach(package:TSCAN)
    return(clusters)

} 
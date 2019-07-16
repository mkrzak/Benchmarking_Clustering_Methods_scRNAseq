# Use capture.output({ }) to not show package messages

run_ascend <- function(data, preproc, dim_red, use_dims_input, dims_input, clust_tech, clust, k_input){

    library(ascend)
    
    em <- ascend::newEMSet(assays = list(counts = data))
    
    if( (preproc != "method_specific") || dim_red!="internal" || (use_dims_input!="TRUE" && use_dims_input!="internal") ||  clust_tech!="fixed" || clust != "estimate"){
        stop("Provide correct parameters !")
    }
    
    if(preproc=="method_specific"){
        print("Method specific preprocessing is performed")
    em <- ascend::filterLowAbundanceGenes(em) #filters
    em <- ascend::normaliseByRLE(em) #normalize 
    }
    
    em <- ascend::runPCA(em)
    
    if(use_dims_input=="TRUE"){
        em <- ascend::runCORE(em, dims = dims_input)
    }
    
    else if(use_dims_input=="internal"){
        em <- ascend::runCORE(em, nres=40)
    }
    
    clusters <- ascend::clusterAnalysis(em)$clusters
    detach(package:ascend)
    return(clusters)
}







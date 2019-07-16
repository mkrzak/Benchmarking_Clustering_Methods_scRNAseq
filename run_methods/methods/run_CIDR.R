run_CIDR <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){

    library(cidr)
    
    if( preproc != "none" || dim_red!="internal" || (use_dims_input!="TRUE" && use_dims_input!="internal") ||  clust_tech!="fixed" || (clust != "set" && clust != "estimate")){
        stop("Provide correct parameters !")
    }

    cid <- cidr::scDataConstructor(data, tagType = "raw") 
    cid <- cidr::determineDropoutCandidates(cid)
    cid <- cidr::wThreshold(cid)
    cid <- cidr::scDissim(cid, threads = 1)
    cid <- cidr::scPCA(cid, plotPC=FALSE)
    
    if(clust=="set" && use_dims_input=="TRUE" ){
        cid <- cidr::scCluster(object = cid, nCluster = k_input, nPC = dims_input) 
    }
    
    else if(clust=="set" && use_dims_input=="internal"){
        cid <- cidr::scCluster(object = cid, nCluster = k_input , nPC=4) 
    }
    
    else if(clust=="estimate" && use_dims_input=="TRUE"){
        cid <- cidr::scCluster(object = cid, nCluster = NULL, nPC = dims_input) 
    }
        
    
    else if(clust=="estimate" && use_dims_input=="internal"){
        cid <- cidr::scCluster(object = cid, nCluster = NULL, nPC=4) 
    }
    
    detach(package:cidr)
    clusters <- cid@clusters
    return(clusters)
}

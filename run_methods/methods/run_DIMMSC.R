run_DIMMSC <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){

    library(DIMMSC)
  
    if(preproc != "none" || dim_red!="none" || use_dims_input!="FALSE" ||  clust_tech!="fixed" || clust != "set"){
        stop("Provide correct parameters !")
    }
    
    dimm = DIMMSC::DIMMSC(data, K = k_input)
    clusters <- as.vector(dimm$mem)
    detach(package:DIMMSC)
    return(clusters)
}


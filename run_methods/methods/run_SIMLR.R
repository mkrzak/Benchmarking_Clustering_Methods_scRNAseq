run_SIMLR <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){
    
    library(SIMLR)
    
    if( (preproc != "none" && preproc!= "method_specific") || dim_red!="internal"  || (use_dims_input!="TRUE" && use_dims_input!="internal") ||  clust_tech!="fixed" || (clust!="set" && clust!="estimate")){
        stop("Provide correct parameters !")
    }
    
    if(clust=="estimate"){
    est = SIMLR::SIMLR_Estimate_Number_of_Clusters(data, NUMC = 2:5) #2:5 is default
    k_input = (2:5)[which.min(est$K1)]
    }
  
    if(preproc=="none" && use_dims_input=="TRUE"){
    siml <- SIMLR::SIMLR(X = data, c = k_input, normalize = FALSE, no.dim=dims_input)
    }
  
    else if(preproc=="none" && use_dims_input=="internal"){
    siml <- SIMLR::SIMLR(X = data, c = k_input, normalize = FALSE, no.dim=NA)
    }
  
    else if(preproc=="method_specific" && use_dims_input=="TRUE"){
      siml <- SIMLR::SIMLR(X = data, c = k_input, normalize = TRUE, no.dim=dims_input)
    }
    
    else if(preproc=="method_specific" && use_dims_input=="internal"){
    siml <- SIMLR::SIMLR(X = data, c = k_input, normalize = TRUE, no.dim=NA)
    }
  
    clusters <- siml$y$cluster
    detach(package:SIMLR)
    return(clusters)
}
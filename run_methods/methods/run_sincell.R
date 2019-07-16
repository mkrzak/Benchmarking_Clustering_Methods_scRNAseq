run_sincell <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){
 
    library(sincell)
    
    if( preproc != "none" || (dim_red!="PCA" && dim_red!="ICA" && dim_red!="tSNE" && dim_red!="classical-MDS" && dim_red!="nonmetric-MDS" ) || (use_dims_input!="TRUE" && use_dims_input!="internal") ||  (clust_tech!="max.distance" && clust_tech!="percent" && clust_tech!="knn" && clust_tech!="k-medoids" && clust_tech!="ward.D") || clust!="estimate"){
        stop("Provide correct parameters !")
    }
    
    sinc <- sincell::sc_InitializingSincellObject(data) #removes genes with zero variance
    
    if(use_dims_input=="TRUE"){
        sinc <- sincell::sc_DimensionalityReductionObj(sinc, method=dim_red, dim=dims_input)
    }
    
    else if(use_dims_input=="internal"){
        sinc <- sincell::sc_DimensionalityReductionObj(sinc, method=dim_red)
    }
    
    sinc <- sincell::sc_clusterObj(sinc, clust.method=clust_tech) 
    
    clusters <- igraph::clusters(sinc[["cellsClustering"]])$membership
    detach(package:sincell)
    return(clusters)
}
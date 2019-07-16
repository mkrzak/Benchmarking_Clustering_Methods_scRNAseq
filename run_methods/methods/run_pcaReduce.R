run_pcaReduce <- function(data, preproc, dim_red, use_dims_input, dims_input, clust_tech, clust, k_input){
   
    library(pcaReduce)

    if( preproc != "none"|| dim_red!="internal" || use_dims_input!="internal" ||  clust_tech!="fixed" || clust != "set"){
        stop("Provide correct parameters !")
    }

    pcar <- pcaReduce::PCAreduce(t(data), nbt = 1, q = k_input-1  , method = 'M')
    clusters <- as.vector(pcar)[[1]][,1]
    # detach(package:pcaReduce)
    return(clusters)
}
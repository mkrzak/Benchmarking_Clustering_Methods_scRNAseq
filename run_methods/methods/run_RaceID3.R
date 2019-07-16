run_RaceID3 <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){
   
    library(RaceID)
    
    if( (preproc != "method_specific") || dim_red!="PCA" || (use_dims_input!="TRUE" && use_dims_input!="internal") ||  (clust_tech!="k-medoids" && clust_tech!="k-means" && clust_tech!="hclust") || (clust != "set" && clust!="estimate")){
        stop("Provide correct parameters !")
    }
    
    
    if(clust_tech=="k-medoids"){
        clust_tech="kmedoids"
    }
    
    else if(clust_tech=="k-means"){
        clust_tech="kmeans"
    }
    

    scs <- SCseq(data.frame(data))
    if(preproc=="method_specific"){
    scs <- RaceID::filterdata(scs, minexpr = 5, minnumber = 1) #filters and normalizes data, doesnt work without it
    }

    #PCA
    if(dim_red=="PCA" &&  use_dims_input=="TRUE"){
        RaceID::CCcorrect(scs, nComp=dims_input, mode="pca")
    }

    else if(dim_red=="PCA" &&  use_dims_input=="internal"){
        RaceID::CCcorrect(scs, mode="pca", dimR=TRUE)
    }

    scs <- compdist(scs)

    if(clust=="set"){
        race3 = RaceID::clustexp(scs, sat=FALSE, cln=k_input, FUNcluster=clust_tech)
    }

    else if(clust=="estimate"){
        race3 = RaceID::clustexp(scs, sat=TRUE, FUNcluster=clust_tech)
    }

    clusters <- race3@cluster$kpart
    missing_ids=which( colnames(scs@expdata) %in% setdiff(colnames(scs@expdata), names(race3@cluster$kpart)))
   
    if(length(missing_ids)==0){
      return(clusters)
    } else{
      return(list(clusters=clusters, missing_ids=missing_ids))
    }

}




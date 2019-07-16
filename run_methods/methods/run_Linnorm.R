run_Linnorm <- function(data, preproc, dim_red, use_dims_input ,dims_input, clust_tech, clust, k_input){
   
    library(Linnorm)
    
    if( (preproc != "none" && preproc != "method_specific")|| (dim_red!="none" && dim_red!="PCA" && dim_red!="tSNE") || (use_dims_input!="FALSE" &&  use_dims_input!="TRUE" &&  use_dims_input!="internal") || (clust_tech!="fixed" && clust_tech!="hclust") || (clust != "set" && clust != "estimate")){
        stop("Provide correct parameters !")
    }
    
    if(preproc=="method_specific"){
        data=Linnorm::Linnorm.Norm(data, output="XPM") #normalize to CPM/TPM
    }
    
    #PCA + default clustering
    if(clust_tech=="fixed" && dim_red=="PCA" && clust=="set" && use_dims_input =="TRUE"){
     
    linn <- Linnorm::Linnorm.PCA(data,  num_center= k_input, num_PC=dims_input) 
    return(clusters <- linn$k_means$cluster)
    }
    
    else if(clust_tech=="fixed" && dim_red=="PCA" && clust=="set" && use_dims_input =="internal"){
       
        linn <- Linnorm::Linnorm.PCA(data,  num_center= k_input)
        return(clusters <- linn$k_means$cluster)
    }
    
    
    else if(clust_tech=="fixed" && dim_red=="PCA" && clust=="estimate" && use_dims_input =="TRUE" ){
       
        linn <- Linnorm::Linnorm.PCA(data, num_PC=dims_input)
        return(clusters <- linn$k_means$cluster)
    }
    
    else if(clust_tech=="fixed" && dim_red=="PCA" && clust=="estimate" && use_dims_input =="internal"){
       
        linn <- Linnorm::Linnorm.PCA(data)
        return(clusters <- linn$k_means$cluster)
    }
    
    #tSNE + default clustering
    
    else if(clust_tech=="fixed" && dim_red=="tSNE" && clust=="set" && use_dims_input =="TRUE"){
     
        linn <- Linnorm::Linnorm.tSNE(data,  num_center= k_input, num_PC=dims_input)
        return(clusters <- linn$k_means$cluster)
    }
    
    else if(clust_tech=="fixed" && dim_red=="tSNE" && clust=="set" && use_dims_input =="internal"){
        
        linn <- Linnorm::Linnorm.tSNE(data,  num_center= k_input)
        return(clusters <- linn$k_means$cluster)
    }
    
    else if(clust_tech=="fixed" && dim_red=="tSNE" && clust=="estimate" && use_dims_input =="TRUE"){
      
        linn <- Linnorm::Linnorm.tSNE(data, num_PC=dims_input)
        return(clusters <- linn$k_means$cluster)
    }
    
    else if(clust_tech=="fixed" && dim_red=="tSNE" && clust=="estimate" && use_dims_input =="internal"){
      
        linn <- Linnorm::Linnorm.tSNE(data)
        return(clusters <- linn$k_means$cluster)
    }
    
    #hclust
    else if(clust_tech=="hclust"  && dim_red=="none" && clust=="set" && use_dims_input =="FALSE"){
        
        linn <- Linnorm::Linnorm.HClust(data, num_Clust= k_input)
        return(clusters <- linn$Results)
    }

    detach(package:Linnorm)
    return(clusters)
}

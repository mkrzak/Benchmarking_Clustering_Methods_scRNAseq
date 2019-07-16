doTable <- function(files){
  library(mclust)
  data_type=list()
  run=list()
  name=list()
  dataset=list()
  ari=list()
  method=list()
  param_combination=list()
  run_time_min=list()
  nr_clusters=list()
  
  for(i in 1:length(files)){
    load(files[i])
    data_type[i]=unlist(strsplit(files[i], "/"))[1]
    temp=gsub("method_specific", "methodspecific", unlist(strsplit(files[i], "/"))[length(unlist(strsplit(files[i], "/")))])
    method[i]=strsplit(temp, "_")[[1]][1]
    param_combination[i]=gsub(".Rdata", "", paste(strsplit(temp, "_")[[1]][2:8], collapse = '_'))
  
    if(length(strsplit(files[1], "/")[[1]])==5){
      run[i]=unlist(strsplit(files[i], "/"))[2]
      name[i]=gsub("Res_", "", unlist(strsplit(files[i], "/"))[3])
      dataset[i]=gsub("Res_", "", unlist(strsplit(files[i], "/"))[4])
    }else if(length(strsplit(files[1], "/")[[1]])==4){
      run[i]="single run"
      name[i]=gsub("Res_", "", unlist(strsplit(files[i], "/"))[2])
      dataset[i]=gsub("Res_", "", unlist(strsplit(files[i], "/"))[3])

    }
    
  
    #ARI
    if(is.null(Result)==TRUE){
      ari[i]=NA
      run_time_min[i]=NA
      nr_clusters[i]=NA
    }
    else {
      ari[i]=adjustedRandIndex(true_labels, Result) 
      run_time_min[i]=Run_time/60
      nr_clusters[i]=length(unique(Result))
    }
    
  }
  df=data.frame("Data_type"= unlist(data_type), "Run"=unlist(run), "Name"=unlist(name), "Dataset"= unlist(dataset), "Method"=unlist(method), "Param_combination"= unlist(param_combination) , "ARI"=unlist(ari), "Run_time_min"=unlist(run_time_min), "Nr_clusters"= unlist(nr_clusters))
  
  return(df)
}
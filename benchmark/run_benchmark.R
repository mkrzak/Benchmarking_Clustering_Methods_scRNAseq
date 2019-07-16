#Comp_settings - data frame of parameter settings
#Comp_data - list that consist of $name_data_folder and inside paths to datasets
#out_folder - name of the output folder
#global_dims_input - list of values of dims_input
library(scater)

run_benchmark <- function(Comp_settings, Comp_data, out_folder, global_dims_input){
    
     system(paste0("mkdir ", out_folder))

    logfile <-file(paste0(out_folder, "/log.txt"))
    sink(logfile)

    for(h in 1:length(Comp_data)){
        system(paste0("mkdir ", out_folder, "/Res_", names(Comp_data)[h]))

    for(i in 1:length(Comp_data[[h]])){
        data=load(Comp_data[[h]][i])
        data.name=sub('\\.Rdata$', '', unlist(strsplit(Comp_data[[h]][i], "/"))[length(unlist(strsplit(Comp_data[[h]][i], "/")))])
        system(paste0("mkdir ", out_folder, "/Res_", names(Comp_data)[h], "/", data.name))

        for(g in 1:length(global_dims_input)){

            temp_dims_input=global_dims_input[[g]][1]

        for(j in 1:dim(Comp_settings)[1]){
            preproc=Comp_settings[j,]$preproc
            method=Comp_settings[j,]$method
            dim_red=Comp_settings[j,]$dim_red
            use_dims_input=Comp_settings[j,]$use_dims_input
            clust_tech=Comp_settings[j,]$clust_tech
            clust=Comp_settings[j,]$clust

            if(clust=="set"){
                k_input=length(unique(sce$Group))
            }
            else if(clust=="estimate"){
                k_input=NA
            }

            if(use_dims_input=="TRUE"){
                dims_input=temp_dims_input
            }

            else if(use_dims_input=="internal" | use_dims_input=="FALSE"){
                dims_input=NA
            }

            file_name=paste0(method,"_", preproc,"_",dim_red,"_",use_dims_input,"_", dims_input, "_", clust_tech,"_", clust, "_", k_input)
            print(paste0("Running..." , file_name))


            #Running benchmark
            start_time <- Sys.time()
            Result0=tryCatch(
                run_methods(data=counts(sce), method=method, preproc=preproc, dim_red=dim_red, use_dims_input=use_dims_input, dims_input=dims_input, clust_tech=clust_tech, clust=clust, k_input=k_input),
                error = function(e) { write(as.character(e), file=logfile, append = TRUE, sep = "\n") }
               )

            end_time <- Sys.time()
            Run_time=difftime(end_time, start_time, units = "secs")


            #Saving results
            if(is.integer(Result0)==TRUE | is.numeric(Result0)==TRUE){
              Result=Result0
              true_labels=sce$Group
            }else if(is.null(Result0)){
                Result=Result0
                true_labels=sce$Group
            }else{
              Result=Result0$clusters
              true_labels=sce$Group[-Result0$missing_ids]}


            file_path=paste0("Res_",names(Comp_data)[h], "/", data.name ,"/", file_name , ".Rdata")

            save(Result, Run_time, true_labels, file=paste0(out_folder,"/",file_path))
        }
    }

}
    }
sink()
}



######################## GENERAL INFORMATION ####################################
#batchCells - number of cells , provide vector or single number
#nGenes - number of genes , provide vector or single number
#group.prob - group probabilities, provide list of vector/vectors, numbers in each vector should sum up to 1, length of each vector is number of groups to simulate
#type - group balance , "balanced" if probabilities are the same in each group, "unbalanced" - if probabilities are unequal in some of the groups
#de.prob - probability of genes to be differentially expressed, provide vector or single number from range [0,1]
#dropout.type - has to be set to "experiment"
#dropout.mid - describe extension of dropouts in dataset, provide vector or single number
#set.seed - set seed for generation of random numbers
#setup - give a name of the simulation setup

library(splatter)

simulateSplatter <- function(batchCells, nGenes, group.prob, type, de.prob, dropout.type, dropout.mid, set.seed, run, setup){
    
    system(paste0("mkdir simulate_data/", run, "/Rdata_", setup))
    
    for(i in 1:length(batchCells)){
        
        for(j in 1:length(nGenes)){
            
            for(k in 1:length(group.prob)){
                
                for(l in 1:length(de.prob)){
                    
                    for(m in 1:length(dropout.mid)){
                        
                        set.seed(seed=set.seed) #for run1 here was 100 instead of 111
                        params <- newSplatParams()
                        params <- setParams(params, batchCells=batchCells[i], nGenes=nGenes[j], group.prob = unlist(group.prob[k]), de.prob=de.prob[l], dropout.type=dropout.type, seed=set.seed)
                        sce <- splatSimulate(params, method = "groups", verbose = FALSE)
                        rowData(sce)$feature_symbol <- rownames(sce)
                        logcounts(sce) <- log2(counts(sce)+1)
                        sce <- splatter:::splatSimDropout(sce, setParam(params, "dropout.mid", dropout.mid[m]))
                        
                        fname=paste0("sim_c", batchCells[i] , "_g", nGenes[j], "_gr", length(unlist(group.prob[k])), "_", type[k], "_de", de.prob[l], "_drop", dropout.mid[m])
                        save(sce, file=paste0("simulate_data/", run, "/Rdata_", setup, "/", fname, ".Rdata"))
                        
                        rmarkdown::render("simulate_data/Report.Rmd", output_dir= paste0("simulate_data/", run, "/Reports_", setup), output_file=paste0("/Reports_", setup, "/", fname, ".html"))
                        
                    }
                    
                }
            }
        }
    }
    
}

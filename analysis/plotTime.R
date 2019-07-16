plotTime_fun <- function(Tab){
  
  Tab$Param_combination <- paste0(Tab$Method, "_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1), "_",
                                  sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2), "_",
                                  sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3), "_",
                                  sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5), "_",
                                  sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6))
  
  Tab$Nr_cells <- sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 2)
  Tab$Nr_cells <- factor(Tab$Nr_cells, levels = c("c500", "c1000", "c5000"))
  
  
  Tab$Log_run_time <- log(Tab$Run_time_min+1)
  
  l1 <- log(1+1)
  l2 <- log(10+1)
  l3 <- log(60+1)
  l4 <- log(600+1)
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  palette0 <- palette0[names(palette0) %in% unique(Tab$Method)]
  
  # N <- paste0(unique(Tab_all$Dataset),", Nr cells: ", unique(Tab_all$Nr_cells))
  # names(N) <- as.character(unique(Tab_all$Dataset))
  # 
  # Tab$Run_time_hour <- Tab$Run_time_min/60 
  
  Nr_cells.labs <- c("Nr of cells = 500", "Nr of cells = 1000", "Nr of cells = 5000")
  names(Nr_cells.labs)  <- c("c500", "c1000", "c5000")
  
  
  p <- ggplot(data=Tab, aes(x=Param_combination, y=Log_run_time, fill=Method)) + geom_boxplot() + scale_fill_manual(values =palette0) + facet_wrap(~Nr_cells, nrow = 3, ncol = 1,  labeller = as_labeller(Nr_cells.labs)) #+ geom_point(aes(colour = Dataset), size=2)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2), strip.text = element_text(size=19), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18)) + labs(x= "Parameter combination", y = "Log (run time in minutes)")
  p <- p + geom_hline(yintercept=l1, linetype="dashed", color = "red") + geom_hline(yintercept=l2, linetype="dashed", color = "red") 
  p <- p + geom_hline(yintercept=l3, linetype="dashed", color = "red") + geom_hline(yintercept=l4, linetype="dashed", color = "red")  
  p <- p +  
    
    return(p)
  
}


plotTime1_fun <- function(Tab){
    
    Tab$Param_combination <- paste0(Tab$Method, "_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1), "_",
                                    sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2), "_",
                                    sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3), "_",
                                    sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5), "_",
                                    sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6))
    
    Tab$Log_run_time <- log(Tab$Run_time_min+1)
    
    l1 <- log(1+1)
    l2 <- log(10+1)
    l3 <- log(60+1)
    l4 <- log(600+1)
    
    palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
    palette0[11] <- "#D02090"
    palette0[12] <- "#008B8B"
    names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
    palette0 <- palette0[names(palette0) %in% unique(Tab$Method)]
    
    
    
    p <- ggplot(data=Tab, aes(x=Param_combination, y=Log_run_time, fill=Method)) + geom_boxplot() + scale_fill_manual(values =palette0) 
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=9), strip.text = element_text(size=19), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13)) + labs(x= "Parameter combination", y = "Log (run time in minutes)")
    p <- p + geom_hline(yintercept=l1, linetype="dashed", color = "red") + geom_hline(yintercept=l2, linetype="dashed", color = "red") 
    p <- p + geom_hline(yintercept=l3, linetype="dashed", color = "red") + geom_hline(yintercept=l4, linetype="dashed", color = "red")  
    p <- p +  
        
        return(p)
    
}


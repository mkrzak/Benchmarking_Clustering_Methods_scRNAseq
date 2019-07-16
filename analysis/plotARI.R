
plotARIheatmap_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a4 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a4, "_", a5, "_", a6 )
  
  Tab <- Tab[order(Tab$Name),]
  
  Tab$Dataset_new <- unlist(c(gsub("g1000_", "", gsub("_de0.5_drop0", "", gsub("sim_", "",as.character(Tab[which(Tab$Name=="setup1"),]$Dataset)))), sub("de", "de.prob=", lapply(strsplit(as.character(Tab[which(Tab$Name=="setup2"),]$Dataset), "_"),`[[`, 6)), sub("drop", "dropout.mid=", lapply(strsplit(as.character(Tab[which(Tab$Name=="setup3"),]$Dataset), "_"),`[[`, 7))))
  
  Tab_sum <- Tab %>% group_by(Combination, Dataset_new, Name) %>% summarize(Mean = mean(ARI, na.rm=TRUE))
  
  Tab_sum$Dataset_new=factor(Tab_sum$Dataset_new, levels = c("c500_gr4_balanced",
                                                             "c500_gr8_balanced",
                                                             "c500_gr16_balanced" ,
                                                             "c1000_gr4_balanced",
                                                             "c1000_gr8_balanced",
                                                             "c1000_gr16_balanced",
                                                             "c5000_gr4_balanced",
                                                             "c5000_gr8_balanced",
                                                             "c5000_gr16_balanced",
                                                             "c500_gr4_unbalanced",
                                                             "c500_gr8_unbalanced",
                                                             "c500_gr16_unbalanced",
                                                             "c1000_gr4_unbalanced",
                                                             "c1000_gr8_unbalanced",
                                                             "c1000_gr16_unbalanced",
                                                             "c5000_gr4_unbalanced",
                                                             "c5000_gr8_unbalanced",
                                                             "c5000_gr16_unbalanced",
                                                             "de.prob=0.1",
                                                             "de.prob=0.5",
                                                             "de.prob=0.9",               
                                                             "dropout.mid=0",
                                                             "dropout.mid=2",
                                                             "dropout.mid=4",
                                                             "dropout.mid=6") )
  
  p <- ggplot(Tab_sum, aes(Dataset_new, Combination)) + geom_tile(aes(fill = Mean)) + scale_fill_gradient("Mean ARI", low = "white", high = "steelblue", breaks=c(-0.5,0,0.5,1), limits=c(-0.5,1))  + facet_wrap(~Name, scales = "free_x") + theme(axis.text.y  = element_text(size=4), axis.text.x  = element_text(size=13, angle=90))
  p <- p + theme(strip.text = element_text(size=16)) + scale_y_discrete(name="", limits = rev(levels(factor(Tab$Combination))))
  p <- p + theme(axis.title.x = element_blank(), axis.text.x  = element_text(size=13, angle=90, hjust=0.95,vjust=0.2))
  p <- p + theme(axis.text.y = element_text(size = 5))
  return(p)
  
}

plotAccuracy_fun <- function(Tab, group_size){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$Nr_cells <- sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 2)
  Tab$Cell_groups <- sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 4)
  Tab$Group_size <- sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 5)
  
  Tab$Nr_cells <- factor(Tab$Nr_cells, levels = c("c500", "c1000", "c5000"))
  Tab$Cell_groups <- factor(Tab$Cell_groups, levels = c("gr4", "gr8", "gr16"))
  
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  palette0 <- palette0[names(palette0) %in% unique(Tab$Method)]
  
  Nr_cells.labs <- c("Nr of cells = 500", "Nr of cells = 1000", "Nr of cells = 5000")
  names(Nr_cells.labs)  <- c("c500", "c1000", "c5000")
  
  if(group_size=="balanced"){
    p <- ggplot(Tab[which(Tab$Group_size=="balanced"),], aes(x=Combination, y=ARI, fill=Method)) + geom_boxplot()  + facet_wrap(~Nr_cells, nrow = 3, ncol = 1, scales = "free_y", labeller = as_labeller(Nr_cells.labs)) + scale_fill_manual(values =palette0) 
    
  }else if(group_size=="unbalanced"){
    p <- ggplot(Tab[which(Tab$Group_size=="unbalanced"),], aes(x=Combination, y=ARI, fill=Method)) + geom_boxplot()  + facet_wrap(~Nr_cells, nrow = 3, ncol = 1, scales = "free_y", labeller = as_labeller(Nr_cells.labs)) + scale_fill_manual(values =palette0) 
    
  }
  
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2, size=8)) + labs(x = "Parameter combination")
  p <- p + geom_hline(yintercept=0.5, linetype="dashed", color = "red")
  p
  
  return(p)
  
}

plotAccuracy1_fun <- function(Tab, setup){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  
  Tab$de.prob <- sub("de", "de.prob = ", sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 6))
  Tab$dropout.mid <- sub("drop", "dropout.mid = ",sapply(strsplit(as.character(Tab$Dataset), "_"), "[[", 7))
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  palette0 <- palette0[names(palette0) %in% unique(Tab$Method)]
  
  #+ geom_boxplot()
  if(setup=="setup2"){
    p <- ggplot(Tab, aes(x=Combination, y=ARI, fill=Method)) + geom_boxplot() + facet_wrap(~de.prob, nrow = 3, ncol = 1, scales = "free_y") + scale_fill_manual(values =palette0) 
    
  }else if(setup=="setup3"){
    p <- ggplot(Tab, aes(x=Combination, y=ARI, fill=Method)) + geom_boxplot() + facet_wrap(~dropout.mid, nrow = 4, ncol = 1, scales = "free_y") + scale_fill_manual(values =palette0) 
    
  }
  
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2, size=8)) + labs(x = "Parameter combination")
  p <- p + geom_hline(yintercept=0.5, linetype="dashed", color = "red")
  p
  
  return(p)
  
}


plotARIComplexHeatmap_fun <- function(Tab, panel){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
  
  Tab_list=list()
  Tab_mat=list()
  for(i in 1:length(unique(Tab$Dataset))){
    Tab_list[[i]]=Tab[which(Tab$Dataset ==as.character(unique(Tab$Dataset))[i]),]
    Tab_mat[[i]]=reshape2::dcast(Tab_list[[i]], Combination~Name, value.var= "ARI")
    rownames(Tab_mat[[i]]) <- Tab_mat[[i]][,1]
    Tab_mat[[i]] <- Tab_mat[[i]][,-1]
    
  }
  
  anno=rownames(Tab_mat[[1]])
  Method <- sapply(strsplit(anno, "_"), "[[", 1)
  dim_red <- sapply(strsplit(anno, "_"), "[[", 3)
  clust_tech <- sapply(strsplit(anno, "_"), "[[", 5)
  clust <- sapply(strsplit(anno, "_"), "[[", 6)
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  
  
  palette1_fun = colorRampPalette(RColorBrewer::brewer.pal(9, "Purples"))
  palette1=rev(palette1_fun(length(unique(dim_red))))
  names(palette1)=unique(dim_red)
  
  palette2_fun = colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrBr"))
  palette2=palette2_fun(length(unique(clust_tech)))
  #palette2=colorspace::rainbow_hcl(length(unique(clust_tech)))
  names(palette2)=unique(clust_tech)
  
  ha_row = rowAnnotation(df = data.frame(Method, dim_red, clust_tech), simple_anno_size = unit(1, "cm"), col = list(Method = palette0[names(palette0) %in% unique(Method)], dim_red=palette1, clust_tech=palette2))
  
  if(panel=="A" | panel=="B"){
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid.rect(x, y, width = width * 0.7, height, gp = gpar(col = NA, fill = fill))}  
  } else {cell_fun=NULL}
  
  
  ht0=ComplexHeatmap::Heatmap(heatmap_legend_param = list(at = c(-0.5,0,0.5,1)), cell_fun = cell_fun, as.matrix(Tab_mat[[1]]), name = "ARI", col = c("white", "steelblue"), row_names_side = "left", row_names_gp = gpar(fontsize=6),left_annotation = ha_row,  column_title = as.character(unique(Tab_list[[1]]$Dataset)), cluster_rows = FALSE,  cluster_columns = FALSE)
  ht4=ComplexHeatmap::Heatmap(heatmap_legend_param = list(at = c(-0.5,0,0.5,1)), cell_fun = cell_fun, as.matrix(Tab_mat[[4]]), name = "ARI", col = c("white", "steelblue"), row_names_side = "left", row_names_gp = gpar(fontsize=6),left_annotation = ha_row,  column_title = as.character(unique(Tab_list[[4]]$Dataset)), cluster_rows = FALSE,  cluster_columns = FALSE)
  
  
  ht=list()
  for(j in 1:length(unique(Tab$Dataset))){
    ht[[j]]=ComplexHeatmap::Heatmap(heatmap_legend_param = list(at = c(-0.5,0,0.5,1)), as.matrix(Tab_mat[[j]]), cell_fun = cell_fun, name = "ARI", col = c("white", "steelblue"), column_title = as.character(unique(Tab_list[[j]]$Dataset)), show_row_names = FALSE,  cluster_rows = FALSE,  cluster_columns = FALSE)
  }
  
  hmap = ht0 + ht[[5]] + ht[[10]] + ht[[2]] + ht[[3]] 
  hmap1 = ht4 + ht[[6]] + ht[[7]] + ht[[8]] + ht[[9]]
  
  hmap_all = ht0 + ht[[5]] + ht[[10]] + ht[[2]] + ht[[3]] + ht[[4]]+ ht[[6]]+ ht[[7]]+ ht[[8]]+ ht[[9]]
  
  if (panel=="A") {
    myplot=draw(hmap, column_title="A", column_title_gp = gpar(fontsize=17, fontface = "bold"))
  } else if (panel=="B") {
    myplot=draw(hmap1, column_title="B", column_title_gp = gpar(fontsize=17, fontface = "bold"))
  } else if (panel=="all") {
    myplot=draw(hmap_all)
  }
  
  return(myplot)
  
}


plotAccuracy2_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  palette0 <- palette0[names(palette0) %in% unique(Tab$Method)]
  
  p <- ggplot(Tab, aes(x=Combination, y=ARI, fill=Method)) + geom_boxplot() + facet_wrap(~Name, nrow = 3, ncol = 1, scales = "free_y") + scale_fill_manual(values =palette0) 
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) + labs(x = "Parameter combination")
  p <- p + geom_hline(yintercept=0.5, linetype="dashed", color = "red")
  p
  
  return(p)
  
}


plotPval_fun <- function(Tab, test_type){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
  
  Combination=unique(Tab$Combination)
  p_val=c()
  for(i in 1:length(Combination)){
    T0=Tab[which(Tab$Combination==Combination[i]),]
    T0$Name <- factor(T0$Name)
    result=kruskal.test(T0$ARI ~ T0$Name, data = T0)
    p_val=c(p_val, result$p.val)
  }
  
  names(p_val) <- Combination
  p_val <- p_val[!is.na(p_val)]
  p_val = sort(p_val, decreasing = FALSE)
  p_adj_val=p.adjust(p_val, method = "hochberg", n = length(p_val))
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  methods_diff=sapply(strsplit(names(p_adj_val), "_"), "[[", 1)
  palette0 <- palette0[names(palette0) %in% methods_diff]
  
  
  df=data.frame("Method"=methods_diff, "Combination"=names(p_adj_val), p_val)
  
  p <- ggplot(df, aes(x=Combination, y=p_val, fill=methods_diff)) + geom_bar(stat="identity") + scale_fill_manual(values =palette0) 
  p <- p + theme(axis.text.x=element_blank(), axis.ticks = element_blank(), axis.title.x=element_blank()) + theme(legend.position="none")
  p <- p + geom_text(aes(label=round(p_val,3)), vjust=1.6, color="white", fontface = "bold", size=3)
  p    
  
  g <- ggplot(df, aes(x=Combination, y=p_adj_val, fill=methods_diff)) + geom_bar(stat="identity") + scale_fill_manual(values =palette0) 
  g <- g + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))
  g <- g + geom_text(aes(label=round(p_adj_val,3)), vjust=1.6, color="white", fontface = "bold", size=3)
  g  
  
  pg=list(p,g)
  plot=grid::grid.draw(egg::ggarrange(plots=pg, heights = c(1/2, 1/2), ncol=1)) 
  
  return(plot)
  
}

plotEstimation_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
  
  Nr_true_populations <- c("Baron2016_m"=13, "Darmanis2015"=9, "Deng2014_raw"=6, "Goolam2016"=4, 
                           "Klein2015"=4,"Kolodziejczyk2015"=3,"Li2017"=9, "Romanov2016"=7,"Tasic2016_raw"=17,"Zeisel2015"=9)
  
  Tab$clust_tech <- a5
  Tab$clust <- a6
  Tab=Tab[which(Tab$clust=="estimate"),]
  
  Tab_i=list()
  for(i in 1:length(unique(Tab$Dataset))){
    Tab_i[[i]]=Tab[which(Tab$Dataset==unique(Tab$Dataset)[i]),]
    Tab_i[[i]]$Nr_true_populations = Nr_true_populations[unique(Tab$Dataset)[i]]
  }
  
  Tab_all= do.call(rbind, Tab_i)
  Tab_all$diff <- Tab_all$Nr_clusters - Tab_all$Nr_true_populations
  Tab_all$diff1 = sign(Tab_all$diff) * log10(abs(Tab_all$diff) + 1) #log-modulus transformation
  
  palette3=colorspace::rainbow_hcl(13)
  names(palette3) <-  c("fixed","densityPeak","louvain","k-medoids","knn","max.distance","percent","ward.D","ADPclust","hclust","k-means","SNN")
  palette3 <- palette3[names(palette3) %in% unique(Tab_all$clust_tech)]
  
  Tab_all$clust_tech <- factor(Tab_all$clust_tech , levels = unique(Tab_all$clust_tech))
  
  p <- ggplot(Tab_all, aes(x=Combination, y=diff1, fill=clust_tech))  + geom_boxplot() + facet_wrap(~Name, nrow = 3, ncol = 1, scales = "free_y")
  p <- p + labs(x="Parameter combination", y="L(nr clusters - nr cell populations)") + theme(axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2))
  p <- p + geom_hline(yintercept=0, linetype="dashed", color = "red") 
  p <- p + theme(axis.title=element_text(size=9)) + scale_fill_manual(values = palette3)
  p
  
}

#################

plotPval1_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
  
  
  Combination=unique(Tab$Combination)
  p_val=c()
  for(i in 1:length(Combination)){
    # T0=Tab[which(Tab$Combination==Combination[i]),]
    # T0$Name <- factor(T0$Name)
    # result=pairwise.wilcox.test(T0$ARI, T0$Name, p.adj = "none", paired = TRUE)
    # p_val=c(p_val, result$p.value)
    
    T0=Tab[which(Tab$Combination==Combination[i]),]
    T0$Name <- factor(T0$Name)
    T0shape=reshape2::dcast(T0, Dataset ~ Name, value.var= "ARI")
    result=wilcox.test(T0shape$QC, T0shape$`QC & FILT`, alternative = c("two.sided"), paired=TRUE)
    p_val=c(p_val, result$p.value)
    
  }
  
  names(p_val) <- Combination
  p_val <- p_val[!is.na(p_val)]
  p_val = sort(p_val, decreasing = FALSE)
  p_adj_val=p.adjust(p_val, method = "hochberg", n = length(p_val))
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  methods_diff=sapply(strsplit(names(p_adj_val), "_"), "[[", 1)
  palette0 <- palette0[names(palette0) %in% methods_diff]
  
  df=data.frame("Method"=methods_diff, "Combination"=names(p_adj_val), p_val)
  
  p <- ggplot(df, aes(x=Combination, y=p_val, fill=methods_diff)) + geom_bar(stat="identity") + scale_fill_manual(values =palette0) 
  p <- p + theme(axis.text.x=element_blank(), axis.ticks = element_blank(), axis.title.x=element_blank()) + theme(legend.position="none")
  p <- p + geom_text(aes(label=round(p_val,3)), vjust=1.6, color="white", fontface = "bold", size=3)
  
  g <- ggplot(df, aes(x=Combination, y=p_adj_val, fill=methods_diff)) + geom_bar(stat="identity") + scale_fill_manual(values =palette0) 
  g <- g + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))
  g <- g + geom_text(aes(label=round(p_adj_val,3)), vjust=1.6, color="white", fontface = "bold", size=3)
  
  
  pg=list(p,g)
  plot=grid::grid.draw(egg::ggarrange(plots=pg, ncol=1)) 
  
  return(plot)
  
}

plotSummaryPCA_fun <- function(Tab, nr_param){

    a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
    a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
    a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
    a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
    a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
    Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
    
    Tab_all=reshape2::dcast(Tab, Combination~Dataset, value.var= "ARI")
    rownames(Tab_all) <- Tab_all[,1]
    Tab_all <- Tab_all[,-1] 
    
    Tab_all[is.na(Tab_all)] <- 0
    dat=prcomp(Tab_all)
    
    Tab_all$Method <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 1))
    Tab_all$preproc <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 2))
    Tab_all$dim_red <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 3))
    Tab_all$use_dims_input <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 4))
    Tab_all$clust_tech <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 5))
    Tab_all$clust <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 6))
    
    
    palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
    palette0[11] <- "#D02090"
    palette0[12] <- "#008B8B"
    names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
    palette0 <- palette0[names(palette0) %in% unique(Tab_all$Method)]
    
    p1 <- autoplot(dat, data= Tab_all, colour= "Method", shape="clust", size=3) + labs(title="A")  + scale_color_manual(values=palette0)  + theme(plot.title = element_text(size=16)) +theme(legend.key.size = unit(0.18, "cm")) + xlim(-0.14, 0.19) + ylim(-0.16, 0.25)#+ theme(legend.text=element_text(size=5))
    p2 <- autoplot(dat, data= Tab_all, colour= "Method", shape="preproc", size=3) + labs(title="B") + scale_color_manual(values=palette0)  + theme(plot.title = element_text(size=16)) +theme(legend.key.size = unit(0.18, "cm")) + xlim(-0.14, 0.19) + ylim(-0.16, 0.25)#+ theme(legend.text=element_text(size=5))
    
    p3 <- autoplot(dat, data= Tab_all, colour= "Method", shape="dim_red", size=4) + labs(title="C") + scale_color_manual(values =palette0) + scale_shape_manual(values=1:nlevels(Tab_all$dim_red)) + theme(plot.title = element_text(size=16))+theme(legend.key.size = unit(0.18, "cm")) + xlim(-0.14, 0.19) + ylim(-0.16, 0.25)#+ theme(legend.text=element_text(size=5))
    p4 <- autoplot(dat, data= Tab_all, colour= "Method", shape="clust_tech", size=4)+ labs(title="D")  + scale_color_manual(values=palette0) + scale_shape_manual(values=1:nlevels(Tab_all$clust_tech))  + theme(plot.title = element_text(size=16)) +theme(legend.key.size = unit(0.18, "cm"))+ xlim(-0.14, 0.19) + ylim(-0.16, 0.25) #+ theme(legend.text=element_text(size=5))
    p5 <- autoplot(dat, data= Tab_all, colour= "Method", shape="use_dims_input", size=3) + labs(title="E")  + scale_color_manual(values=palette0)  + theme(plot.title = element_text(size=16)) +theme(legend.key.size = unit(0.18, "cm"))+ xlim(-0.14, 0.19) + ylim(-0.16, 0.25) #+ theme(legend.text=element_text(size=5))

    if(nr_param==2){
        g <- gridExtra::grid.arrange(grobs= list(p1, p2) , ncol = 2, nrow = 1)
    } else if (nr_param==5){
        g <- gridExtra::grid.arrange(grobs= list(p1, p2, p3, p4, p5) , ncol = 3, nrow = 2)
    }
    
    return(g)
    
}

plotCorrelation_fun <- function(Tab){
    
    a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
    a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
    a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
    a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
    a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
    Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
    
    Tab_all=reshape2::dcast(Tab, Combination~Dataset, value.var= "ARI")
    rownames(Tab_all) <- Tab_all[,1]
    Tab_all <- Tab_all[,-1] 
    Tab_all[is.na(Tab_all)] <- 0
    dat=prcomp(Tab_all)
    
    A=cbind(dat$x[,1:2], "ARI_mean"=rowMeans(Tab_all[,1:dim(dat$x)[2]]))
    g <- ggpairs(A, upper = list(continuous = wrap("cor", size = 7)), diag=list(continuous = wrap("barDiag", bins = 20, fill="navy"))) 
    
    return(g)
    
}


plotARIComplexHeatmap1_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  Tab$dims <- paste0("dims_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4))
  Tab$dims <- factor(Tab$dims, levels = c("dims_3","dims_5","dims_10","dims_15"))
  
  Tab_list=list()
  Tab_mat=list()
  for(i in 1:length(unique(Tab$Dataset))){
    Tab_list[[i]]=Tab[which(Tab$Dataset ==as.character(unique(Tab$Dataset))[i]),]
    Tab_mat[[i]]=reshape2::dcast(Tab_list[[i]], Combination~dims, value.var= "ARI")
    rownames(Tab_mat[[i]]) <- Tab_mat[[i]][,1]
    Tab_mat[[i]] <- Tab_mat[[i]][,-1]
    
  }
  
  anno=rownames(Tab_mat[[1]])
  Method <- sapply(strsplit(anno, "_"), "[[", 1)
  dim_red <- sapply(strsplit(anno, "_"), "[[", 3)
  clust_tech <- sapply(strsplit(anno, "_"), "[[", 5)
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  
  palette1_fun = colorRampPalette(RColorBrewer::brewer.pal(9, "Purples"))
  palette1=rev(palette1_fun(length(unique(dim_red))))
  names(palette1)=unique(dim_red)
  
  palette2_fun = colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrBr"))
  palette2=rev(palette2_fun(length(unique(clust_tech))))
  # palette2=colorspace::rainbow_hcl(length(unique(clust_tech)))
  names(palette2)=unique(clust_tech)
  
  ha_row = rowAnnotation(df = data.frame(Method, dim_red, clust_tech), simple_anno_size = unit(1, "cm"), col = list(Method = palette0[names(palette0) %in% unique(Method)], dim_red=palette1, clust_tech=palette2))
  
  ht0=ComplexHeatmap::Heatmap(heatmap_legend_param = list(at = c(-0.5,0,0.5,1)), as.matrix(Tab_mat[[2]]), name = "ARI", col = c("white", "steelblue"), row_names_side = "left", row_names_gp = gpar(fontsize=6),left_annotation = ha_row,  column_title = as.character(unique(Tab_list[[2]]$Dataset)), cluster_rows = FALSE,  cluster_columns = FALSE)# + scale_color_brewer(palette="Paired")
  ht=list()
  for(j in 1:length(unique(Tab$Dataset))){
    ht[[j]]=ComplexHeatmap::Heatmap(heatmap_legend_param = list(at = c(-0.5,0,0.5,1)), as.matrix(Tab_mat[[j]]), name = "ARI", col = c("white", "steelblue"), column_title = as.character(unique(Tab_list[[j]]$Dataset)), show_row_names = FALSE,  cluster_rows = FALSE,  cluster_columns = FALSE)
  }
  
  
  hmap = ht0 + ht[[3]] + ht[[4]] + ht[[6]] + ht[[7]] + ht[[1]]+ ht[[5]]
  return(draw(hmap))
  
}

plotAccuracyPerMethod2_fun <- function(Tab){
    
    a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
    a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
    a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
    a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
    a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
    
    Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
    Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
    Tab$dims <- paste0("dims_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4))
    Tab$dims <- factor(Tab$dims, levels = c("dims_3","dims_5","dims_10","dims_15"))
    
    palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
    palette0[11] <- "#D02090"
    palette0[12] <- "#008B8B"
    names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
    palette0 <- palette0[names(palette0) %in% unique(Tab$Method)]
    
    p <- ggplot(Tab, aes(x=Combination, y=ARI, fill=Method)) + geom_boxplot() + facet_wrap(~dims, nrow = 4, ncol = 1, scales = "free_y") + scale_fill_manual(values =palette0) 
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) + labs(x = "Parameter combination")
    p <- p + geom_hline(yintercept=0.5, linetype="dashed", color = "red")
    p
    
    return(p)
    
}

plotPval2_fun <- function(Tab){
    
    a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
    a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
    a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
    a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
    a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
    
    Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
    Tab$Name <- toupper(gsub("_", " & ", Tab$Name))
    Tab$dims <- paste0("dims_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4))
    #Tab$dims <- factor(Tab$dims, levels = c("dims_3","dims_5","dims_10","dims_15"))
    
    Combination=unique(Tab$Combination)
    p_val=c()
    for(i in 1:length(Combination)){
        T0=Tab[which(Tab$Combination==Combination[i]),]
        T0$dims <- factor(T0$dims)
        result=kruskal.test(T0$ARI ~ T0$dims, data = T0)
        print(result$p.val)
        p_val=c(p_val, result$p.val)
    }
    
    names(p_val) <- Combination
    p_val <- p_val[!is.na(p_val)]
    p_val = sort(p_val, decreasing = FALSE)
    p_adj_val=p.adjust(p_val, method = "hochberg", n = length(p_val))
    
    palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
    palette0[11] <- "#D02090"
    palette0[12] <- "#008B8B"
    names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
    methods_diff=sapply(strsplit(names(p_adj_val), "_"), "[[", 1)
    palette0 <- palette0[names(palette0) %in% methods_diff]
    
    
    df=data.frame("Method"=methods_diff, "Combination"=names(p_adj_val), p_val)
    
    p <- ggplot(df, aes(x=Combination, y=p_val, fill=methods_diff)) + geom_bar(stat="identity") + scale_fill_manual(values =palette0) 
    p <- p + theme(axis.text.x=element_blank(), axis.ticks = element_blank(), axis.title.x=element_blank()) + theme(legend.position="none")
    p <- p + geom_text(aes(label=round(p_val,3)), vjust=1.6, color="white", fontface = "bold", size=3)
    p    
    
    g <- ggplot(df, aes(x=Combination, y=p_adj_val, fill=methods_diff)) + geom_bar(stat="identity") + scale_fill_manual(values =palette0) 
    g <- g + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))
    g <- g + geom_text(aes(label=round(p_adj_val,3)), vjust=1.6, color="white", fontface = "bold", size=3)
    g  
    
    pg=list(p,g)
    plot=grid::grid.draw(egg::ggarrange(plots=pg, heights = c(1/2, 1/2), ncol=1)) 
    
    return(plot)
    
}

plotEstimation1_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a4 <- paste0("dims_", sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 4))
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  
  
  Nr_true_populations <- c("Biase2014"=4, "Deng2014_rpkm"=5, "Segerstolpe2016"=15,"Tasic2016_rpkm"=17,"Treutlein2014"=5, "Xin2016"=8, "Yan2013"=6)
  
  Tab$dims <- factor(a4, levels = c("dims_3", "dims_5", "dims_10", "dims_15"))
  Tab$clust_tech <- a5
  Tab$clust <- a6
  Tab=Tab[which(Tab$clust=="estimate"),]
  
  Tab_i=list()
  for(i in 1:length(unique(Tab$Dataset))){
    Tab_i[[i]]=Tab[which(Tab$Dataset==unique(Tab$Dataset)[i]),]
    Tab_i[[i]]$Nr_true_populations = Nr_true_populations[unique(Tab$Dataset)[i]]
  }
  
  Tab_all= do.call(rbind, Tab_i)
  Tab_all$diff <- Tab_all$Nr_clusters - Tab_all$Nr_true_populations
  Tab_all$diff1 = sign(Tab_all$diff) * log10(abs(Tab_all$diff) + 1) #log-modulus transformation
  
  palette3=colorspace::rainbow_hcl(13)
  names(palette3) <-  c("fixed","densityPeak","louvain","k-medoids","knn","max.distance","percent","ward.D","ADPclust","hclust","k-means","SNN")
  palette3 <- palette3[names(palette3) %in% unique(Tab_all$clust_tech)]
  
  Tab_all$clust_tech <- factor(Tab_all$clust_tech , levels = unique(Tab_all$clust_tech))
  
  p <- ggplot(Tab_all, aes(x=Combination, y=diff1, fill=clust_tech))  + geom_boxplot() + facet_wrap(~dims, nrow = 4, ncol = 1, scales = "free_y")
  p <- p + labs(x="Parameter combination", y="L(nr clusters - nr cell populations)") + theme(axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2))
  p <- p + geom_hline(yintercept=0, linetype="dashed", color = "red") 
  p <- p + theme(axis.title=element_text(size=11)) + scale_fill_manual(values = palette3)
  p
  
}


plotSummaryPCA1_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  
  Tab_all=reshape2::dcast(Tab, Combination~Dataset, value.var= "ARI")
  rownames(Tab_all) <- Tab_all[,1]
  Tab_all <- Tab_all[,-1] 
  
  Tab_all[is.na(Tab_all)] <- 0
  dat=prcomp(Tab_all)
  
  Tab_all$Method <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 1))
  Tab_all$preproc <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 2))
  Tab_all$dim_red <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 3))
  Tab_all$use_dims_input <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 4))
  Tab_all$clust_tech <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 5))
  Tab_all$clust <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 6))
  
  A=cbind(dat$x[,1:2], "ARI_mean"=rowMeans(Tab_all[,1:dim(dat$x)[2]]))
  CorPC1= cor(A[,"PC1"], A[,"ARI_mean"], method = "pearson") 
  CorPC2= cor(A[,"PC2"], A[,"ARI_mean"], method = "pearson") 
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  palette0 <- palette0[names(palette0) %in% unique(Tab_all$Method)]

  
  if(CorPC1 < 0 ){
    dat$x[,1] <- -1*dat$x[,1] #make positively correlated with ARI
  }
  if(CorPC2 < 0){
    dat$x[,2] <- -1*dat$x[,2] #make positively correlated with ARI
  }
  
  p1 <- autoplot(dat, data= Tab_all, colour= "Method", shape="clust", size=3) + labs(title="A")  + scale_color_manual(values=palette0)  + theme(plot.title = element_text(size=16)) + theme(legend.key.size = unit(0.18, "cm")) + xlim(-0.31, 0.31) + ylim(-0.6, 0.5)#+ theme(legend.text=element_text(size=5))
  p3 <- autoplot(dat, data= Tab_all, colour= "Method", shape="dim_red", size=4) + labs(title="B") + scale_color_manual(values =palette0) + scale_shape_manual(values=1:nlevels(Tab_all$dim_red)) + theme(plot.title = element_text(size=16)) + theme(legend.key.size = unit(0.18, "cm"))+ xlim(-0.31, 0.31) + ylim(-0.6, 0.5)#+ theme(legend.text=element_text(size=5))
  p4 <- autoplot(dat, data= Tab_all, colour= "Method", shape="clust_tech", size=4)+ labs(title="C")  + scale_color_manual(values=palette0) + scale_shape_manual(values=1:nlevels(Tab_all$clust_tech))  + theme(plot.title = element_text(size=16)) + theme(legend.key.size = unit(0.18, "cm"))+ xlim(-0.31, 0.31) + ylim(-0.6, 0.5)#+ theme(legend.text=element_text(size=5))
  
  p3 <- p3 + guides(fill = guide_legend(order = 0), shape = guide_legend(order = 1))
  
  g <- gridExtra::grid.arrange(grobs= list(p1, p3, p4) , ncol = 3, nrow = 1)
  
  return(g)
  
}

plotCorrelation1_fun <- function(Tab){
  
  a1 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 1)
  a2 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 2)
  a3 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 3)
  a5 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 5)
  a6 <- sapply(strsplit(as.character(Tab$Param_combination), "_"), "[[", 6)
  Tab$Combination <- paste0(Tab$Method, "_", a1,"_", a2, "_", a3, "_", a5, "_", a6 )
  
  Tab_all=reshape2::dcast(Tab, Combination~Dataset, value.var= "ARI")
  rownames(Tab_all) <- Tab_all[,1]
  Tab_all <- Tab_all[,-1] 
  
  Tab_all[is.na(Tab_all)] <- 0
  dat=prcomp(Tab_all)
  
  Tab_all$Method <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 1))
  Tab_all$preproc <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 2))
  Tab_all$dim_red <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 3))
  Tab_all$use_dims_input <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 4))
  Tab_all$clust_tech <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 5))
  Tab_all$clust <- factor(sapply(strsplit(rownames(Tab_all), "_"), "[[", 6))
  
  A=cbind(dat$x[,1:2], "ARI_mean"=rowMeans(Tab_all[,1:dim(dat$x)[2]]))
  CorPC1= cor(A[,"PC1"], A[,"ARI_mean"], method = "pearson") 
  CorPC2= cor(A[,"PC2"], A[,"ARI_mean"], method = "pearson") 
  
  palette0=c(RColorBrewer::brewer.pal(n=12, name="Paired"),"#3307F4") 
  palette0[11] <- "#D02090"
  palette0[12] <- "#008B8B"
  names(palette0) <- c("ascend", "CIDR", "DIMMSC", "Linnorm", "monocle3", "pcaReduce", "RaceID3", "SC3", "Seurat", "SIMLR",  "sincell", "sscClust", "TSCAN")
  palette0 <- palette0[names(palette0) %in% unique(Tab_all$Method)]
  

  
  if(CorPC1 < 0 ){
    dat$x[,1] <- -1*dat$x[,1] #make positively correlated with ARI
  }
  if(CorPC2 < 0){
    dat$x[,2] <- -1*dat$x[,2] #make positively correlated with ARI
  }
  
  A=cbind(dat$x[,1:2], "ARI_mean"=rowMeans(Tab_all[,1:dim(dat$x)[2]]))

  g <- ggpairs(A, upper = list(continuous = wrap("cor", size = 7)), diag=list(continuous = wrap("barDiag", bins = 20, fill="navy"))) 
  
  return(g)
  
}













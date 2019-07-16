#ARI
ARI <- function(truth, clusters){
    library(mclust)
    ARI=adjustedRandIndex(truth, clusters)
    return(ARI) 
}

# #Jaccard
# Jaccard <- function(truth, clusters){
#     library(clusteval)
#     Jaccard=cluster_similarity(as.numeric(factor(truth)), as.numeric(factor(clusters)), similarity = "jaccard", method = "independence")
#     return(Jaccard) 
# }

#NMI
Jaccard <- function(truth, clusters){
    library(igraph)
    NMI=igraph::compare(as.numeric(factor(clusters)), as.numeric(factor(truth)), method = "nmi")
    return(NMI) 
}

compare_pca <- function(data, res){
    pca <- prcomp(log2(data + 1),scale=T) 
    # par(mfrow=c(3,5))
    for(i in 1:length(res)){
        plot(pca$rotation[,1],pca$rotation[,2], col=res[[i]], main=paste0("Method: ",gsub("^.*?_","",names(res[i]))))  
    }
    # dev.off()
}


plot_pca <- function(data, labels){
    pca <- prcomp(log2(data + 1),scale=T) 
    plot(pca$rotation[,1],pca$rotation[,2], col=as.numeric(factor(labels)))
}

barplot_comparison <- function(res, labels){
    library(mclust)
    nr_clusters = lapply(res,  function(x){length(unique(x))} )
    ARI = unlist(lapply(res, function(x){mclust::adjustedRandIndex(x, labels)}))
    names(ARI)=gsub("^.*?_","",names(res))
    my_bar=barplot(ARI, main="Adjusted Rand Index", cex.lab=0.9, cex.names=0.9, font = 2)
}

z.transf <- function(x) {
    x <- as.numeric(x)
    x.mu <- mean(x)
    x.sd <- sd(x)
    if (x.sd == 0) {
        x <- rep(0, length(x))
    } else {
        x <- (x-x.mu)/x.sd
    }
    return(x)
}
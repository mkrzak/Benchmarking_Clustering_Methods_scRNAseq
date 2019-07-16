library(SingleCellExperiment)
library(scater)

create_sce <- function(data, colData, rowData = NULL) {
    sceset <- SingleCellExperiment(assays = list(counts = as.matrix(data)), colData = colData)
    rowData(sceset)$feature_symbol <- rownames(sceset)
    return(sceset)
}





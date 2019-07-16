main.path="/media/monika/6F76-2B59/Benchmarking_Clustering_Methods_scRNAseq"
setwd(main.path)

#Import and preprocess Raw datasets
source("raw_data/import_raw_data.R")
source("raw_data/preprocess_data.R")

#Import and preprocess RPKM/FPKM datasets
source("norm_data/import_norm_data.R")
source("norm_data/preprocess_data.R")

#Simulate three setups
source("simulate_data/simulate_data.R")
 
#Benchmark scRNAseq clustering methods
source("benchmark/benchmark.R")

#Analyze results
library(evaluate)
rapply((evaluate(file("analysis/analysis.R"))), cat)





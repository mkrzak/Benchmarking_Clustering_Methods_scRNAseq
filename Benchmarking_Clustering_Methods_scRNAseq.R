main.path="/PathtoDownloadedRepository/Benchmarking_Clustering_Methods_scRNAseq-master" #change this directory
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





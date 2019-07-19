# Benchmarking Clustering Methods scRNAseq

This repository stores the code used in the study:

M. Krzak, Y. Raykov, A. Boukouvalas, L. Cutillo, C. Angelini, "Benchmark and parameter sensitivity analysis of scRNAseq clustering methods."

Codes include data import, basic preprocessing, benchmark of scRNAseq clustering methods and analysis of the results. Please note that in order to run the codes you need to install proper versions of the packages that are reported in the article and section below. We cannot guarantee that the results will be generated properly if any other package versions will be used.  

## Package installation
Prior to running the main code of our analysis install and test the required R packages by executing the scripts:

"install_packages.R" - installs clustering methods R packages and other package dependencies for data manipulation and plotting

"test_installation.R" - tests, if all packages are installed properly, if not warning message, will be returned with the name of the missing package

Note that installation code may call for the recent versions of packages. It is recommended to check if they match with the versions reported in the article. 

Some packages may require manual installation of package dependencies. Check install_packages.R script for more details. 

## Running the code
Below we report steps necessary to reproduce the results of our study. Note that running all the pipeline may take a considerable amount of time.  

- Download and unzip this repository
- Change main.path="/PathtoDownloadedRepository/Benchmarking_Clustering_Methods_scRNAseq-master" inside Benchmarking_Clustering_Methods_scRNAseq.R
- Run Benchmarking_Clustering_Methods_scRNAseq.R script 

<br/>

Benchmarking_Clustering_Methods_scRNAseq.R will execute the sequence of following scripts:

"import_raw_data.R" - imports 10 raw scRNAseq datasets and saves count matrix with cell population annotation in .Rdata format in the folder raw_data/Rdata

"preprocess_data.R" - performs 3 basic preprocessing types on raw data and saves each preprocessed count matrix with annotation in .Rdata format in the raw_data folder (raw_data/Rdata_qc, raw_data/Rdata_qc_filt, raw_data/Rdata_qc_filt_norm)  

"import_norm_data.R" - imports 7 RPKM/FPKM scRNAseq datasets and saves count matrix with cell population annotation in .Rdata format in the folder norm_data/Rdata

"preprocess_data.R" - performs fixed basic preprocessing of norm data and saves preprocessed count matrix with annotation in .Rdata format in the norm_data folder (norm_data/Rdata_qc_filt_hvg)

"simulate_data.R" - simulates datasets from three simulation setups (see Simulation_scheme.jpg) and repeats the simulation 5 times with different setting of the random seed. Simulated datasets will be stored in the simulate_data folder with a subfolder named by the run

"benchmark.R" - runs specific parameter combinations on each type of the dataset (see Clustering_scheme.jpg). Results, in terms of clustering output and additional information such as run time of the algorithm, will be stored in .Rdata format (with the name of the method followed by its parameter combination) in Results_raw_data, Results_norm_data, and Results_simulate_data folders 

"makeFigures.R" - produces figures and supplementary figures from the article

"makeTables.R" - produces supplementary tables from the article

Note that scripts for raw and RPKM/FPKM data preprocessing and data simulation will produce additional .html report files, each one per dataset, with a detailed description of data preprocessing steps.

## Minimum system requirements
The scripts have been tested on Ubuntu 16.04.5 LTS system with R version 3.5.1 and machine with specifications, Intel Core i7, 4.00 GHz x 8 and 24 GB RAM, which are the minimum system requirements for the analysis. 

## Additional R package versions

```
evaluate_0.13
ggplot2_3.1.1
ggfortify_0.4.6
GGally_1.4.0
dplyr_0.8.0.1
ComplexHeatmap_1.99.6
scater_1.10.1
mclust_5.4.3
SingleCellExperiment_1.4.1
splatter_1.6.1
tidyr_0.8.3
scran_1.10.2
```

## Contact

Please report any bugs or issues you will encounter when using this repository. Feel free to contact m.krzak@na.iac.cnr.it for any other queries. 


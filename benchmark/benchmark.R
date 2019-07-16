library(tidyr)

P=read.csv(file="benchmark/Valid_parameter_settings.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Sep_P=P
for(i in 1:dim(P)[2]){
    Sep_P=separate_rows(Sep_P, names(Sep_P)[i], sep = "/", convert = TRUE)
}

source("run_methods/run_methods.R")
source("benchmark/run_benchmark.R")

#Benchmark of RawUMIs/Raw read counts
################################################################################
Comp1=list("qc"= list.files("raw_data/Rdata_qc", full.names = TRUE), "qc_filt"= list.files("raw_data/Rdata_qc_filt", full.names = TRUE))
Sep_P1=Sep_P
run_benchmark(Comp_settings=Sep_P1, Comp_data=Comp1, out_folder= "Results_raw", list(3))

Sep_P1=Sep_P[which(Sep_P$preproc=="none"),]
Comp1=list("qc_filt_norm"= list.files("raw_data/Rdata_qc_filt_norm", full.names = TRUE))
run_benchmark(Comp_settings=Sep_P1, Comp_data=Comp1, out_folder= "Results_raw", list(3))


#Benchmark of FPKM/RPKM counts
################################################################################
Sep_P2=Sep_P[which(Sep_P$preproc=="none"),]
Sep_P2=Sep_P2[which(Sep_P2$use_dims_input=="TRUE"),]

Comp2=list("qc_filt_hvg"= list.files("norm_data/Rdata_qc_filt_hvg", full.names = TRUE))
run_benchmark(Comp_settings=Sep_P2, Comp_data=Comp2, out_folder= "Results_norm", list(3, 5, 10, 15))

#Benchmark of Simulated counts
################################################################################
setup1 = list.files("simulate_data/Rdata_setup1", full.names = TRUE)
df=data.frame(t(sapply(strsplit(setup1, "_"),c)))
df=df[order(as.numeric(sub("c", "", df$X4)), decreasing=FALSE),]
setup1 <- paste(df$X1, df$X2, df$X3, df$X4, df$X5, df$X6, df$X7, df$X8, df$X9, sep='_')
Comp3=list("setup2" = list.files("simulate_data/Rdata_setup2", full.names = TRUE), "setup3" = list.files("simulate_data/Rdata_setup3", full.names = TRUE), "setup1" = setup1)
 
Sep_P3=Sep_P
run_benchmark(Comp_settings=Sep_P3, Comp_data=Comp3, out_folder= "Results_simulate_data", list(3))








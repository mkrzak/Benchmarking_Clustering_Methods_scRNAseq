data=list.files("norm_data/Rdata", full.names = TRUE)

system("mkdir norm_data/Reports")
system("mkdir norm_data/Rdata_qc_filt_hvg")

for(i in 1:length(data)){
    print(data[i])
    load(data[i])
    rmarkdown::render("norm_data/Report.Rmd", output_file= paste0("Reports/", sub('\\.Rdata$', '',  unlist(strsplit(data[i], "/"))[length(unlist(strsplit(data[i], "/")))] ),  ".html") )
}




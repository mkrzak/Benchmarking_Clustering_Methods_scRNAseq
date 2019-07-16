data=list.files("raw_data/Rdata", full.names = TRUE)

system("mkdir raw_data/Reports")
system("mkdir raw_data/Rdata_qc")
system("mkdir raw_data/Rdata_qc_filt")
system("mkdir raw_data/Rdata_qc_filt_norm")

for(i in 1:length(data)){
    print(data[i])
    load(data[i])
    rmarkdown::render("raw_data/Report.Rmd", output_file= paste0("Reports/", sub('\\.Rdata$', '',  unlist(strsplit(data[i], "/"))[length(unlist(strsplit(data[i], "/")))] ),  ".html") )
}








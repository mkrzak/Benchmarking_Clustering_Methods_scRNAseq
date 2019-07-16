#Run scripts
system("mkdir norm_data/Rdata")
system("mkdir norm_data/temp")

bash_scripts=list.files("norm_data/bash")
R_scripts=list.files("norm_data/R")

df=data.frame(bash_scripts,R_scripts)

for(i in 1:dim(df)[1] ){
print(bash_scripts[i])
system(paste0("sh norm_data/bash/", bash_scripts[i]))
system(paste0("Rscript norm_data/R/", R_scripts[i]))
}


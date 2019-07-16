#Run scripts
system("mkdir raw_data/Rdata")
system("mkdir raw_data/temp")

bash_scripts=list.files("raw_data/bash")
R_scripts=list.files("raw_data/R")

df=data.frame(bash_scripts,R_scripts)
df

for(i in 1:dim(df)[1]){
print(bash_scripts[i])
system(paste0("sh raw_data/bash/", bash_scripts[i]))
system(paste0("Rscript raw_data/R/", R_scripts[i]))
}

system(paste0("sh raw_data/bash/", bash_scripts[10]))

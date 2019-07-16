
simulate_data <- function(set.seed, run){

source("simulate_data/simulateSplatter.R")    
system(paste0("mkdir simulate_data/", run))    
    
# SETUP 1
batchCells=c(500,1000,5000)
nGenes=1000
group.prob=list(rep(1/4,4), 
                rep(1/8,8), 
                rep(1/16,16),
                c(0.05, 0.05, 0.4, 0.5), 
                c(0.01, 0.01, 0.04 , 0.04, 0.2, 0.2, 0.2, 0.3),
                c(0.01, 0.01, 0.04 , 0.04, 0.1, 0.1, 0.1, 0.1, 0.01, 0.01, 0.04 , 0.04, 0.1, 0.1, 0.1, 0.1))

type=c("balanced", 
       "balanced", 
       "balanced",
       "unbalanced",
       "unbalanced",
       "unbalanced")
de.prob=0.5
dropout.type ="experiment"    
dropout.mid=0
setup = "setup1"
simulateSplatter(batchCells, nGenes, group.prob, type, de.prob, dropout.type, dropout.mid, set.seed,run, setup)

# SETUP 2
batchCells=1000
nGenes=1000
group.prob=list(c(0.25, 0.25, 0.25, 0.25))
type="balanced"
de.prob=c(0.1,0.5,0.9)
dropout.type ="experiment"
dropout.mid=0
setup = "setup2"
simulateSplatter(batchCells, nGenes, group.prob, type, de.prob, dropout.type, dropout.mid, set.seed,run, setup)

# SETUP 3
nGenes=1000
batchCells=1000
group.prob=list(c(0.25, 0.25, 0.25, 0.25))
type="balanced"
de.prob=0.5
dropout.type = "experiment"
dropout.mid=c(0,2,4,6)
setup = "setup3"
simulateSplatter(batchCells, nGenes, group.prob, type, de.prob, dropout.type, dropout.mid, set.seed,run, setup)

}

setSeeds = c(111,500,777,800,999)
for(i in 1:length(setSeeds)){
  simulate_data(setSeeds[i], paste0("run",i))
}








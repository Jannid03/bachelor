load("./optalpha.RData")
source("./single-cell-lib.R")

prior <- genprior.c(n=58, alpha=optalpha,
                    fdr=6.04e-5, ado=0.4309, Blin=1000, Bmut=10000)

save(prior, file="./prior.RData")

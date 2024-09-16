rm(list=ls())

args <- commandArgs(TRUE)
if (length(args) == 0) {
    stop("need > 0 arguments")
} else {
    for (i in 1:length(args))
        eval(parse(text=args[[i]]))
}

source("./single-cell-lib.R")

seed <- 20120528 + PARAM
set.seed(seed)

alpha <- PARAM/100
mprob <- genprior.c(n=58, alpha=alpha,
                    fdr=6.04e-5, ado=0.4309, Blin=1000, Bmut=10000)$mp

save(mprob, file=sprintf("./mprob-alpha-%02d.RData", PARAM))


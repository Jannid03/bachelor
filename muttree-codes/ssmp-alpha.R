rm(list=ls())

source("./single-cell-lib.R")

pij <- matrix(, 9, 100)
for (i in 1:100) {
    load(sprintf("./mprob-alpha-%02d.RData", i-1))
    pij[,i] <- mprob
}
load("./marginal-freq.RData")

ssmp <- ssmpdiff(fij, pij)
optalpha <- (which.min(ssmp) - 1)/100
save(ssmp=ssmp, optalpha=optalpha, file="./optalpha.RData")

pdf("./optalpha-nomain.pdf")
plot(ssmp, type="b",
     ylab="Distance",
     xlab="Time to MRCA, T1")
dev.off()





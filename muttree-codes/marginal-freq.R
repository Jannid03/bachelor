source("./single-cell-preprocess.R")
source("./single-cell-lib.R")

mc <- matrix(, p, n)
for (i in 1:n) mc[,i] <- mcount(mx[,i])

pairmat <- countpairs(mc)
csummat <- colSums(pairmat)
fij <- csummat/sum(csummat)

save(fij, file="marginal-freq.RData")


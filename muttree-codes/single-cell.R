rm(list=ls())
setwd("D:/Uni/Sommersemester_24/Bachelorarbeit/Material/Code/muttree-codes/muttree-codes")
## args <- commandArgs(TRUE)
## if (length(args) == 0) {
##     stop("need > 0 arguments")
## } else {
##     for (i in 1:length(args))
##         eval(parse(text=args[[i]]))
## }


PARAM <- 0

source("./single-cell-preprocess.R")
source("./single-cell-lib.R")
# source("./single-cell-validity.R")

mc <- matrix(, p, n)
rownames(mc) <- site
colnames(mc) <- sid
for (i in 1:n)
    mc[,i] <- mcount(mx[,i])

# Neues Prior Modell oder Laden davon
# prior <- genprior.c(10, alpha = 0.1, fdr = 1e-5)
# saveRDS(prior, file = "../../../../code/data/R_prior/alph_0.1_fdr_1e-5_10mut.RData")

prior <- readRDS("../../../../code/data/R_prior/alph_0.8_fdr_1e-7_10mut.RData")


#Einlesen von externer Datenmatrix
# mc <- read.table("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_norm/data4_0__n10_m50_fn0.800_fp1.000e-07_0doublets.data")
mc <- read.table("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_noise/data4_0__n10_m50_fn0.800_fp1.000e-07_0doublets.noisy")

mc <- as.matrix(mc)

p <- dim(mc)[1]
cells <- dim(mc)[2]

site <- c()
for (i in 1:p) {
  site <- c(site, paste("V",i,sep=""))
}


missense <- snptype == "missense"
nonsense <- snptype == "Nonsense"
synonymous <- snptype == "synonymous"
nonsynonymous <- missense | nonsense
tolerated <- prediction == "TOLERATED"
#stopifnot(sum(nonsynonymous) == 78)
# 
# sel <-  nonsynonymous & (
#         site == "ABCB5" |
#         site == "ANAPC1" |
#         site == "ARHGAP5" |
#         site == "ASNS" |
#         site == "DLEC1" |
#         site == "DMXL1" |
#         site == "DNAJC17" |
#         site == "FAM115C" |
#         site == "FRG1" |
#         ## (site == "MLL3" & !tolerated) |
#         (site == "MLL3" & tolerated) |
#         site == "NTRK1" |
#         (site == "PABPC1" & !tolerated) |
#         site == "PDE4DIP" |
#         site == "RETSAT" |
#         site == "SESN2" |
#         site == "ST13" |
#         site == "TOP1MT" |
#         site == "USP32")

sel <- nonsynonymous & (
  site == "M1" |
    site == "M2" |
    site == "M3" |
    site == "M4" |
    site == "M5"
)

#Neues sel? (Einfach alle)
sel <- rep(T, p)

nsel <- sum(sel)
npair <- nsel*(nsel - 1)/2
sel.gene <- site[sel]

sel.rate <- mrate[sel]
sel.freq <- mfreq[sel]
sel.pair <- matrix(, npair, 2)
k <- 1
for (i in 1:(nsel-1))
    for (j in (i+1):nsel) {
        sel.pair[k,1] <- sel.gene[i]
        sel.pair[k,2] <- sel.gene[j]
        k <- k + 1
    }

if (PARAM == 0) {
    sel.mat <- mc[sel,]
} else {
    sel.mat <- mc[sel, -as.integer(PARAM)]
}
res <- countpairs(mc)

posterior <- computeposterior(res, prior)
assoc <- sign(posterior - apply(posterior, 1, max))

library(RBGL)

# #### Für Root ####
# sel.gene <- c(sel.gene, "root")
# sel.rate <- c(sel.rate, 1)

G <- new("graphNEL", nodes=sel.gene, edgemode="directed")
for (k in 1:npair)
{
    v1 <- sel.pair[k,1]
    v2 <- sel.pair[k,2]
    w1 <- -log(posterior[k,1])
    w2 <- -log(posterior[k,2])
    if (assoc[k,1] == 0) {
        G <- addEdge(v1, v2, G, w1)
    }
    if (assoc[k,2] == 0) {
        G <- addEdge(v2, v1, G, w2)
    }
}

diff <- c()
gesamt <- c()

for (i in 1:nrow(posterior)) {
  ord <- order(posterior[i,], decreasing = T)
  
  gesamt <- c(gesamt, sum(posterior[i,]))
  diff <- c(diff, (posterior[i,ord[1]]-posterior[i,ord[2]]))
}
rank1 <- rank(gesamt, ties.method = "random")
rank2 <- rank(diff, ties.method = "random")

rank <- c(rank1+rank2)
test <- cbind(sel.pair, log(posterior),rank)

file.create("data.txt")
cat(test[1,], file = "data.txt", sep = " ", append = T)
for (i in 2:nrow(test)) {
  #writeLines(test[i,], fileConn, sep = " ")
  cat("\n", file = "data.txt", append=T)
  cat(test[i,], file = "data.txt", sep = " ", append = T)
}

file.create("data_matrix.txt")
cat(mc[1,], file = "data_matrix.txt", sep = " ", append = T)
for(i in 2:nrow(mc)) {
  cat("\n", file = "data_matrix.txt", append = T)
  cat(mc[i,], file = "data_matrix.txt", sep = " ", append = T)
}
###### NEU #######
#G <- addEdge("root", "M4", G, 10)

# for (i in sel.gene) {
#   if (i == "root") {
#     break
#   }
#   G <- addEdge("root", i, G, 10000)
# }

## Edmonds' algorithm for minimum total weights
Gob <- eobmin(G)
source("./single-cell-network.R")







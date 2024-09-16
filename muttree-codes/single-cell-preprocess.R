#DIR <- sprintf("/home/%s/Documents/MutationTree", Sys.info()["user"])
#DIR.Data <- sprintf("%s/Data", DIR)
#DIR.C <- sprintf("%s/C", DIR)
#DIR.R <- sprintf("%s/R", DIR)

# p <- 712
p <- 5

#mutationfile <- sprintf("%s/mutation.csv", DIR.Data)
#tmp <- as.matrix(read.csv(mutationfile, header=TRUE, skip=2))
# tmp <- as.matrix(read.csv("mutation.csv", header=TRUE, skip=2))
tmp <- as.matrix(read.csv("mutation_Versuch2.csv", header=TRUE, skip=2))
normal <- tmp[1:p, "LN.T1"]
cancer <- tmp[1:p, "LC.T1"]

excluded <- c("Coordinates", "LN.T1", "LC.T1")

n <- ncol(tmp) - length(excluded)
mx <- tmp[1:p, -which(colnames(tmp) %in% excluded)]
sid <- colnames(mx)

#genelistfile <- sprintf("%s/genelist.csv", DIR.Data)
# tmp <- as.matrix(read.csv("genelist.csv", header=TRUE, skip=1))
tmp <- as.matrix(read.csv("genlist_Versuch.csv", header=TRUE, skip=1))
snptype <- tmp[,4]
prediction <- tmp[,6]
site <- tmp[,7]

gid <- paste("g", 1:p, sep="")

tmp <- matrix(as.integer(unlist(strsplit(tmp[,5], split="/"))),
              p, 2, byrow=TRUE)
mfreq <- tmp[,1]
mtotl <- tmp[,2]
mrate <- mfreq/mtotl



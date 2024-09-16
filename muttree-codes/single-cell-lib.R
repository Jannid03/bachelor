libso <- "./single-cell-lib.so"
libedmonds <- "./eob.so"

ismis <- function(x) x == "-"
issgl <- function(x) x == "A"| x == "C"| x == "G"| x == "T"
isdbl <- function(x) !issgl(x) & !ismis(x)
isnml <- function(x) x == normal
ispur <- function(x) x == "R" | x == "A" | x == "G"
ispyr <- function(x) x == "Y" | x == "C" | x == "T"
isket <- function(x) x == "K" | x == "G" | x == "T"
isami <- function(x) x == "M" | x == "A" | x == "C"
isstr <- function(x) x == "S" | x == "G" | x == "C"
iswea <- function(x) x == "W" | x == "A" | x == "T"
hvnml <- function(x)
{
   stopifnot(is.vector(x) && length(x) == p)
   ifelse(normal == "A",
          x == "R" | x == "M" | x == "W",
          ifelse(normal == "C",
                 x == "Y" | x == "M" | x == "S",
                 ifelse(normal == "G",
                        x == "R" | x == "K" | x == "S",
                        x == "Y" | x == "K" | x == "W")))
}
mcount <- function(x)
{
   stopifnot(is.vector(x) && length(x) == p)
   mc <- rep(0, length(x))
   mc[ismis(x)] <- NA
   mc[issgl(x) & isnml(x)] <- 0
   mc[issgl(x) & !isnml(x)] <- 2
   mc[isdbl(x) & hvnml(x)] <- 1
   mc[isdbl(x) & !hvnml(x)] <- 3
   mc
}
countpairs <- function(x)
{
    nr <- nrow(x)
    nc <- ncol(x)

    storage.mode(x) <- "integer"
    storage.mode(nr) <- "integer"
    storage.mode(nc) <- "integer"

    dyn.load(libso)
    ans <- matrix(.Call("_countpairs", x, nr, nc),
                  ncol=9, byrow=TRUE)
    dyn.unload(libso)
    colnames(ans) <- c("00", "01", "02", "10", "11", "12",
                       "20", "21", "22")
    ans
}
genprior.c <- function(n, alpha=0.0,
                       fdr=6.04e-5, ado=0.4309, Blin=1000, Bmut=10000)
{
    storage.mode(n) <- "integer"
    storage.mode(alpha) <- "double"
    storage.mode(fdr) <- "double"
    storage.mode(ado) <- "double"
    storage.mode(Blin) <- "integer"
    storage.mode(Bmut) <- "integer"

    dyn.load(libso)
    ans <- .Call("_genprior_c",
                 n, alpha, fdr, ado, Blin, Bmut)
    dyn.unload(libso)
    ans[sapply(ans, is.null)] <- NULL
    ans
}
genprior.v <- function(n, alpha=0.0, rho=0.0, vpercent=0.0,
                       fdr=6.04e-5, ado=0.4309, Blin=1000, Bmut=10000)
{
    storage.mode(n) <- "integer"
    storage.mode(alpha) <- "double"
    storage.mode(rho) <- "double"
    storage.mode(vpercent) <- "double"
    storage.mode(fdr) <- "double"
    storage.mode(ado) <- "double"
    storage.mode(Blin) <- "integer"
    storage.mode(Bmut) <- "integer"

    dyn.load(libso)
    ans <- .Call("_genprior_v",
                 n, alpha, rho, vpercent, fdr, ado, Blin, Bmut)
    dyn.unload(libso)
    ans[sapply(ans, is.null)] <- NULL
    ans
}
ssmpdiff <- function(x, y)
{
    stopifnot(length(x) == 9)
    if (is.matrix(y)) {
        stopifnot(nrow(y) == 9)
        z <- double(ncol(y))
    } else {
        stopifnot(length(y) == 9)
        z <- double(1)
    }

    dyn.load(libso)
    .Call("_ssmpdiff", as.double(x), as.double(y), z)
    dyn.unload(libso)
    z
}
computeposterior <- function(x, prior)
{
    nr <- nrow(x)
    nc <- ncol(x)

    prob <- matrix(, nr, 3)
    colnames(prob) <- c("x->y", "x<-y", "x<+>y")
    rownames(prob) <- rownames(x)

    for (i in 1:nr) {
        if (sum(x[i,]) > 1) {
            prob[i,1] <- prod(prior$qr^x[i,])*prior$tp[1]
            prob[i,2] <- prod(prior$ql^x[i,])*prior$tp[2]
            prob[i,3] <- prod(prior$qu^x[i,])*prior$tp[3]
        } else {
            prob[i,1] <- prob[i,2] <- 0
            prob[i,3] <- 1
        }
    }
    # prob <- prob/rowSums(prob)
    return(prob)
}
eobmin <- function(Graph)
{
    # if (!isDirected(g))
    #     stop("only appropriate for directed graphs")
    # nv <- length(nodes(g))
    # em <- edgeMatrix(g)
    # ne <- ncol(em)
    # eW <- unlist(edgeWeights(g))
  
    nv <- length(Graph$nodes)
    em <- Graph$edgeL
    ne <- ncol(em)
    eW <- Graph$edgeData

    dyn.load(libedmonds)
    ans <- .Call("eobmin",
                 as.integer(nv),
                 as.integer(ne),
                 as.integer(em - 1),
                 as.double(eW))
    dyn.unload(libedmonds)

    ans[[1]] <- apply(ans[[1]], 2, function(x, y) y[x + 1], Graph$nodes)
    rownames(ans[[1]]) <- c("from", "to")
    rownames(ans[[2]]) <- c("weight")
    names(ans) <- c("edgeList", "weights")
    ans$nodes <- unique(as.vector(ans[[1]]))
    ans
}
treePath <- function(tree, root, leaf)
{
    stopifnot(length(root) == 1)
    path <- vector("list", length(leaf))
    names(path) <- leaf
    revtree <- reverseEdgeDirections(tree)
    for (k in leaf) {
        tmp <- c()
        nxtnode <- k
        while (nxtnode != root) {
            tmp <- c(tmp, nxtnode)
            nxtnode <- adj(revtree, nxtnode)[[1]]
        }
        tmp <- c(tmp, nxtnode)
        path[[k]] <- rev(tmp)
    }
    path
}
findPair <- function(pair, node1, node2)
{
    loc <- pair[,1] == node1 & pair[,2] == node2
    if (sum(loc) == 0) loc <- pair[,2] == node1 & pair[,1] == node2
    which(loc)
}

computeposterior_new <- function(x, prior) {
  nr <- nrow(x)
  nc <- ncol(x)
  
  prob <- matrix(, nr, 3)
  colnames(prob) <- c("x->y", "x<-y", "x<+>y")
  rownames(prob) <- rownames(x)
  
  for (i in 1:nr) {
    if (sum(x[i,]) > 1) {
      prob[i,1] <- sum(log(prior$qr)*x[i,])
      prob[i,2] <- sum(log(prior$ql)*x[i,])
      prob[i,3] <- sum(log(prior$qu)*x[i,])
    } else {
      prob[i,1] <- prob[i,2] <- 0
      prob[i,3] <- 1
    }
  }

  return(prob)
}


ausfuehren <- function(fn, fp, repeats, n, m, noise) {
  #Manuelles Ausführen:
  # fn <- 0.2
  # fp <- 1e-03
  # repeats <- 1
  # n <- 20
  # m <- 50
  # noise <- T
  # i <- 1
  
  #Processing into string
  st_fp <- sprintf("%.3e", fp)
  st_fn <- sprintf("%.3f", fn)
  
  for (run in 1:repeats) {
    #Generate data
    nullen <- TRUE
    
    #Ergebnisspeicherung
    results <- data.frame()
    results_vec <- data.frame()
    
    while (nullen) {
      setwd("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/")
      command <- sprintf("./generate_data data%s %s %s %s %s 0 0 1 0", run, n, m, fn, fp) 
      system(command)
      
      matrix_file_noise <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, "_0doublets.noisy", sep="")
      mc <- read.table(matrix_file_noise)
      mc <- as.matrix(mc)
      
      for (j in 1:n) {
        if(sum(mc[j,]) == 0) {
          nullen <- T
          break
        }
        else {
          nullen <- F
        }
      }
    }
    
    print("Matrix Found!")
    #Working directory
    setwd("D:/Uni/Sommersemester_24/Bachelorarbeit/code/muttree-codes/")
    
    #Daten ordnen
  
    graphviz_datei <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, ".gv", sep = "")
    graphviz_datei_neu <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/graphviz/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, ".gv", sep = "")
    
    if (file.exists(graphviz_datei)) {
      file.rename(graphviz_datei, graphviz_datei_neu)
    }
    else {
      graphviz_datei <- graphviz_datei_neu
    }
    svg_datei <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/graphviz/data", run, "_ori.svg", sep = "")
    
    dotfile <- sprintf(fmt = graphviz_datei_neu)
    svgfile <- sprintf(fmt = svg_datei)
    command <- sprintf("dot -Tsvg %s -o %s", dotfile, svgfile)
    system(command)
    
    newick_datei <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, ".newick", sep = "")
    newick_datei_neu <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/newick/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, ".newick", sep = "")
    
    if (file.exists(newick_datei)) {
      file.rename(newick_datei, newick_datei_neu)
    }
    
    #Matrix sortieren und richtige abrufen
    matrix_file_noise <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, "_0doublets.noisy", sep="")
    matrix_file_norm <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data", run, "_0__n", n, "_", "m", m, "_fn", st_fn, "_fp", st_fp, "_0doublets.data", sep="")
    
    matrix_name_noise_new <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_noise/data", run, "_fn", fn, "_fp_", fp, "_n_", n, "_m_", m, "_matrix_noise.noisy", sep = "")
    matrix_name_norm_new <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_norm/data", run, "_fn", fn, "_fp_", fp, "_n_", n, "_m_", m, "_matrix_norm.data", sep = "")
    
    if(file.exists(matrix_file_norm)) {
      file.rename(matrix_file_norm, matrix_name_norm_new)
      file.rename(matrix_file_noise, matrix_name_noise_new)
    }
    
    #Lädt R Funktionen
    # source("./single-cell-preprocess.R") # Falls aus Tabelle ausgelesen werden soll, erstmal unwichtig
    source("./single-cell-lib.R")
    
    #Laden der Prior-Daten, IF-Else einfügen
    prior_file <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/R_prior/", "fn", fn, "_fp_", fp, ".RData", sep="")
  
    if(file.exists(prior_file)) {
      prior <- readRDS(prior_file)
    } else {
      o_00_r_00 <- (1-fp)*(1-fp)
      o_01_r_00 <- (1-fp)*fp
      o_10_r_00 <- (1-fp)*fp
      o_11_r_00 <- fp*fp
      r_00 <- c(o_00_r_00, o_01_r_00, o_10_r_00, o_11_r_00)
      
      o_00_r_01 <- (1-fp)*fn
      o_01_r_01 <- (1-fp)*(1-fn)
      o_10_r_01 <- fp*fn
      o_11_r_01 <- (1-fn)*fp
      r_01 <- c(o_00_r_01, o_01_r_01, o_10_r_01, o_11_r_01)
      
      o_00_r_10 <- (1-fp)*fn
      o_01_r_10 <- fp*fn
      o_10_r_10 <- (1-fp)*(1-fn)
      o_11_r_10 <- (1-fn)*fp
      r_10 <- c(o_00_r_10,o_01_r_10,o_10_r_10,o_11_r_10)
      
      o_00_r_11 <- fn*fn
      o_01_r_11 <- fn*(1-fn)
      o_10_r_11 <- fn*(1-fn)
      o_11_r_11 <- (1-fn)*(1-fn)
      r_11 <- c(o_00_r_11, o_01_r_11, o_10_r_11, o_11_r_11)
      
      pr <- c(1/3,0,1/3,1/3)
      pl <- c(1/3, 1/3, 0, 1/3)
      pu <- c(1/3,1/3,1/3,0)
      
      pr_matrix <- matrix(c(pr[1]*r_00, pr[2]*r_01, pr[3]*r_10, pr[4]*r_11),
                          byrow = T, nrow = 4)
      pl_matrix <- matrix(c(pl[1]*r_00, pl[2]*r_01, pl[3]*r_10, pl[4]*r_11),
                          byrow = T, nrow = 4)
      pu_matrx <- matrix(c(pu[1]*r_00, pu[2]*r_01, pu[3]*r_10, pu[4]*r_11),
                         byrow = T, nrow = 4)
      
      qr <- colSums(pr_matrix)
      ql <- colSums(pl_matrix)
      qu <- colSums(pu_matrx)
      
      prior <- list(tp = c(1/3, 1/3, 1/3), pr = pr,
                    pl = pl, pu = pu, qr = qr, ql = ql, qu = qu)
      
      saveRDS(prior, file = prior_file)
    }
  
    #Einlesen der Datenmatrix, Name dynamisch
    matrix_file <- c()
    if (noise) {
      mc <- read.table(matrix_name_noise_new)
      matrix_file <- matrix_name_noise_new
    } else {
      mc <- read.table(matrix_name_norm_new)
      matrix_file <- matrix_name_norm_new
    }
    
    mc <- as.matrix(mc)
    
    #Dimensionen werden gespcihert, p = Mutationen
    p <- dim(mc)[1]
    cells <- dim(mc)[2]
    
    #Generische Namen V1, V2, etc.
    site <- c()
    for (j in 1:n) {
      site <- c(site, paste(j,sep=""))
    }

    #Paarerstellung
    npair <- p*(p - 1)/2
    
    pairs <- matrix(, npair, 2)
    
    k <- 1
    for (eins in 1:(p-1))
      for (zwei in (eins+1):p) {
        pairs[k,1] <- site[eins]
        pairs[k,2] <- site[zwei]
        k <- k + 1
      }

    #Übernommen aus Code, Berechnung posterior
    res <- countpairs(mc)
    
    #Anpassung der counted pairs, entfernen von 2
    res <- res[,c(1,2,4,5)]
    
    posterior <- computeposterior_new(res, prior)
    assoc <- sign(posterior - apply(posterior, 1, max))
    
    #Grapherstellung
    # library(RBGL)
    
    start1 <- Sys.time()
    Edgeliste <- c()
    Edgedata <- c()
    
    # G <- new("graphNEL", nodes=site, edgemode="directed")
    for (k in 1:npair)
    {
      v1 <- pairs[k,1]
      v2 <- pairs[k,2]
      w1 <- -(posterior[k,1])
      w2 <- -(posterior[k,2])
      if (assoc[k,1] == 0) {
        Edgeliste <- c(Edgeliste, which(site %in% v1), which(site %in% v2))
        Edgedata <- c(Edgedata, w1)
        # G <- addEdge(v1, v2, G, w1)
      }
      if (assoc[k,2] == 0) {
        # G <- addEdge(v2, v1, G, w2)
        Edgedata <- c(Edgedata, w2)
        Edgeliste <- c(Edgeliste, which(site %in% v2), which(site %in% v1))
      }
    }
    Edgeliste <- matrix(Edgeliste, nrow = 2)
    Graph <- list(nodes = site, edgeL = Edgeliste, 
                  edgeData = Edgedata, nodeData = c(),
                  graphdata = list(edgemode = "directed"))
    # Gob <- eobmin(G)
    Gob <- eobmin(Graph)
    end1 <- Sys.time()
    
    part1 <- end1-start1
    part1 <- as.numeric(part1)*1000
    print(part1)
    #Prioberechnung und Export
    diff <- c()
    gesamt <- c()
    
    for (curr_row in 1:nrow(posterior)) {
      ord <- order(posterior[curr_row,], decreasing = T)
      
      gesamt <- c(gesamt, sum(posterior[curr_row,]))
      diff <- c(diff, (posterior[curr_row,ord[1]]-posterior[curr_row,ord[2]]))
    }
    
    
    rank1 <- rank(gesamt, ties.method = "random")
    rank2 <- rank(diff, ties.method = "random")
    
    rank <- c(rank1+rank2)
    test <- cbind(pairs, posterior,rank)
    
    data_name <- c()
    
    if (noise) {
      data_name <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_noise/data", run, "_noise.txt", sep = "")  
    } else {
      data_name <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_norm/data", run, "_norm.txt", sep = "")  
    }
    
    file.create(data_name)
    cat(test[1,], file = data_name, sep = " ", append = T)
    for (i in 2:nrow(test)) {
      #writeLines(test[i,], fileConn, sep = " ")
      cat("\n", file = data_name, append=T)
      cat(test[i,], file = data_name, sep = " ", append = T)
    }
    
    ori <- graphviz_datei_neu
    kim_simon <- "D:/Uni/Sommersemester_24/Bachelorarbeit/code/muttree-codes/scnob-norate00.dot"
    scite <- paste("D:/Uni/Sommersemester_24/Bachelorarbeit/code/data/data_noise/data", run, "_fn", fn, "_fp_", fp, "_n_", n, "_m_", m, "_matrix_noise_ml0.gv", sep = "")
      
    file.create("dateinamen.txt")
    cat(data_name, file = "dateinamen.txt", sep = "\n", append = T)
    cat(matrix_file, file = "dateinamen.txt", sep = "\n", append = T)
    cat(fn, file = "dateinamen.txt", sep = "\n", append = T)
    cat(fp, file = "dateinamen.txt", sep = "\n", append = T)
    cat(p, file = "dateinamen.txt", sep = "\n", append = T)
    cat(cells, file = "dateinamen.txt", sep = "\n", append = T)
    cat(ori, file = "dateinamen.txt", sep = "\n", append = T)
    cat(kim_simon, file = "dateinamen.txt", sep = "\n", append = T)
    cat(scite, file = "dateinamen.txt", sep = "\n", append = T)
    
    ## Edmonds' algorithm for minimum total weights
    #FÜr andere Programme wichtig
    nsel <- p
    sel.rate <- 1
    sel.gene <- site
  
    PARAM <- 0
    # source("./single-cell-network.R")
    
    #####Kopiert weil Parameter nicht funktionieren
    
    dotname <- c("scn", "scnob", "scnob-norate")
    for (d in dotname)
    {
      fname <- sprintf("./%s%02d.dot", d, PARAM)
      cat(file=fname)
      dcat <- function(...)
        cat(..., sep="", file=fname, append=TRUE)
      
      dcat("digraph G {\n")
      dcat("rankdir=TB\n")
      if (d == "scn" || d == "scnob") {
        ord <- order(sel.rate, decreasing=TRUE)
        for (i in 1:nsel)
          dcat("t", i, "[shape=plaintext, label=",
               round(sel.rate[ord][i], digits=3), "]\n")
        for (i in 2:nsel)
          dcat("t", i-1, "->t", i, "[arrowhead=none]\n")
        for (i in 1:nsel)
          dcat("{rank=same;t", i, ";", sel.gene[ord][i], ";}\n")
      }
      if (d == "scn") {
        k <- 1
        for (i in 1:(nsel-1)) {
          for (j in (i+1):nsel) {
            if (assoc[k,1] == 0) {
              dcat(sel.gene[i], "->", sel.gene[j], "\n")
            }
            if (assoc[k,2] == 0) {
              dcat(sel.gene[j], "->", sel.gene[i], "\n")
            }
            k <- k + 1
          }
        }
      } else {
        for (i in 1:ncol(Gob$edgeList)) {
          w <- exp(-Gob$weights[i])
          dcat(Gob$edgeList[1,i], " -> ", Gob$edgeList[2,i], "\n")
        }
      }
      dcat("}\n")
    }
    
    dotfile <- sprintf("./scnob-norate%02d.dot", PARAM)
    svgfile <- sprintf("./scnob-norate%02d.svg", PARAM)
    command <- sprintf("dot -Tsvg %s -o %s", dotfile, svgfile)
    system(command)
    
    setwd("D:/Uni/Sommersemester_24/Bachelorarbeit/code/SCITE_normal/")
    command <- c()
    command <- sprintf("./scite -i %s -n %s -m %s -r 1 -l 10000 -fd %s -ad %s 0 -cc 0", matrix_file, n, m, fp, fn)
    start3 <- Sys.time()
    system(command)
    ende3 <- Sys.time()
    
    part3 <- (ende3 - start3)
    part3 <- as.numeric(part3)* 1000
    print(part3)
    
    
    setwd("D:/Uni/Sommersemester_24/Bachelorarbeit/code/")
    command <- c()
    command <- sprintf("./bachelor_main D://Uni//Sommersemester_24//Bachelorarbeit//code//muttree-codes//dateinamen.txt T")
    start2 <- Sys.time()
    system(command)
    ende2 <- Sys.time()
    
    part2 <- (ende2 - start2)
    part2 <- as.numeric(part2)
    print(part2* 1000)
    
    
    ###Ergebnisse
    
    output_file <- "D:/Uni/Sommersemester_24/Bachelorarbeit/code/output.txt"
    output <- read.table(output_file, sep = ",", colClasses = "numeric")
    
    output <- c(output, part1, part3)
    
    output_vec_file <- "D:/Uni/Sommersemester_24/Bachelorarbeit/code/output_vec.txt"
    output_vec <- read.table(output_vec_file, sep = ";", colClasses = "character")
    
    results_vec <- rbind.data.frame(results_vec, output_vec)
    results <- rbind.data.frame(results, output)
    
    result_file <- "D:/Uni/Sommersemester_24/Bachelorarbeit/code/results.txt"
    # all_results <- c()
    parent_vecs <- "D:/Uni/Sommersemester_24/Bachelorarbeit/code/parent_vecs.txt"
    # all_vecs <- c()
    
    if (!file.exists(result_file)) {
      file.create(result_file)
    }
    
    if (!file.exists(parent_vecs)) {
      file.create(parent_vecs)
    }
    
    cat(as.character(results),file = result_file, sep = " ", append = T)
    cat("\n",file = result_file, sep = " ", append = T)
    
    cat(as.character(results_vec),file = parent_vecs, sep = ";", append = T)
    cat("\n",file = parent_vecs, sep = " ", append = T)
    
    
  } #End of for Schleife
  
}

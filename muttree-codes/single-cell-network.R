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
            dcat(Gob$edgeList[1,i], "->", Gob$edgeList[2,i], "\n")
        }
    }
    dcat("}\n")
}

dotfile <- sprintf("./scnob-norate%02d.dot", PARAM)
svgfile <- sprintf("./scnob-norate%02d.svg", PARAM)
command <- sprintf("dot -Tsvg %s -o %s", dotfile, svgfile)
system(command)


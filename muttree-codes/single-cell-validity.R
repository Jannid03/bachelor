ch <- apply(mx, 1, unique)
pur <- unlist(lapply(ch, function(x) all(ispur(x) | ismis(x))))
pyr <- unlist(lapply(ch, function(x) all(ispyr(x) | ismis(x))))
ket <- unlist(lapply(ch, function(x) all(isket(x) | ismis(x))))
ami <- unlist(lapply(ch, function(x) all(isami(x) | ismis(x))))
str <- unlist(lapply(ch, function(x) all(isstr(x) | ismis(x))))
wea <- unlist(lapply(ch, function(x) all(iswea(x) | ismis(x))))
stopifnot(all(pur + pyr + ket + ami + str + wea == 1))


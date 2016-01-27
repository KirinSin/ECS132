getnewpass <- function(len) return(sample(0:2,len,replace=TRUE,prob=c(0.5,0.4,0.1)))
passleft <- function(len, vec) return(rbinom(len, vec, 0.2))

parta <- function(nreps) {
    l1 <- getnewpass(nreps)
    l2 <- l1 - passleft(length(l1), l1)
    l2 <- l2 + getnewpass(nreps)
    return(mean(l1*l2)-mean(l1)*mean(l2))
}

getnewpass <- function(len) return(sample(0:2,len,replace=TRUE,prob=c(0.5,0.4,0.1)))

im <- function(nreps) {
    b <- getnewpass(nreps)
    b <- b + getnewpass(nreps)
    b <- b + getnewpass(nreps)
    sapply(c(0:6), function(x) sum(b==x)/nreps)
}

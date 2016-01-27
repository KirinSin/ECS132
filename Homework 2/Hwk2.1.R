
getallweights <- function(len) return(sample(1:3,len,replace=TRUE))

parta <- function(nreps) {
    firstweights <- getallweights(nreps)
    currentweights <- vector(length=nreps)
    sumweights <- firstweights
    expected <- 0
    while (sum(sumweights<=4)>0) {
        idx1 <- which(sumweights<=4)
        currentweights <- getallweights(nreps)
        sumweights <- sumweights + currentweights
        idx2 <- which(sumweights>4)
        idx <- intersect(idx1, idx2)
        expected <- expected + sum(currentweights[idx])

    }
    return(expected/nreps)
}

partb <- function(nreps) {
    firstweights <- getallweights(nreps)
    currentweights <- vector(length=nreps)
    sumweights <- firstweights
    expected <- 0
    expectedsqr <- 0
    while (sum(sumweights<=4)>0) {
        idx1 <- which(sumweights<=4)
        currentweights <- getallweights(nreps)
        sumweights <- sumweights + currentweights
        idx2 <- which(sumweights>4)
        idx <- intersect(idx1, idx2)
        expected <- expected + sum(currentweights[idx])
        expectedsqr <- expectedsqr + sum(currentweights[idx]^2)

    }
    return(expectedsqr/nreps - (expected/nreps)^2)
}

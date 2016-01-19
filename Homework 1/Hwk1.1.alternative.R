getallweights <- function(len) return(sample(1:3,len,replace=TRUE))

parta <- function(nreps) {
    sumweight <- vector(length=nreps)
    for (i in 1:3) sumweight <- sumweight + getallweights(nreps)
    finalweight <- sumweight + getallweights(nreps)
    return(sum(finalweight[which(sumweight<=4)]>4)/nreps)
}

partb <- function(nreps) {
    sumweight <- getallweights(nreps)
    countequal4 <- 0
    for (i in 2:4) {
        sumweight <- sumweight + getallweights(nreps)
        countequal4 <- countequal4 + sum(sumweight==4)
    }
    return(1-countequal4/nreps)
}

partc <- function(nreps) {
    firstweights <- getallweights(nreps)
    currentweights <- vector(length=nreps)
    sumweights <- firstweights + getallweights(nreps)
    count21 <- 0
    while (sum(sumweights<=4)>0) {
        idx1 <- which(sumweights<=4)
        currentweights <- getallweights(nreps)
        sumweights <- sumweights + currentweights
        idx2 <- which(sumweights>4)
        idx <- intersect(idx1, idx2)
        count21 <- count21 + sum(currentweights[idx]==1)
    }
    return(count21/nreps)
}

partd <- function(nreps) {
    firstweights <- getallweights(nreps)
    currentweights <- vector(length=nreps)
    sumweights <- firstweights + getallweights(nreps)
    count11 <- 0
    count21 <- 0
    while (sum(sumweights<=4)>0) {
        idx1 <- which(sumweights<=4)
        currentweights <- getallweights(nreps)
        sumweights <- sumweights + currentweights
        idx2 <- which(sumweights>4)
        idx <- intersect(idx1, idx2)
        count21 <- count21 + sum(currentweights[idx]==1)
        idxx <- intersect(which(currentweights==1), idx)
        count11 <- count11 + sum(firstweights[idxx]==1)
    }
    return(count11/count21)
}

getallrolls <- function(len) return(sample(1:6,len,replace=TRUE))

parta <- function(nreps) {
    posjack <- rep(2,nreps)
    steps <- getallrolls(nreps)
    posjack <- (posjack + steps) %% 8
    idx3 <- which(posjack==3)
    steps <- getallrolls(length(idx3))
    posjack[idx3] <- (posjack[idx3] + steps) %% 8
    return(sum(posjack==0)/nreps)
}

partb <- function(nreps) {
    posjack <- rep(2,nreps)
    steps <- getallrolls(nreps)
    posjack <- (posjack + steps)
    idx3 <- which(posjack==3)
    steps <- getallrolls(length(idx3))
    posjack[idx3] <- (posjack[idx3] + steps)
    
    posjill <- rep(0,nreps)
    steps <- getallrolls(nreps)
    posjill <- (posjill + steps)
    idx3 <- which(posjill==3)
    steps <- getallrolls(length(idx3))
    posjill[idx3] <- (posjill[idx3] + steps)
    return(sum(posjill>posjack)/nreps)
}

partc <- function(nreps) {
    posjack <- rep(2,nreps)
    steps <- getallrolls(nreps)
    posjack <- (posjack + steps) %% 8
    idxjack <- which(posjack!=3)
    idx3jack <- which(posjack==3)
    steps <- getallrolls(length(idx3jack))
    posjack[idx3jack] <- (posjack[idx3jack] + steps) %% 8
    
    posjill <- rep(0,nreps)
    steps <- getallrolls(nreps)
    posjill <- (posjill + steps) %% 8
    idxjill <- which(posjill!=3)
    idx3jill <- which(posjill==3)
    steps <- getallrolls(length(idx3jill))
    posjill[idx3jill] <- (posjill[idx3jill] + steps) %% 8

    idx <- intersect(idxjack, idxjill)
    return(sum(posjill[idx]==posjack[idx])/sum(posjill==posjack))
}

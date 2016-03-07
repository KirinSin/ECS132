altfindpi <- function(p,k){
    nilters <- ceiling(log2(k))
    prd <- p
    for(i in 1:nilters){
        prd <- prd %*% prd}
    colMeans(prd)}

findp1<-function(p){
    n<- nrow(p)
    imp <- diag(n) - t(p)
    imp[n,] <- rep(1,n)
    rhs <- c(rep(0,n-1),1)
    solve(imp,rhs)}

p <- matrix(rep(0,16),nrow=4)

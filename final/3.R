pval <- function(x, p0) {
    phat <- mean(x)
    Z <- (phat - p0)/sqrt(p0*(1-p0)/length(x))
    2*(1-pnorm(Z,0,1))
}

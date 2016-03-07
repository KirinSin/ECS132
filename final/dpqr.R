exact3failures <- function(k) {
    defective_flag <- rbinom(k, 1, 0.05)
    if (k < 3) return(0)
    sum(defective_flag)==3
}

binomial_poisson <- function(k) {
    dpois(k,50)*dbinom(3,k,0.05)
}

verify <- function() {
    sum(sapply(3:42, binomial_poisson))/ppois(42,50)
}

sim <- function(nreps) {
    component_num <- rpois(nreps, 50)
    probless42 <- sum(component_num <= 42)/nreps
    idx <- which(component_num <= 42)
    res <- sapply(component_num[idx], exact3failures)
    d3andprobless42 <- sum(res==T)/nreps
    return(d3andprobless42/probless42)
}



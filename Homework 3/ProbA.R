# guys, I think we need to switch to not using loop
# but to generate samples in parallel and also use
# sapply. Or I'm worried that we will not be able
# to meet the runtime limitation (if there is one).
exact3failures <- function(k, nreps) {
    defective_flag <- rbinom(nreps, 1, 0.05)
    for (i in 2:k) {
        defective_flag <- cbind(defective_flag, rbinom(nreps, 1, 0.05))
    }
    res <- rowSums(defective_flag)
    return(sum(res==3)/nreps)
}

verify3failures <- function(k) {
    return(choose(k,3)*(0.05^3)*(0.95^(k-3)))
}

sim <- function(nreps) {
    component_num <- rpois(nreps, 50)
    idx <- which(component_num <= 42 & component_num > 2)
    # Even parallelizing stuff like this, using the following code will still
    # cause the program to hang:
    # res <- sapply(component_num[idx], exact3failures, nreps=nreps)
    # not sure if I made a mistake or we need to improve the perf.  but using
    # equation to compute the probability of
    # 3 defective runs very fast. However, it feels like cheating, because it
    # is basically calling dbinom(3,k,0.05)
    res <- sapply(component_num[idx], verify3failures)
    return(mean(res))
}



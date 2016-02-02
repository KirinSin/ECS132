# authors: Yangzihao Wang

# arguments:
#
# x: data, a vector of positive integers
# 
# p: the "success" probability
#
# k: the right end of the support of the distribution
#
# return value:
# the vector that stores P(X=x)
#

dgeomk <- function(x, p, k) {
    res <- x
    res[x==0] <- p
    res[x<0] <- 0
    res[x>k] <- 0
    #compute c:
    c <- 1/sum(p*(1-p)^(0:(k-1)))
    idx <- which(x>0 & x<=k)
    res[idx] <- c*p*(1-p)^(res[idx]-1)
    return(res)
}

# arguments:
#
# x: data, a vector of positive integers
# 
# p: the "success" probability
#
# k: the right end of the support of the distribution
#
# return value:
# the vector that stores P(X<=x)
#
pgeomk <- function(x, p, k) {
    res <- x
    res[x==0] <- p
    res[x<0] <- 0
    res[x>k] <- 1
    #compute c:
    c <- 1/sum(p*(1-p)^(0:(k-1)))
    idx <- which(x>0 & x<=k)
    res[idx] <- sapply(res[idx], function(x) c*p*sum((1-p)^(0:(x-1))))
    return(res)
}

# arguments:
#
# q: a vector of numbers in [0,1]
# 
# p: the "success" probability
#
# k: the right end of the support of the distribution
#
# return value:
# TODO:
#
# find the first c that P(X<=c)>q
# TODO: for each element in q, findInterval(q, intervals),
# need to handle some coner cases too.
qgeomk <- function(q, p, k) {
    intervals <- cbind(c(0:k), pgeomk(c(0:k),p,k))
}

# arguments:
#
# n: a positive integer
# 
# p: the "success" probability
#
# k: the right end of the support of the distribution
#
# return value:
# TODO:
#
rgeomk <- function(n, p, k) {

}

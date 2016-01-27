parta <- function(nreps)
{
  L1 <- 0 #B1
  B2 <- 0
  A2 <- 0
  L2 <- 0

    L1 <- sample(0:2, nreps, prob = c(0.5,0.4,0.1),replace = TRUE)

for(n in 1:nreps)
{
    for(k in 1:L1[nreps])
    {
      A2[k] <- A2[k] + sample(0:1, 1, prob = c(0.8,0.2))
    }
}

    B2 <- sample(0:2, nreps, prob = c(0.5,0.4,0.1),replace = TRUE)

    L2 <- L1 + B2 - A2

  return (cov(L1,L2))
}

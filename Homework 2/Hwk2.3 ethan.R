parta <- function(nreps)
{
  D4 <- 0
  N4 <- 0
  N3 <- 0

  for(n in 1:nreps)
  {
    d1 <- 1
    d2 <- 1

    N3[n] <- sample(1:2, 1, prob = c(0.5,0.5),replace = TRUE)

    d1 <- d1 + (2 - N3[n])

    d2 <- d2 + (N3[n] - 1)

    d3 <- 1

    N4[n] <- sample(1:3, 1, prob = c(0.25 * (3 - N3[n]), 0.25 * N3[n],0.25),replace = TRUE)

    if (N4[n] == 1) D4[n] <- d1
    if (N4[n] == 2) D4[n] <- d2
    if (N4[n] == 3) D4[n] <- d3

  }

return (mean(D4))
}

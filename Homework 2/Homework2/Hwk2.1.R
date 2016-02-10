parta <- function(nreps)
{
  sums <- 0

  for (n in 1:nreps)
  {
     temp <- 0
     k <- 0
     while (k <= 4)
     {
       temp <- sample(1:3, 1)
       k <- k + temp
       if (k > 4) sums <- sums + temp
     }
  }
  return (sums / nreps)
}

partb <- function(nreps)
{
  sums <- 0
  sumssuqare <-0

  for (n in 1:nreps)
  {
     temp <- 0
     k <- 0
     while (k <= 4)
     {
       temp <- sample(1:3, 1)
       k <- k + temp
       if (k > 4)
       {
         sums <- sums + temp
         sumssuqare <- sumssuqare + temp^2
       }
     }
  }
  return ((sumssuqare / nreps)-(sums/nreps)^2)
}

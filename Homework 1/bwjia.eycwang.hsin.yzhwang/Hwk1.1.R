parta <- function(nreps)
{
trials <- nreps
matches <- 0
for (n in 1:trials)
{
   count <- -1
   k <- 0
   while (k <= 4)
   {
     count <- count + 1
     k <- k + sample(1:3, 1)
   }
   if (count == 3) matches <- matches + 1
}
return (matches / trials)
}

partb <- function(nreps)
{
  trials <- nreps
  matches <- 0
  for (n in 1:trials)
  {
     k <- 0
     while (k < 4)
     {
       k <- k + sample(1:3, 1)
       if (k > 4) matches <- matches + 1
     }
  }
  return (matches / trials)
}

partc <- function(nreps)
{
  matches <- 0
  for (n in 1:nreps)
  {
     temp <- 0
     k <- 0
     while (k <= 4)
     {
       temp <- sample(1:3, 1)
       k <- k + temp
       if (k > 4 && temp == 1) matches <- matches + 1
     }
  }
  return (matches / nreps)
}

partd <- function(nreps)
{
  base <- 0
  matches <- 0
  for (n in 1:nreps)
  {
     temp <- 0
     k <- 0
     box1 <- 0
     while (k <= 4)
     {
       temp <- sample(1:3, 1)
       if (k == 0 && temp == 1) box1 <- 1
       k <- k + temp
       if (k > 4 && temp == 1) matches <- matches + 1
       if (k > 4 && temp == 1 && box1 == 1) base <- base + 1
     }
  }
  return (base / matches)
}

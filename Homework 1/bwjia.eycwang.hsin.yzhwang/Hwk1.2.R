parta <- function(nreps)
{
  matches <- 0
  for (n in 1:nreps)
  {
    jack <- 2
    jill <- 0
    jackbonus <- 0
    jillbonus <- 0
    jackroll <- sample(1:6, 1)
    jillroll <- sample(1:6, 1)
    jack <- jack + jackroll
    jill <- jill + jillroll
    jackroll2 <- sample(1:6, 1)
    jillroll2 <- sample(1:6, 1)
    if (jack %% 8 == 3) jackbonus <- 1
    if (jill %% 8 == 3) jillbonus <- 1
    if (jackbonus == 1) jack <- jack + jackroll2
    if (jillbonus == 1) jill <- jill + jillroll2
    if (jack %% 8 == 0) matches <- matches + 1
  }
  return (matches / nreps)
}

partb <- function(nreps)
{
  matches <- 0
  for (n in 1:nreps)
  {
    jack <- 2
    jill <- 0
    jackbonus <- 0
    jillbonus <- 0
    jackroll <- sample(1:6, 1)
    jillroll <- sample(1:6, 1)
    jack <- jack + jackroll
    jill <- jill + jillroll
    jackroll2 <- sample(1:6, 1)
    jillroll2 <- sample(1:6, 1)
    if (jack %% 8 == 3) jackbonus <- 1
    if (jill %% 8 == 3) jillbonus <- 1
    if (jackbonus == 1) jack <- jack + jackroll2
    if (jillbonus == 1) jill <- jill + jillroll2
    if (jill > jack) matches <- matches + 1
  }
  return (matches / nreps)
}

partc <- function(nreps)
{
  base <-0
  matches <- 0
  for (n in 1:nreps)
  {
    jack <- 2
    jill <- 0
    jackbonus <- 0
    jillbonus <- 0
    jackroll <- sample(1:6, 1)
    jillroll <- sample(1:6, 1)
    jack <- jack + jackroll
    jill <- jill + jillroll
    jackroll2 <- sample(1:6, 1)
    jillroll2 <- sample(1:6, 1)
    if (jack %% 8 == 3) jackbonus <- 1
    if (jill %% 8 == 3) jillbonus <- 1
    if (jackbonus == 1) jack <- jack + jackroll2
    if (jillbonus == 1) jill <- jill + jillroll2
    if (jack == jill) matches <- matches + 1
    if (jack == jill && jackbonus == 0 && jillbonus == 0) base <- base + 1
  }
  return (base / matches)
}

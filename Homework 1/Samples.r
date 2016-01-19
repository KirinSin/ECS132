a1 <- function(nreps)
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

a2 <- function(nreps)
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

a3 <- function(nreps)
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

a4 <- function(nreps)
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

b1 <- function(nreps)
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

b2 <- function(nreps)
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

b3 <- function(nreps)
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

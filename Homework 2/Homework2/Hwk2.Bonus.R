parta <- function(p,q,r,nreps)
{
	x <- 0
	x <- sample(0:1, nreps, prob = c(1-p,p),replace = TRUE)
	y <- 0

	for (i in 1:nreps)
	{
		if (x[i] == 1) y[i] <- sample(0:1, 1, prob = c(1-r/p,r/p),replace = TRUE)
		else y[i] <- sample(0:1, 1, prob = c(1-(q-r/p),q-r/p),replace = TRUE)
	}

	return (var(x+y))
}
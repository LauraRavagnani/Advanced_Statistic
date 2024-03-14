fibonacci <- function(n){
	x <- c(rep(1,n))
	y <- x

	for (i in 3:n){
		y[i] = y[i-1] + y[i-2]
	}
	return(y[n])
}

fibonacci(20)
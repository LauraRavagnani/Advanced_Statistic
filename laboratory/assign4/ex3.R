library(ggplot2)

# number of data 
n.data <- c(1, 10, 50, 100, 1000, 5000)
n.sample <- 200

a.true <- 1		# km
b.true <- 1.5 	# km

a <- seq(-2, 2, 4/n.sample)
b <- seq(1, 3,	2/n.sample)

par(mfrow = c(2, 3))
for(i in n.data){
	# angle of emission
	theta <- runif(i, 0, pi)

	# position of the flash arrival
	data <- a.true + b.true * tan(theta)

	# posterior distribution is proportional to the likelihood
	# take the logarithm of the likelihood
	logL <- matrix(1, nrow = length(a), ncol = length(b))

	for(x in data){
		for(j in 1:length(a)){
			for(k in 1:length(b)){
				logL[j,k] <- logL[j,k] + log(b[k] / (b[k]^2 + (x - a[j])^2))
			}
		}
	}

	post <- logL - max(logL)
	post <- exp(post)

	df <- matrix(0, nrow = length(a)*length(b), ncol = 3)
	val <- 1
	
		for(j in 1:length(a)){
			for(k in 1:length(b)){
				df[val,1] <- a[j]
				df[val,2] <- b[k]
				df[val,3] <- post[j,k]

				val <- val+1
			}
		}

	df <- as.data.frame(df)

	contour(a, b, post,
		nlevels = 5,
		labcex = 0.5,
		xlim = c(0, 2), ylim = c(1, 2),
		main = paste("# data = ", i),
		xlab = "a", ylab = "b")
	abline(v = a.true,
		col = "red")
	abline(h = b.true,
		col = "red")
}
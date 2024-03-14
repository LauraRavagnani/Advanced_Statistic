n <- 10
y <- c(5, 8, 4, 6, 11, 6, 6, 5, 6, 4)
mu <- seq(0, 10, 0.01)

# the likelihood is a poisson distribution with mu unknown: f({y_i}|mu) is proportional to mu^{sum(y_i)} * e^{-n mu}

# function to find the mean
first <- function(f){
	mean <- 0
	for(i in 1:length(f)){
		mean <- mean + mu[i] * f[i]
	}
	return(mean)
}

# function to find the median
find_median <- function(f){
	p <- 0
	integral <- sum(f)
	for(i in 1:length(f)){
		p <- p + f[i]
		if(p > 0.5 * integral){
			break
		}
	}
	return(mu[i-1])
}

# function to find the variance
second <- function(f){
	E_2 <- 0
	for(i in 1:length(f)){
		E_2 <- E_2 + mu[i]^2 * f[i]
	}
	var <- E_2 - first(f)^2
	return(var)
}

# function to find the min of the credibility interval (2.5%)
min_ci <- function(f){
	val <- 0
	integral <- sum(f)
	for(i in 1:length(f)){
		val <- val + f[i]
		if(val > 0.025 * integral){
			break
		}
	}
	return(mu[i-1])
}

# function to find the max of the credibility interval (97.5%)
max_ci <- function(f){
	val <- 0
	integral <- sum(f)
	for(i in 1:length(f)){
		val <- val + f[i]
		if(val > 0.975 * integral){
			break
		}
	}
	return(mu[i-1])
}

# a) -> uniform prior
unif_prior <- rep(1, length(mu))

# posterior is proportional to a Gamma(alpha = sum(y_i) + 1, lambda = n)

sum_y <- sum(y)
unif_posterior.star <- dgamma(mu, sum_y + 1, n)		# not normalized
unif_posterior <- unif_posterior.star / sum(unif_posterior.star)

# b) -> Jeffrey's prior 
jeff_prior <- 1/sqrt(mu)

# posterior is proportional to a Gamma(alpha = sum(y_i) + 1/2, lambda = n)

jeff_posterior.star <- dgamma(mu, sum_y + 1/2, n)		# not normalized
jeff_posterior <- jeff_posterior.star / sum(jeff_posterior.star)


# plot the priors
plot(mu, jeff_prior, type = 'l', lty = 2, lwd = 3, col = "orchid3", xlab = expression(paste(mu)), ylab = expression(paste("P(y|", mu, ")")), main = "Priors")
lines(mu, unif_prior, type = 'l', lty = 2, lwd = 3, col = "darkgoldenrod2")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
legend(7, 9, legend = c("uniform", "Jeffrey's"), col = c("darkgoldenrod2", "orchid3"), lty = 1,, lwd = 3, cex = 1)

# mean (first moment)
unif_mean <- first(unif_posterior)
jeff_mean <- first(jeff_posterior)

# median
unif_median <- find_median(unif_posterior)
jeff_median <- find_median(jeff_posterior)

# variance (second moment)
unif_variance <- second(unif_posterior)
jeff_variance <- second(jeff_posterior)

# credibility interval
unif_min_ci <- min_ci(unif_posterior)
unif_max_ci <- max_ci(unif_posterior)
jeff_min_ci <- min_ci(jeff_posterior)
jeff_max_ci <- max_ci(jeff_posterior)


# plot the posteriors
plot(mu, unif_posterior, type = 'l', lwd = 3, col = "darkgoldenrod2", xlab = expression(paste(mu)), ylab = expression(paste("P(", mu, "|y)")), main = "Posteriors")
abline(v = unif_min_ci, lty = 2, lwd = 2, col = "darkgoldenrod2")
abline(v = unif_max_ci, lty = 2, lwd = 2, col = "darkgoldenrod2")

lines(mu, jeff_posterior, type = 'l', col = "orchid3", lwd = 3)
abline(v = jeff_min_ci, lty = 2, lwd = 2, col = "orchid3")
abline(v = jeff_max_ci, lty = 2, lwd = 2, col = "orchid3")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
legend(0.5, 0.0045, legend = c("uniform", "Jeffrey's"), col = c("darkgoldenrod2", "orchid3"), lty = 1,, lwd = 3, cex = 1)


# summary table
prior <- c("uniform", "Jeffrey's")
mean <- c(unif_mean, jeff_mean)
median <- c(unif_median, jeff_median)
variance <- c(unif_variance, jeff_variance)
min <- c(unif_min_ci, jeff_min_ci)
max <- c(unif_max_ci, jeff_max_ci)

df <- data.frame(prior, mean = round(mean, 3), median, variance = round(variance, 2), min, max)
print(df)

# c) normal approximations
unif_norm <- dnorm(mu, unif_mean, unif_variance)
jeff_norm <- dnorm(mu, jeff_mean, jeff_variance)

# credibility interval
unif_norm_min_ci <- min_ci(unif_norm)
unif_norm_max_ci <- max_ci(unif_norm)
jeff_norm_min_ci <- min_ci(jeff_norm)
jeff_norm_max_ci <- max_ci(jeff_norm)


# summary table
distribution <- c("uniform", "normal approx", "-", "Jeffrey's", "normal approx")
min <- c(unif_min_ci, unif_norm_min_ci, "-", jeff_min_ci, jeff_norm_min_ci)
max <- c(unif_max_ci, unif_norm_max_ci, "-", jeff_max_ci, jeff_norm_max_ci)

df2 <- data.frame(distribution, min, max)
print(df2)

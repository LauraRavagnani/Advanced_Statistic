n <- 75
y <- 6
n.sample <- 1000

p <- seq(0, 1, 1/n.sample) 

# a) binomial

# b) frequentist estimator (unbiased)
p_F <- y / n

cat(paste("The frequentist estimator of the failure probability of the new method is:", p_F, '\n'))

# mean and variance of the prior Beta(a, b)
p_0 <- 0.15
stdev <- 0.14^2

a <- p_0 * (p_0 - p_0^2 - stdev) / stdev   
b <- (p_0 - 2*p_0^2 - stdev + p_0^3 + p_0*stdev) / stdev 

# posterior distribution Beta(a_2, b_2)
a_2 <- a + y 
b_2 <- b + n - y

posterior <- dbeta(p, a_2, b_2)

# mean and variance
mean <- integrate(function(p) dbeta(p, a_2, b_2) * p, 0, 1)	

second_moment <- integrate(function(p) dbeta(p, a_2, b_2) * p^2, 0, 1)
variance <- second_moment$value - (mean$value)^2 

sigma_1 <- mean$value - sqrt(variance)
sigma_2 <- mean$value + sqrt(variance) 

plot(p, posterior,
	type = 'l', lwd = 3,
	col = "green3",
	xlim = c(0, 0.3))
abline(v = mean$value,
	lty = 3, lwd = 2,
	col = "black")
abline(v = sigma_1,
	lty = 2, lwd = 2,
	col = "firebrick")
abline(v = sigma_2,
	lty = 2, lwd = 2,
	col = "firebrick")
grid(nx = NULL, ny = NULL,
	lty = 2, lwd = 0.5,
	col = "gray")
legend(0.17, 12,
	legend = c(paste("Beta(", round(a_2, 1), ",", round(b_2, 1), ")"), "mean", "variance"),
	col = c("green3", "black", "firebrick"),
	lty = c(1, 3, 2), lwd = 2, cex = 1)


# c) one-side hypothesis test (bayesian approach)
# find P(p >= p_0| y = 6)

test <- integrate(function(p) dbeta(p, a_2, b_2), 0.15, 1)

# plot
plot(p, posterior,
	type = 'l', lwd = 3,
	col = "green3",
	xlim = c(0, 0.3))
polygon(c(p[p >= p_0], p_0),
	c(posterior[p >= p_0], 0),
        col = "purple",
        border = 1,
        text(x = 0.2, y = 2, paste("Area = ", round(test$value, 4)), col = "purple"))
grid(nx = NULL, ny = NULL,
	lty = 2, lwd = 0.5,
	col = "gray")
	

# d) one-side hypothesis test (frequentist approach)
# null distribution
null <- dbinom(0:30, n, p_0)

# find if the p-value is < alpha
p.value <- 0 
for(i in 0:y){
	p.value <- p.value + dbinom(i, n, p_0)
}
p.value

barplot(null, names = 0:30,
	col = c(rep("magenta", 7), rep("blue", 24)), angle = 45, density = 40,
	xlab = "y", ylab = "1 - F(y)",
	main = "Bin(n = 75, p = 0.15)")
abline(h = 0.05,
	col = "red", lwd = 3, lty = 4,
	text(x = 32, y = 0.055, label = "alpha = 5%", col = "red"))








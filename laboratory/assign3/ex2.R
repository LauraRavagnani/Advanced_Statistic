# ASSUMING BINOMIAL INFERENCE
# WRITE THE POSTERIOR
n <- 117
y <- 17
n.sample <- 1000
p <- seq(1 / n.sample, 1, 1 / n.sample)

# uniform
unif_posterior.star <- dbinom(y, n, p)   # un-normalized posterior
unif_posterior <- unif_posterior.star / (sum(unif_posterior.star) / n.sample)

# beta
beta_posterior.star <- dbinom(y, n + 3, p)   # un-normalized posterior
beta_posterior <- beta_posterior.star / (sum(beta_posterior.star) / n.sample)


# FIRST AND SECOND MOMENTS
first <- function(f){
    mean <- sum(p * f) / n.sample

    return(mean)
}

stdev <- function(f){
    E_2 <- sum(p^2 * f) / n.sample
    var <- E_2 - first(f)^2
    sigma <- sqrt(var)

    return(sigma)
}

# uniform
unif_mean <- first(unif_posterior)
unif_sigma <- stdev(unif_posterior)

# beta
beta_mean <- first(beta_posterior)
beta_sigma <- stdev(beta_posterior)


# CREDIBILITY INTERVAL
# functions to find the interval
ci_min <- function(f){
    integral <- sum(f)
    val <- 0
    for(i in 1:length(f)){
        val <- val + f[i]
        if(val > integral * 0.025){
            break
        }
    }
    return(i / 1000)
}

ci_max <- function(f){
    integral <- sum(f)
    val <- 0
    for(i in 1:length(f)){
        val <- val + f[i]
        if(val > integral * 0.975){
            break
        }
    }
    return(i / 1000)
}

# uniform
unif_min <- ci_min(unif_posterior)
unif_max <- ci_max(unif_posterior)

# beta
beta_min <- ci_min(beta_posterior)
beta_max <- ci_max(beta_posterior)


# PLOT
plot(p, unif_posterior, type = "l", col = "red", lwd = 3, xlim = c(0, 0.4), cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = unif_min, lty = 2, lwd = 1, col = "red")
abline(v = unif_max, lty = 2, lwd = 1, col = "red")

lines(p, beta_posterior, type = "l", col = "blue", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = beta_min, lty = 2, lwd = 1, col = "blue")
abline(v = beta_max, lty = 2, lwd = 1, col = "blue")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)

legend(0.28, 11.5, legend = c("uniform", "Beta(1,4)"), col = c("red", "blue"), lty = 1,, lwd = 3, cex = 1)


# FIND THE NORMAL APPROXIMATION
# uniform
unif_gauss <- dnorm(p, unif_mean, unif_sigma)

# beta
beta_gauss <- dnorm(p, beta_mean, beta_sigma)


# CREDIBILITY INTERVAL FOR GAUSSIAN APPROXIMATION
# uniform
unif_gauss_min <- ci_min(unif_gauss)
unif_gauss_max <- ci_max(unif_gauss)

# beta
beta_gauss_min <- ci_min(beta_gauss)
beta_gauss_max <- ci_max(beta_gauss)


# PLOT
# uniform and gauss approx
plot(p, unif_posterior, type = "l", col = "red", lwd = 3, xlim = c(0, 0.4), cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = unif_min, lty = 2, lwd = 1, col = "red")
abline(v = unif_max, lty = 2, lwd = 1, col = "red")

lines(p, unif_gauss, type = "l", col = "dark orange", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = unif_gauss_min, lty = 2, lwd = 1, col = "dark orange")
abline(v = unif_gauss_max, lty = 2, lwd = 1, col = "dark orange")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)

legend(0.26, 11.5, legend = c("uniform", "normal approx."), col = c("red", "dark orange"), lty = 1,, lwd = 3, cex = 1)

# beta and gauss approx
plot(p, beta_posterior, type = "l", col = "blue", lwd = 3, xlim = c(0, 0.4), cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = beta_min, lty = 2, lwd = 1, col = "blue")
abline(v = beta_max, lty = 2, lwd = 1, col = "blue")

lines(p, beta_gauss, type = "l", col = "cyan3", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = beta_gauss_min, lty = 2, lwd = 1, col = "cyan3")
abline(v = beta_gauss_max, lty = 2, lwd = 1, col = "cyan3")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)

legend(0.256, 11.5, legend = c("Beta(1,4)", "normal approx."), col = c("blue", "cyan3"), lty = 1,, lwd = 3, cex = 1)


# SUMMARY TABLES
# uniform
unif_prior_dist <- c("uniform","normal approximation")
unif_min_conf.int <- c(unif_min, unif_gauss_min)
unif_max_conf.int <- c(unif_max, unif_gauss_max)

df_unif <- data.frame(unif_prior_dist, unif_min_conf.int, unif_max_conf.int)
print(df_unif)

# beta
beta_prior_dist <- c("Beta(1,4)","normal approximation")
beta_min_conf.int <- c(beta_min, beta_gauss_min)
beta_max_conf.int <- c(beta_max, beta_gauss_max)

df_beta <- data.frame(beta_prior_dist, beta_min_conf.int, beta_max_conf.int)
print(df_beta)
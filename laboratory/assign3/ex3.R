# outcomes of the flippings
outcome <- c("T", "T", "T", "T", "T", "H", "T", "T", "H", "H", "T", "T", "H", "H", "H", "T", "H", "T", "H", "T", "H", "H", "T", "H", "T", "H", "T", "H", "H", "H")
for(i in 1:length(outcome)){
    outcome[i] <- ifelse(outcome[i] == "T", 1, 0)
}

outcome <- as.numeric(outcome)
str(outcome)

n <- 30
y <- sum(outcome)     # outcome is H
n.sample <- 1000
p <- seq(1 / n.sample, 1, 1 / n.sample)


# BINOMIAL LIKELIHOOD
likelihood <- dbinom(y, n, p)

plot(p, likelihood, type = "l", col = "green3", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(y|p,n,H)")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
legend(0.7, 0.13, legend = "binomial \n likelihood", col = "green3", lty = 1,, lwd = 3, cex = 1)


# PRIORS
# uniform
unif_prior <- rep(0.5, length(p))

# beta
alpha <- 5
beta <- 5
beta_prior <- dbeta(p, alpha, beta)

# plot
plot(p, beta_prior, type = "l", col = "dark turquoise", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "Prior[p]")
lines(p, unif_prior, type = "l", col = "magenta", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "Prior[p]")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
legend(0.75, 2.25, legend = c("uniform", "Beta(5,5)"), col = c("magenta", "dark turquoise"), lty = 1,, lwd = 3, cex = 1)


# POSTERIORS
# uniform
unif_posterior.star <- dbinom(y, n, p) * unif_prior   # un-normalized posterior
unif_posterior <- unif_posterior.star / (sum(unif_posterior.star) / n.sample)

# beta
beta_posterior.star <- dbinom(y, n, p) * beta_prior   # un-normalized posterior
beta_posterior <- beta_posterior.star / (sum(beta_posterior.star) / n.sample)

# plots
plot(p, beta_posterior, type = "l", col = "dark turquoise", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
lines(p, unif_posterior, type = "l", col = "magenta", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
legend(0.75, 4.5, legend = c("uniform", "Beta(5,5)"), col = c("magenta", "dark turquoise"), lty = 1,, lwd = 3, cex = 1)


# MOST PROBABLE VALUE
cat(paste("The most probable value for the coin probability p is:", p[which.max(unif_posterior)], "using an uniform prior, and:", p[which.max(beta_posterior)], "using a Beta(5,5) prior", '\n'))


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

# plot
plot(p, beta_posterior, type = "l", col = "dark turquoise", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = beta_min, lty = 2, lwd = 1, col = "dark turquoise")
abline(v = beta_max, lty = 2, lwd = 1, col = "dark turquoise")

lines(p, unif_posterior, type = "l", col = "magenta", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
abline(v = unif_min, lty = 2, lwd = 1, col = "magenta")
abline(v = unif_max, lty = 2, lwd = 1, col = "magenta")

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
legend(0.75, 4.5, legend = c("uniform", "Beta(5,5)"), col = c("magenta", "dark turquoise"), lty = 1,, lwd = 3, cex = 1)

# summary table
prior_dist <- c("uniform","Beta(5,5)")
min_conf.int <- c(unif_min, beta_min)
max_conf.int <- c(unif_max, beta_max)

df_unif <- data.frame(prior_dist, min = min_conf.int, max = max_conf.int)
print(df_unif)


# SEQUENTIAL ANALYSIS
toss <- seq(1:n)

unif_most_prob <- rep(0, n)
seq_unif_min <- rep(0, n)
seq_unif_max <- rep(0, n)

beta_most_prob <- rep(0, n)
seq_beta_min <- rep(0, n)
seq_beta_max <- rep(0, n)

for(i in 1:n){
    seq_n <- i 
    seq_y <- 0
    for(j in 1:i){
        seq_y <- seq_y + outcome[j]
    }

    # uniform
    seq_unif_posterior.star <- dbinom(seq_y, seq_n, p) * unif_prior   # un-normalized posterior
    seq_unif_posterior <- seq_unif_posterior.star / (sum(seq_unif_posterior.star) / n.sample)

    unif_most_prob[i] <- p[which.max(seq_unif_posterior)]
    seq_unif_min[i] <- ci_min(seq_unif_posterior)
    seq_unif_max[i] <- ci_max(seq_unif_posterior)

    # beta
    seq_beta_posterior.star <- dbinom(seq_y, seq_n, p) * beta_prior   # un-normalized posterior
    seq_beta_posterior <- seq_beta_posterior.star / (sum(seq_beta_posterior.star) / n.sample)

    beta_most_prob[i] <- p[which.max(seq_beta_posterior)]
    seq_beta_min[i] <- ci_min(seq_beta_posterior)
    seq_beta_max[i] <- ci_max(seq_beta_posterior)

    # plot
    if(i %% 5 == 0){
        plot(p, seq_beta_posterior, type = "l", col = "dark turquoise", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)", main = i)
        abline(v = seq_beta_min[i], lty = 2, lwd = 1, col = "dark turquoise")
        abline(v = seq_beta_max[i], lty = 2, lwd = 1, col = "dark turquoise")

        lines(p, seq_unif_posterior, type = "l", col = "magenta", lwd = 3, cex.lab = 1.5, xlab = "p", ylab = "P(p|y,n,H)")
        abline(v = seq_unif_min[i], lty = 2, lwd = 1, col = "magenta")
        abline(v = seq_unif_max[i], lty = 2, lwd = 1, col = "magenta")

        grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)
    }

    # posterior becomes the prior
    unif_prior <- seq_unif_posterior
    beta_prior <- seq_beta_posterior
}

# summary table
unif_df <- data.frame(toss, most_probable = unif_most_prob, min = seq_unif_min, max = seq_unif_max)
beta_df <- data.frame(toss, most_probable = beta_most_prob, min = seq_beta_min, max = seq_beta_max)






































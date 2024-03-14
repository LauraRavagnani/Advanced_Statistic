# select a random box (from 0 to 5)
j <- floor(runif(1, 0, 6))

n_trial <- 100
trial <- 0:n_trial

# outcome
outcome <- rbinom(n_trial, 1, j/5)  # 1 = white, 0 = black
E <- c("-", outcome)

cat(paste("Box selected:", "H", j, '\n'))

H0 <- rep(0.167, length(trial))
H1 <- rep(0.167, length(trial))
H2 <- rep(0.167, length(trial))
H3 <- rep(0.167, length(trial))
H4 <- rep(0.167, length(trial))
H5 <- rep(0.167, length(trial))

sum <- rep(0, length(trial))

for (i in 2:length(trial)) {

    if(E[i] == 1){
        sum[i-1] <- 1/5 * H1[i-1] + 2/5 * H2[i-1] + 3/5 * H3[i-1] + 4/5 * H4[i-1] + H5[i-1]

        H0[i] <- 0
        H1[i] <- 1/5 * H1[i-1] / sum[i-1]
        H2[i] <- 2/5 * H2[i-1] / sum[i-1]
        H3[i] <- 3/5 * H3[i-1] / sum[i-1]
        H4[i] <- 4/5 * H4[i-1] / sum[i-1]
        H5[i] <- 5/5 * H5[i-1] / sum[i-1]
    }
    else{
        sum[i-1] <- H0[i-1] + 4/5 * H1[i-1] + 3/5 * H2[i-1] + 2/5 * H3[i-1] + 1/5 * H4[i-1]

        H0[i] <- 5/5 * H0[i-1] / sum[i-1]
        H1[i] <- 4/5 * H1[i-1] / sum[i-1]
        H2[i] <- 3/5 * H2[i-1] / sum[i-1]
        H3[i] <- 2/5 * H3[i-1] / sum[i-1]
        H4[i] <- 1/5 * H4[i-1] / sum[i-1]
        H5[i] <- 0
    }
}

# plots
plot(trial, H0, type = "p", pch = 19, col = "red", ylab = "probability", main = "H0", ylim = c(0,1))
plot(trial, H1, type = "p", pch = 19, col = "orange", ylab = "probability", main = "H1", ylim = c(0,1))
plot(trial, H2, type = "p", pch = 19, col = "yellow2", ylab = "probability", main = "H2", ylim = c(0,1))
plot(trial, H3, type = "p", pch = 19, col = "green", ylab = "probability", main = "H3", ylim = c(0,1))
plot(trial, H4, type = "p", pch = 19, col = "blue", ylab = "probability", main = "H4", ylim = c(0,1))
plot(trial, H5, type = "p", pch = 19, col = "purple", ylab = "probability", main = "H5", ylim = c(0,1))

df <- data.frame(trial, E, H0, H1, H2, H3, H4, H5)
head(df, 20)




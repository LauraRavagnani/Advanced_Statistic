# A)
# simulation of the waiting time
lambda <- 1 / 30

random <- rexp(60, lambda)
data <- data.frame(person = 1:60, waiting_time = random[order(random)])

barplot(data$waiting_time, names = data$person)

# B)
cat(paste("The probability that a person will wait for less than 12 minutes is: ", pexp(12,lambda), '\n'))


# C)
# expected value
E <- 1 / lambda

# average waiting time
mean_waiting <- mean(data$waiting_time)

# comparison
cat(paste("The average waiting time of the simulated data is T_sim = ", mean_waiting, ", the expected value is, instead T_exp = ", E, '\n'))

# D) 
#probability of waiting more than one hour
cat(paste("The probability that a person will wait for more than one hour is: ", 1 - pexp(60,lambda), '\n'))
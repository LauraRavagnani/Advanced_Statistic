# function that picks random arrival time in minutes from 10:45 : 0 -> 10:45, 60 -> 11.45
arrival_time <- function(n_people){
	x <- c(round(runif(n_people, 0, 60),0))
	return(x)
}

# time of scheduled trains in minutes from 10:45 : 15 -> 11:00 train, 45 -> 11:30 train, 75 -> 12:00 train
sched_time <- c(15, 45, 75)

# function that calculates the waiting time in minutes
waiting_time <- function(n_people){
	wait <- rep(0, n_people)
	arrival <- arrival_time(n_people)

	for(i in 1:n_people){
		if(arrival[i] <= sched_time[1]){
			wait[i] <- sched_time[1] - arrival[i]
		} else if(arrival[i] > sched_time[1] && arrival[i] <= sched_time[2]){
			wait[i] <- sched_time[2] - arrival[i]
		} else {
			wait[i] <- sched_time[3] - arrival[i]
		}
	}
	return(wait)
}

# create a dataframe with the waiting time and the frequency
df <- as.data.frame(table(waiting_time(100)))
df$Var1 <- as.numeric(df$Var1)


# a)
# function that calculates the probability that has to wait at most 10 minutes
max_10 <- function(n_people){
	a <- with(df, sum(Freq[Var1 <= 10]))/n_people
	return(a)
}

cat(paste("The probability that a person has to wait at most 10 minutes is: ", max_10(100) * 100, "%", '\n'))


# b)
# function that calculates the probability that has to wait at least 15 minutes
min_15 <- function(n_people){
	b <- with(df, sum(Freq[Var1 >= 15]))/n_people
	return(b)
}

cat(paste("The probability that a person has to wait at least 15 minutes is: ", min_15(100) * 100, "%", '\n'))


# c)
# function that calculates the average time spent waiting
average_time <- function(n_people){
	c <- sum(waiting_time(n_people))/n_people
	return(c)
}

cat(paste("The average time spent waiting is: ", average_time(100), " minutes", '\n'))



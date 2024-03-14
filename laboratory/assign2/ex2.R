library(ggplot2)

# 1)
# write the pdf
dtriang <- function(x, a, c, b){
	pdf <- ifelse(x < c, 2*(x - a) / ((b - a) * (c - a)), 2*(b - x) / ((b - a) * (b - c)))

	return(pdf)
}

k <- 0:10

plot(k, dtriang(k, 0, 5, 10), type="l")


# 2) function to generate random numbers
# cdf
ptriang <- function(x, a, c, b){
	val <- ifelse(x < c, ((x-a)^2)/((b-a)*(c-a)), (a*c - a*b - b*c + 2*b*x -(x^2))/((b-a)*(b-c)))

	return(val)
}

plot(k,ptriang(k, 0, 5, 10), type = "l")

# function that samples from the pdf using the inverse method
rtriang <- function(n, a, c, b){
	u <- runif(n)
	val <- ifelse(u < (c - a)/(b - a), a + sqrt(u * (b - a) * (c - a)), b - sqrt(b^2 - a * b - b * c + a * c - u * (b - a) * (b - c)))

	return(val)
}

# create the plot
df_line <- data.frame(k, dtriang(k, 0, 5, 10))
df_histo <- as.data.frame(rtriang(10^4, 0, 5, 10))

plot <- ggplot(df_histo, aes(x=rtriang(10^4, 0, 5, 10))) + geom_histogram(aes(y=after_stat(density))) + geom_line(df_line, mapping=aes(x = k, y = dtriang(k, 0, 5, 10)))
plot










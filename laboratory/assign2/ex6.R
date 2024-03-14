# annual return rate = profit/investment

investment <- 200 * 85


# plot of the gain
x <- seq(-0.4 * investment,0.6 * investment,1)
plot(x, dnorm(x,0.1 * investment, 0.12 * investment), type="l")

# probability that after a year his net profit from the investment is at least 800e
prob <- 1 - pnorm(800, 0.1 * investment, 0.12 * investment)

cat(paste("The probability that after a year his net profit from the investment is at least 800€ is: ", prob * 100, " %", '\n'))
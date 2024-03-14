# model 
signal <- function(x, a, b, x0, w, t){
	t * (a * exp(-(x-x0)^2/(2*w^2)) + b)
}

# model parameters
x0 <- 0 		# signal peak
w <- c(0.1, 0.25, 1, 2, 3)			# signal width
a.real <- 2		# signal amplitude
b.real <- 1		# background amplitude
delta.t <- 5	#exposure time

set.seed(307)
par(mfrow = c(2, 3))

for(i in 1:length(w)){

	# generate observed data
	xdat <- seq(from = -7*w[i], to = 7*w[i], by = 0.5*w[i])
	s.true <- signal(xdat, a.real, b.real, x0, w[i], delta.t)
	distrib_dat <- rpois(length(s.true), s.true)

	#xplot <- seq(min(xdat), max(xdat), 0.05*w[i])
	#splot <- signal(xplot, a.real, b.real, x0, w[i], delta.t)

	# sampling grid for computing posterior
	alim <- c(0, 4)
	blim <- c(0.5, 1.5)
	Nsamp <- 100
	unigrid <- seq(1/(2*Nsamp), 1-1/(2*Nsamp), 1/Nsamp)
	delta_a <- diff(alim/Nsamp)
	delta_b <- diff(blim/Nsamp)
	a <- alim[1] + diff(alim)*unigrid
	b <- blim[1] + diff(blim)*unigrid

	# log posterior
	log.post <- function(d, x, a, b, x0, w, t){
		if(a < 0 || b < 0) {return(-Inf)}
		sum(dpois(d, lambda = signal(x, a, b, x0, w, t), log = TRUE))
	}

	z <- matrix(data = NA, nrow = length(a), ncol = length(b))
	for(j in 1:length(a)){
		for(k in 1:length(b)){
			z[j,k] <- log.post(distrib_dat, xdat, a[j], b[k], x0, w[i], delta.t)
		}
	}

	z <- z - max(z)

	contour(a, b, exp(z), nlevels = 5, labcex = 0.5, main = paste("w = ", w[i]))
	abline(v = a.real,
		col = "red")
	abline(h = b.real,
		col = "red")
}


# model parameters
x0 <- 0 		# signal peak
w <- 1			# signal width
ratio <- c(0.5, 1, 1.5, 2, 2.5, 3)
a.real <- 1		# signal amplitude
b.real <- a.real * ratio		# background amplitude
delta.t <- 5	#exposure time

set.seed(135)
par(mfrow = c(2, 3))
for(i in 1:length(ratio)){

	# generate observed data
	xdat <- seq(from = -7*w, to = 7*w, by = 0.5*w)
	s.true <- signal(xdat, a.real, b.real[i], x0, w, delta.t)
	distrib_dat <- rpois(length(s.true), s.true)

	#xplot <- seq(min(xdat), max(xdat), 0.05*w[i])
	#splot <- signal(xplot, a.real, b.real, x0, w[i], delta.t)

	# sampling grid for computing posterior
	alim <- c(0, 4)
	blim <- c(0, 4)
	Nsamp <- 100
	unigrid <- seq(1/(2*Nsamp), 1-1/(2*Nsamp), 1/Nsamp)
	delta_a <- diff(alim/Nsamp)
	delta_b <- diff(blim/Nsamp)
	a <- alim[1] + diff(alim)*unigrid
	b <- blim[1] + diff(blim)*unigrid

	# log posterior
	log.post <- function(d, x, a, b, x0, w, t){
		if(a < 0 || b < 0) {return(-Inf)}
		sum(dpois(d, lambda = signal(x, a, b, x0, w, t), log = TRUE))
	}

	z <- matrix(data = NA, nrow = length(a), ncol = length(b))
	for(j in 1:length(a)){
		for(k in 1:length(b)){
			z[j,k] <- log.post(distrib_dat, xdat, a[j], b[k], x0, w, delta.t)
		}
	}

	z <- z - max(z)

	contour(a, b, exp(z),
		nlevels = 5,
		labcex = 0.5,
		main = paste("ratio = ", ratio[i]))
	abline(v = a.real,
		col = "blue")
	abline(h = b.real[i],
		col = "blue")
}









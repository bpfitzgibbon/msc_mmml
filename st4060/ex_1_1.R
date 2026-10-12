N = 1000
rate = 0.5
x = rexp(N, rate)
hist(x, freq=FALSE)
x_grid = seq(0, 15, length=N)
y_grid = dexp(x_grid, rate)
lines(x_grid, y_grid, lwd=4)
lines(density(x), col="red", lwd=4)
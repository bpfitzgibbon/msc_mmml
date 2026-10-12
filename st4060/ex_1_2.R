# (b)
attach(faithful)
plot(waiting, eruptions)
lm.out <- lm(eruptions ~ waiting)
lm.out$coefficients
abline(lm.out, lwd=2, col="red")
lm.out.summ = summary(lm.out)
r_adj <-lm.out.summ$adj.r.squared

# (c)
waiting_time = 80
predict(lm.out, data.frame(waiting=waiting_time), interval="confidence")
waiting_times = max(waiting) + c(1:5)
predict(lm.out, data.frame(waiting=waiting_times), interval="confidence")
plot(waiting, eruptions, xlim=c(40,110), ylim=c(1.5,7.5))
abline(lm.out, lwd=4, col="red")
points(waiting_times, predict(lm.out, data.frame(waiting=waiting_times)), col="red", pch=3)
detach(faithful)

# (d)
lm.out$coefficients
summary(lm.out)$coefficients[, 4] # p-values
par(mfrow=c(1,3))
plot(lm.out$residuals)
hist(lm.out$residuals, freq=FALSE)
lines(density(lm.out$residuals), lwd=4, col="red")
m = 0
s = sd(lm.out$residuals)
g = seq(-5,5,length=1000)
points(g, dnorm(g,m,s))
qqlm = qqnorm(lm.out$residuals)
abline(lm(qqlm$y ~ qqlm$x), lwd=4, col="red")

# (e)
mylm <- function(x, y){
  xm = x - mean(x)
  ym = y - mean(y)
  beta = sum(xm*ym)/sum(xm*xm)
  alpha = mean(y-beta*x)
  resid = y - alpha - beta*x
  return(list(alpha, beta, resid))
}
mylm(waiting, eruptions)
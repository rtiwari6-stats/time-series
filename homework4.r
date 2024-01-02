#homework 4

#3.6
library(astsa)
tsplot(varve)
n = length(varve)-1
#variance of first half
var(varve[1:316])
#variance of second half
var(varve[317:633])

#with logs
varve_log = log(varve)
#variance of first half
var(varve_log[1:316])
#variance of second half
var(varve_log[317:633])

#histograms
par(mfrow=c(1,2))
hist(varve)
hist(varve_log)

#3.6b
par(mfrow=c(1,1))
tsplot(varve_log)

#3.6c
acf(varve_log, main = "Sample ACF of log(varve)")

#3.6d
diff_log_varve = diff(varve_log)
tsplot(diff_log_varve)
acf(diff_log_varve)

#3.7
par(mfrow=c(2,2))
tsplot(gtemp, main="gtemp series")

#MA smoother
w = c(0.5, rep(1,9), 0.5)/10
f = filter(gtemp, sides=2, filter=w)
tsplot(gtemp,  main="MA smoother for gtemp series")
lines(f, lwd=2, col=4)

#kernel smoother
tsplot(gtemp,  main="Kernel smoother for gtemp series")
lines(ksmooth(time(gtemp), gtemp, "normal", bandwidth = 10), lwd=2, col=2)

#lowess smoother
tsplot(gtemp,  main="Lowess smoother for gtemp series with span = 2/3")
lines(lowess(gtemp), lwd=2, col=3)

#problem 2
library(astsa)
set.seed(90210)
w = rnorm(500 + 50) # 50 extra to avoid startup problems
x1 = filter(w, filter=c(1.5,-.75), method="recursive")[-(1:50)]
x2 = filter(w, filter=c(1/7,-1/9), method="recursive")[-(1:50)]
par(mfrow=c(1,2))
tsplot(x1, main="autoregression with original coefficients", col="blue")
tsplot(x2, main="autoregression with inverse of TAMU UIN coefficients",col="darkred")

#problem 2b
polyroot(c(1,-1.5,0.75))
polyroot(c(1,-7,9))
polyroot(c(1,-1/7,1/9))
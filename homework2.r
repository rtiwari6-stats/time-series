#2.3 acf plot
x = seq(from=1, to=10, by=1)
y=c(1,4/6,1/6, rep(0,7))
plot(x,y, main="ACF plot", ylab=expression(rho(h)), xlab = "h", ylim = c(0.1,1), xlim = c(1, 5))
lines(c(x[1], x[1]), c(y[1], 0))
lines(c(x[2], x[2]), c(y[2], 0))
lines(c(x[3], x[3]), c(y[3], 0))

#problem 3
library(astsa)
set.seed(90210)
w = rnorm(500 + 50) # 50 extra to avoid startup problems
x1 = filter(w, filter=c(1.5,-.75), method="recursive")[-(1:50)]
x2 = filter(w, filter=c(1,-4), method="recursive")[-(1:50)]
par(mfrow=c(1,2))
tsplot(x1, main="autoregression with original coefficients", col="blue")
tsplot(x2, main="autoregression with TAMU UIN coefficients",col="darkred")

#trial
w = rnorm(500 + 50) # 50 extra to avoid startup problems

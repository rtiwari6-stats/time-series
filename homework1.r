#homework 1
setwd("~/tamu/MS-STAT-2022/626/2023/Homework1")
install.packages("astsa")
library(astsa)
#1,1a

set.seed(90210)
w = rnorm(100 + 50)
x = filter(w, filter = c(0, -0.9), method = "recursive")[-(1:50)] #autoregression
v = filter(x, sides=1, filter = rep(1/4,4)) #MA filter
tsplot(x, main="1.1a) autoregression with moving average", col=4)
lines(v, col="darkred", lty=2)

#1.1b
set.seed(90210)
t = 1:100
cs = 2 * cos(2 * pi * t / 4)
w = rnorm(100)
x = cs + w
v = filter(x, sides=1, filter = rep(1/4,4)) #MA filter
tsplot(x, main="1.1b) Sinusoidal Signal with moving average", col=4)
lines(v, col="darkred", lty=2)

#1.1c
set.seed(90210)
x = log(jj)
v = filter(x, sides=1, filter = rep(1/4,4)) #MA filter
tsplot(x, main="1.1c) log with moving average", type="o",  col=4)
lines(v, col="darkred", lty=2)

#1.3a
set.seed(90210)
par(mfrow=c(3,3))
for(i in 1:9){
  w = rnorm(500)
  x = cumsum(w)
  tsplot(x, col=4)
  abline(h=0, lty=2)
}

#1.3b
set.seed(90210)
par(mfrow=c(3,3))
for(i in 1:9){
  w = rnorm(500)
  v = filter(w, sides=2, filter=rep(1/3,3))
  tsplot(v, col=4)
}

#1.3a (with drift)
set.seed(90210)
par(mfrow=c(3,3))
for(i in 1:9){
  w = rnorm(500) + 1.5
  x = cumsum(w)
  tsplot(x, col=4)
  abline(h=0, lty=2)
}

#1.3b (with drift)
set.seed(90210)
par(mfrow=c(3,3))
for(i in 1:9){
  w = rnorm(500) + 1.5
  x = cumsum(w)
  v = filter(x, sides=2, filter=rep(1/3,3))
  tsplot(v, col=4)
}

#1.4a
par(mfrow=c(1,1))
tsplot(gdp)

#1.4b
tsplot(diff(log(gdp)), type = "o", col = 6, ylab = "GDP Growth")
points(diff(gdp)/lag(gdp,-1), pch = 11, col = 4)

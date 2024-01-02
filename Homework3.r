#homework3
library(astsa)

#2.11a
w = rnorm(500)
acf1(w, 20)

#2.11b
w = rnorm(50)
acf1(w, 20)

#2.12a
w = rnorm(500) # 500 N(0,1) variates
v = filter(w, filter=rep(1/3,3)) # moving average
acf1(v, 20)

#2.12b
w = rnorm(50) # 50 N(0,1) variates
v = filter(w, filter=rep(1/3,3)) # moving average
acf1(v, 20)

#2.13
set.seed(90210)
w = rnorm(500 + 50) # 50 extra to avoid startup problems
x = filter(w, filter=c(1.5,-.75), method="recursive")[-(1:50)]
#tsplot(x, main="autoregression", col=4)
acf1(x, 50)

#2.14
set.seed(90210)
t = 1:500
cs = 2*cos(2*pi*(t+15)/50) # signal
w1 = rnorm(500, mean=0, sd=0) # noise
w2 = rnorm(500, mean=0, sd=1)
w3 = rnorm(500, mean=0, sd=5)
par(mfrow=c(3,1))
acf1(cs+w1, 100)
acf1(cs+w2, 100)
acf1(cs+w3, 100)

#problem2a
par(mfrow=c(1,1))
x = rep(0,100)
y = rep(0,100)
set.seed(90210)
for(i in 2:100){
  x[i] = x[i-1] + rnorm(1)
  y[i] = y[i-1] + rnorm(1)
}
plot(x,y, main = "x versus y")

summary(lm(y~x))

#problem2b
count = 0
for(n in 1:1000){
  x = rep(0,100)
  y = rep(0,100)
  set.seed(2 * n)
  for(i in 2:100){
    x[i] = x[i-1] + rnorm(1)
    y[i] = y[i-1] + rnorm(1)
  }
  m = lm(y~x)
  pval = summary(m)$coefficients[2,4]
  if(pval < 0.05){
    count = count+1
  }
}
count

#problem3.1
t=1:100
x = 0.5 + 0.7*t + rnorm(100)
shapiro.test(x)
acf1(x)


#simulate MC 1
errors = rnorm(1000)
time = seq(from=1, to=1000, by=1)
beta0 = 5
beta1 = 7
x = beta0 + beta1*time + errors
qqnorm(x)
qqline(x)

#simulate MC 1
library(MASS)
errors = mvrnorm(1000, mu=0, Sigma=1)
time = seq(from=1, to=1000, by=1)
beta0 = 5
beta1 = 7
x = beta0 + beta1*time + errors
qqnorm(x)
qqline(x)

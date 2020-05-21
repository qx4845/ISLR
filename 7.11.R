### ISLR Chapter 7, Question 11

n = 100
nloops = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = x1 + 2*x2 + 3*x1^2 - 0.5*2^3

NaNs = rep(0, 2*nloops)

betas = array(NaNs, c(2, nloops))
betas[1, 1] = 22
beta0s = array(NaNs, c(2, nloops))

for(i in 1:nloops){
  a = y - betas[1,i] * x1
  betas[2,i] = lm(a~x2)$coef[2]
  beta0s[1,i] = lm(a~x2)$coef[1]
  
  a = y - betas[2,i] * x2
  betas[1,i] = lm(a~x1)$coef[2]
  beta0s[2,i] = lm(a~x1)$coef[1]
}

plot(1:nloops, beta0s[1,], col="black", lty=2)

points(1:nloops, betas[2,], col="red", pch="*")
lines(1:nloops, betas[2,], col="red",lty=2)

points(1:nloops, beta0s[1,], col="blue", pch="+")
lines(1:nloops, beta0s[1,], col="blue",lty=2)

points(1:nloops, beta0s[2,], col="green", pch=".")
lines(1:nloops, beta0s[2,], col="green",lty=2)

### plot not working for some reason. All other Ch7 problems seem redundant from my understanding of the chapter. 

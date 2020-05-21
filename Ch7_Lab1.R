### ISLR, Chapter 7: Lab 1

library(ISLR)
attach(Wage)

### Section 7.8.1 Polynomial Regression and Step Functions
fit=lm(wage~poly(age,4), data=Wage)
coef(summary(fit))

fit2=lm(wage~poly(age,4,raw=T), data=Wage)
coef(summary(fit2))

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

fit2b = lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)
summary(fit2b)

## Create a grid of values for age at which we want predictions, and then we call the generic predict()
## function, specifying that we want standard errors as well.
agelims=range(age)
age.grid = seq(from=agelims[1], to=agelims[2])
preds = predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands=cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

## Plot the data and add the fit from the degree-4 polynomial, as in Figure 7.1 (LHS):
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, 1), oma=c(0, 0, 4, 0))
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

## Illustrating that regardless of whether the basis functions procuded by poly() are orthogonal, 
## the model obtained will not be affected in any meaningful way. Obtained fitted values are identical. - ISLR pg289
preds2 = predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit))

### Hypothesis Testing

## pg 290
fit.1 = lm(wage~age, data=Wage)
fit.2 = lm(wage~poly(age,2), data=Wage)
fit.3 = lm(wage~poly(age,3), data=Wage)
fit.4 = lm(wage~poly(age,4), data=Wage)
fit.5 = lm(wage~poly(age,5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))
### Illustration that the square of the t-stastics are equal to the F-statistics from the anova() function:
(-11.980)^2

fit.1 = lm(wage~education+age, data=Wage)
fit.2 = lm(wage~education+poly(age,2), data=Wage)
fit.3 = lm(wage~education+poly(age,3), data=Wage)
anova(fit.1, fit.2, fit.3)

### Alternatively to Hypothesis Testing, we could also choose the polynomial degree using cross-validation from Ch5.

### Does an individual earn > $250,000 / year?

fit = glm(I(wage>250)~poly(age,4), data=Wage, family=binomial)
preds = predict(fit, newdata=list(age=age.grid), se=T)
pfit = exp(preds$fit) / (1 + exp(preds$fit))
## Estimate 95% confidence interval using +/- (2 * standard_error, per Ch7)
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit) / (1 + exp(se.bands.logit))
## N.B. We could have directly computed probabilities by using type="reponse" optinon in the predict() function.
preds = predict(fit, newdata=list(age=age.grid), type="response")
## But the corresponding confidence intervals would not have been sensible, b/c would get negative probabilities!!!

## Finally plot RHS of Figure 7.1
#plot(age, I(wage>250), xlim=agelims, ylim=c(0, 0.2))
plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0, 0.2))
points(jitter(age), I((wage>250) / 5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

## Create a step-function using cut() # Can also specify custom break points using "breaks" option in cut() function
table(cut(age,4))
fit=lm(wage~cut(age,4), data=Wage)
coef(summary(fit))

### Section 7.8.2: Splines
library(splines)
fit=lm(wage~bs(age, knots=c(25, 40, 60)), data=Wage)
pred=predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")

dim(bs(age, knots=c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

fit2=lm(wage~ns(age, df=4), data=Wage)
pred2 = predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

## Code that produced Figure 7.8 in ISLR
plot(age, wage, xlim=agelims, cex=0.5, dol="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age, wage, df=16)
fit2 = smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

## Local Regression using loess() function
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Local Regression")
fit=loess(wage~age, span=0.2, data=Wage)
fit2=loess(wage~age, span=0.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

### Section 7.8.3: GAMs
## GAM using natural splines
gam1=lm(wage~ns(year,4)+ns(age, 5)+education, data=Wage)

## We now fit model 7.16 using smoothing splines
## We require the "gam" library when using GAMs with components that cannot be espressed in terms of basis functions.
library(gam)
gam.m3 = gam(wage~s(year,4)+s(age,5)+education, data=Wage)
## Produce Figure 7.12: plot() automatically recognizes type gam and uses plot.gam()
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
## Figure 7.11: plot.gam() even works of class 'lm' in addition to class 'gam'.
#plot.gam(gam1, se=TRUE, col="red")
## ISLR book uses old version of R software (I am currently using RStudio version 1.2.5019, which uses Rv4.x).
## Instead of plot.gam() use plot.Gam()
plot.Gam(gam1, se=TRUE, col="red")

gam.m1 = gam(wage~s(age,5)+education, data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")
## Summary of GAM Fit:
summary(gam.m3)

## Make predictions on the training set
preds=predict(gam.m2, newdata=Wage)
## Use local regression fits as building blocks in a GAM using the "lo()" function
gam.lo=gam(wage~s(year, df=4)+lo(age, span=0.7)+education, data=Wage)

## Use "lo()" function to create interactions before calling the "gam()" function
gam.lo.i = gam(wage~lo(year, age, span=0.5)+education, data=Wage)
## Get some errors, but so do online people: https://rpubs.com/leechau/392774
#  install.packages("akima",repos="http://cran.r-project.org",depend=T)
## Plot the local interaction surface between year and age:
par(mfrow=c(1,2))
library(akima)
plot(gam.lo.i)

## Fit a logistic regression GAM using I() function and family=binomial option:
gam.lr = gam(I(wage>250)~year+s(age, df=5)+education, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")

## It is easy to see that there are no high earners in the <HS category:
table(education, I(wage>250))

## So we instead fit a logistic regression GAM using all but this (<HS) category:
gam.lr.s = gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green")

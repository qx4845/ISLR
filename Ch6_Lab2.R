## Section 6.6.1: Ridge Regression

library(ISLR)
#library(glmnet)
Hitters = na.omit(Hitters)

x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

library(glmnet)
grid = 10^seq(10, -2, length=100)               ## => Lambda from 10^10 to 10^-2
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)  ## alpha=0: Ridge Regression, alpha=1: Lasso

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))             ## L2 norm

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))             ## L2 norm

predict(ridge.mod, s=50, type="coefficients")[1:20,]      ## Predict coefficients for lambda = 50 not in original grid.

## pg. 253
set.seed(2)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

## If we had insteas simply fit a model with just an intercept, we sould have predicted each test observation
## using the mean of the training observations like this:
mean((mean(y[train])-y.test)^2)

## We can get the same result using a huge value of lambda such as 1e10:
ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

## Now check MSE using lambda=0 which corresponds to least-squares fitting
### Error: used coef.glmnet() or predict.glmnet() with `exact=TRUE` so must in addition supply original argument(s)  x and y  in order to safely rerun glmnet
### happns when I use s=0 below, so I changed to s=0.1, as explained here: https://rpubs.com/leechau/isl-ch6-lab
#ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
ridge.pred = predict(ridge.mod, s=0.01, newx=x[test,], exact=T)
mean((ridge.pred-y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod, s=0.01, exact=T, type="coefficients")[1:20,]

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

rige.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
#rige.pred=predict(ridge.mod, s=212, newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]

## Section 6.6.2: The Lasso

lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

### from rpubs.com/leechau/isl-ch6-lab
lbl = cv.out$lambda.min
#lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
lasso.pred=predict(lasso.mod, s=lbl, newx=x[test,])
mean((lasso.pred-y.test)^2)

#out=glmnet(x,y,alpha=1, lambda=grid)
out <- glmnet(x,y,alpha=1, lambda=lbl)

#lasso.coef = predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.coef = predict(out, type="coefficients", s=lbl)
lasso.coef
### For some reason gives completely wrong numbers, and some coefficients not 0 like they should be in book. 
### I have no idea WTF I did wrong. I did everything in the book to the letter.... B/C i'm using R4.x?


#### ISLR Section 6.5.1: Best Subset Selection

library(leaps)
library(ISLR)
Hitters = na.omit(Hitters)


regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type='l')
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col="red", cex=2, pch=20)

## 6.5.2:             Forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

## 6.5.3
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test=(!train)
regfit.best = regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat = model.matrix(Salary~., data=Hitters[test,])
val.errors=rep(NA, 19)
for(i in 1:19){
  ## Extract the coefficients from regfit.best for the best model of each size "i",
  coefi = coef(regfit.best, id=i)
  ## multiply them into the appropriate columns of the test model matrix to form the predictions,
  pred=test.mat[,names(coefi)]%*%coefi
  ## and compute the test MSE.
  val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}
## not giving same answers as the book for some reason
set.seed(1)
nrow(Hitters)

## again same problem
train=sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~., data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
  }
val.errors

predict.regsubsets = function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars=names(coefi)
  mat[, xvars]%*%coefi
}


###

k=10
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~., data=Hitters[folds != j,], nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit, Hitters[folds == j,], id=i)
    cv.errors[j,i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors = apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

reg.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 11)


## a.) 
model.length = 100
set.seed(1)
x = rnorm(model.length)
y=x-2*x^2+rnorm(model.length)

## b.) 
plot(x,y)

## c.)
theData = data.frame(x,y)
model.length = 100
N_order = 4
NaNs = rep(NaN, N_order * model.length)
err = array(NaNs, c(N_order, model.length))
set.seed(1)
for(order in 1:N_order){
  for(i in 1:model.length){
    glm.LOOCV = glm(y~poly(x,order), data=theData[-i])
    glm.pred = predict(glm.LOOCV, theData)
    err[order, i] = glm.pred[i] - y[i]
  }
}

LOOCV_test_error_estimates = rep(NaN, N_order)
LOOCV_test_error_estimates = array(LOOCV_test_error_estimates, c(1,4))
for(order in 1:N_order){
  LOOCV_test_error_estimates[1, order] = sum(err[order,]) / model.length
}

LOOCV_test_error_estimates
plot(c(1:4), LOOCV_test_error_estimates, xlab="Polynomial Order", ylab="LOOCV Test Error Est.", type = "l", col=2)

## d.)
theData = data.frame(x,y)
model.length = 100
N_order = 4
NaNs = rep(NaN, N_order * model.length)
err = array(NaNs, c(N_order, model.length))


set.seed(7)


for(order in 1:N_order){
  for(i in 1:model.length){
    glm.LOOCV = glm(y~poly(x,order), data=theData[-i])
    glm.pred = predict(glm.LOOCV, theData)
    err[order, i] = glm.pred[i] - y[i]
  }
}

LOOCV_test_error_estimates = rep(NaN, N_order)
LOOCV_test_error_estimates = array(LOOCV_test_error_estimates, c(1,4))
for(order in 1:N_order){
  LOOCV_test_error_estimates[1, order] = sum(err[order,]) / model.length
}

LOOCV_test_error_estimates
plot(c(1:4), LOOCV_test_error_estimates, xlab="Polynomial Order", ylab="LOOCV Test Error Est.", type = "l", col=2)
title("5.8d.) Different Seed")



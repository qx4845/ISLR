
## a.)
glm.fits=glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)

## b.)
#B_subset = Weekly[2:dim(Weekly)[1],]
#glm.fits2 = glm(Direction~Lag1+Lag2, data=B_subset, family=binomial)
glm.fits2 = glm(Direction~Lag1+Lag2, data=Weekly[2:dim(Weekly)[1],], family=binomial)

## c.) 
glm.probs = predict(glm.fits2, Weekly)[1]

## d.) 
n = dim(Weekly)[1]
err = rep(1, n)
for(i in 1:n){
  ### i.)
  glm.LOO = glm(Direction~Lag1+Lag2, data= Weekly[-i,], family=binomial)
  set.seed(1)
  
  ### ii.)
  glm.probs = predict(glm.LOO, Weekly)[i]
  
  ### iii.)
  glm.pred = "Down"
  if(glm.probs > 0.5){
    glm.pred = "Up"
  }
  
  ### iv.) 
  if(glm.pred == Weekly[i,9]){
    err[i] = 0
  }
}

## e.) 
LOOCV_test_error_estimate = sum(err) / n


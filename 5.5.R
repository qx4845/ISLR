#### ISLR Chapter 5 Question #5

LoadStuff()

## a.)
glm.fit = glm(default~income+balance, data=Default, family=binomial)

## b_i.)
train = 1:8000
#train.X = Default[train,3:4]
#test.X = Default[-train,3:4]
#
#train.Y = Default[train,1]
test.Y = Default[-train,1]

train.data = Default[train,]
test.data = Default[-train,]

## b_ii.)
glm.fit2 = glm(default~income+balance, data=train.data, family=binomial)
glm.probs = predict(glm.fit2, Default)[-train]
glm.pred = rep("No", 2000)
glm.pred[glm.probs > 0.5]="Yes"
table(glm.pred, test.Y)


## c.) 

N_it = 4 # number of iterations
emptyData = rep(NaN, 2*2*N_it)
tables = array(emptyData, c(2, 2, N_it))

emptyData2 = rep(NaN, 1*N_it)
validationErrors = array(emptyData2, c(1,N_it))

for(i in 1:N_it){
  
  train = 1:(10000-1000*i)
  test.Y3 = Default[-train,1]
  
#  train.data3 = Default[train]
#  test.data3 = Default[-train,]
  
  glm.fit3 = glm(default~income+balance, data=Default[train,], family=binomial)
  
  set.seed(2)
  glm.probs3 = predict(glm.fit3, Default)[-train]
  glm.pred3 = rep("No", 1000*i)
  glm.pred3[glm.probs3 > 0.5]="Yes"
  print(cat("Validation set size = ", 1000*i, " "))
  tables[,,i] = table(glm.pred3, test.Y3)
  validationErrors[i] = (tables[2,1,i] + tables[1,2,i]) / sum(tables[,,i])
}

tables
validationErrors

## d.) 

N_it = 4 # number of iterations
emptyData = rep(NaN, 2*2*N_it)
tables4 = array(emptyData, c(2, 2, N_it))

emptyData2 = rep(NaN, 1*N_it)
validationErrors4 = array(emptyData2, c(1,N_it))

for(i in 1:N_it){
  
  train = 1:(10000-1000*i)
  test.Y4 = Default[-train,1]
  
  #  train.data3 = Default[train]
  #  test.data3 = Default[-train,]
  
  glm.fit4 = glm(default~., data=Default[train,], family=binomial)
  
  set.seed(1)
  glm.probs4 = predict(glm.fit4, Default)[-train]
  glm.pred4 = rep("No", 1000*i)
  glm.pred4[glm.probs4 > 0.5]="Yes"
  print(cat("Validation set size = ", 1000*i, " "))
  tables4[,,i] = table(glm.pred4, test.Y4)
  validationErrors4[i] = (tables[2,1,i] + tables[1,2,i]) / sum(tables[,,i])
}

tables4
validationErrors4

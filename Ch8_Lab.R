### ISLR Chapter 8 Lab

library(tree)
library(ISLR)
attach(Carseats)

## Section 8.3.1
High = ifelse(Sales <= 8, "No", "Yes")

Carseats = data.frame(Carseats, High)

tree.carseats.training = tree(High~.-Sales, Carseats)
summary(tree.carseats.training)
plot(tree.carseats.training)
text(tree.carseats.training, pretty=0)

## Regression (Decision) Trees: test set prediction from training set model
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats, subset=train)
tree.pred.training = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred.training, High.test)
##(86+57)/200
#t0 = (104 + 50) / 200
t0 = (table(tree.pred.training, High.test)[1,1] + table(tree.pred.training, High.test)[2,2])/ 200

## Pruning Regression Trees
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
## N.B. Here 'dev' corresponds to the cross-validation error rate.
minSizeLoc = which(cv.carseats$dev == min(cv.carseats$dev))
minSize = cv.carseats$size[minSizeLoc]

## Plot cross-validation error rate vs "size" and "k" for pre-pruned tree:
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

##and plot cross-validation error rate vs "size" and "k" for pruned tree:
## Prune the tree:
#prune.carseats = prune.misclass(tree.carseats, best=9)
prune.carseats = prune.misclass(tree.carseats, best=minSize)

plot(prune.carseats)
text(prune.carseats, pretty=0)

## How well does this pruned tree perform on the test data set?
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
##(94+60)/200
#t1 = (97+58)/200
t1 = (table(tree.pred, High.test)[1,1] + table(tree.pred, High.test)[2,2])/ 200

ifelse(t1 > t0, print("Pruned tree is better"), print("Unpruned tree is better"))

## incereasing the value of "best" leads to larger pruned tree with lower classification accuracy:
prune.carseats.largetree = prune.misclass(tree.carseats, best=15)
plot(prune.carseats.largetree)
text(prune.carseats.largetree, pretty=0)
tree.pred2 = predict(prune.carseats.largetree, Carseats.test, type="class")
table(tree.pred2, High.test)
t2 = (table(tree.pred2, High.test)[1,1] + table(tree.pred2, High.test)[2,2])/ 200

ifelse(t2 > t1, print("Larger, less-pruned tree is better"), print("Smaller, more-pruned tree is better"))

### Section 8.3.2: Fitting Regression Trees
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
## Grow the tree
tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)
## N.B. In the context of regression trees the deviance is the sum of squared errors for the tree.

## Plot the tree
plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")

## Prune the tree
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

## In keeping with the cross-validation results,
## we use the unpruned tree to make predictions on the test set:
yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
regTreeMSE = mean((yhat-boston.test)^2)
regTreeMSE
MedianHomeValueErrorDollars = 1000 * sqrt(regTreeMSE)
## Therefore this model leads to test predictions that are within around "MedianHomeValueErrorDollars"
## of the true median home value for the Boston suburb.

## Section 8.3.3: Bagging and Random Forests
## Bagging = randomForest with m = p
library(randomForest)
set.seed(1)
p = length(names(Boston))-1
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
#bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=p, importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
bagTestMSE = mean((yhat.bag-boston.test)^2)
bagTestMSE

ifelse(bagTestMSE < regTreeMSE, print("Bagging is better than pruning here."), print("Pruning is better than bagging here."))

## Change number of bagging trees grown with "ntree" arguement to randomForest():
bag.boston2 = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
#bag.boston2 = randomForest(medv~., data=Boston, subset=train, mtry=p, ntree=25)
yhat.bag2 = predict(bag.boston, newdata=Boston[-train,])
bag2MSE = mean((yhat.bag2-boston.test)^2)
bag2MSE

## Grow a random forest (m=6, m!=p)
set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
rfMSE = mean((yhat.rf-boston.test)^2)
rfMSE

ifelse(rfMSE < min(bag2MSE, bagTestMSE), print("Random forests are better than bagging here."), print("Bagging is better than random forests here."))

## Using the importance() function, we can view the importance of each variable:
importance(rf.boston)
## plot measures of variable importance:
varImpPlot(rf.boston)
## The results indicate that across all considered trees, the wealth level (lstat) and house size (rm)
## are by far the two most important variables. 

## Section 8.3.4: Boosting
library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.boston)

## Produce "partial dependence plots" for 'rm' and 'lstat', the two most influential variables:
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

## Use the boosted model to predict 'medv' on the test set:
yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
boostMSE = mean((yhat.boost-boston.test)^2)
boostMSE

ifelse(boostMSE < rfMSE, print("Boosting is better than random forests here."), print("Random forests are better than boosting here."))
ifelse(boostMSE < min(bag2MSE, bagTestMSE), print("Boosting is better than bagging here."), print("Bagging is better than boosting here."))

## Perform boosting with custom value of lambda in Eq.8.10 using the "shrinkage" option for gmb().
## (default lambda = 0.001 (absent any "shrinkage" option))
boost.boston2 = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost2 = predict(boost.boston2, newdata=Boston[-train,], n.trees=5000)
boostMSE2 = mean((yhat.boost2-boston.test)^2)
boostMSE2

ifelse(boostMSE2 < boostMSE, print("Boosting is better with lambda = 0.2 here (compared to 0.001)."), print("Boosting is better with lambda = 0.001 here (compared to 0.2)."))



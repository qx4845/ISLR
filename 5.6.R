LoadStuff()
set.seed(7)

## a.)
glm.fit = glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)

## b.) 
boot.fn=function(data,index)
  coefficients(glm(default~income+balance, data=data, family=binomial, subset=index))

## c.)
set.seed(1)
boot(Default, boot.fn, 1000)


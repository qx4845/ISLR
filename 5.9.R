attach(Boston)

## a.)
mu_bar = mean(medv)

## b.) Compute the Standard Error (SE) in mu:
SE_mu = sd(medv) / sqrt(dim(Boston[1])[1])

## c.) Estimate the standard error the mean:
### This estimates the standard error of the mean (https://stackoverflow.com/questions/18341569/r-calculate-the-standard-error-using-bootstrap)
meanFunc <- function(data, index){mean(data[index])}
bootMean <- boot(Boston[,"medv"], meanFunc, 1000)
bootMean

## d.)
conf_int_low = 22.53281 - 2*0.4142283
conf_int_high = 22.53281 + 2*0.4142283
print(paste("Bootstrap 95% confidence interval: [", conf_int_low, ", ", conf_int_high, "]"))

## e.) Extract median from data
mu_bar = mean(medv)
min_idx = NaN
dist = 1000
for(i in 1:dim(Boston[1])[1]){
  if(abs(Boston[i, "medv"] - mu_bar) < dist){
    min_idx = i
    dist = abs(Boston[i, "medv"] - mu_bar)
  }
}

min_idx
dist
Boston[min_idx, "medv"]
mu_bar
mu_diff = abs(Boston[min_idx, "medv"] - mu_bar)
mu_diff

####### Alternatively, you can just do this:
median_home_value = median(Boston[,"medv"])
median_home_value

## f.) Estimate standard error of the median using the bootstrap
set.seed(18)
medianFunc = function(data, index){median(data[index])}
bootMedian = boot(Boston[,"medv"], medianFunc, 1000)
bootMedian

## g.) provide an estimate of the 10th percentile of medv in Boston suburbs using the quantile() 

#glm.fit_g = glm(medv~dis+rad, data=Boston)
#glm.pred = predict(glm.fit_g, Boston, type="response") 

mu_0.1 = quantile(Boston[,"medv"], probs = seq(0, 1, by= 0.1))[2]

## h.) Use bootstrap to estimate the standard error in mu_0.1
set.seed(21)
percentileFunc = function(data, index){quantile(data[index], probs = seq(0, 1, by= 0.1))[2]}
bootPercentile = boot(Boston[,"medv"], percentileFunc, 1000)
bootPercentile

 
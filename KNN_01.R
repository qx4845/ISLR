x1=c(0, 2, 0, 0 ,-1, 1)
x2=c(3, 0, 1, 1, 0, 1)
x3=c(0,0,3,2,1,1)

Y=c("Red", "Red", "Red", "Green", "Green", "Red")
#Y = as.factor(Y)

d = vector(length=length(x1))

K = 3
NN = vector(length=K) ## Contains indices of K nearest 
## neighbors (increasing Euclidian distance)

for(i in 1:length(x1))
  d[i] = sqrt(x1[i]^2 + x2[i]^2 + x3[i]^2)
end

for(l in 1:K)
#  sort(d)[1:K]
  NN[l] = which(d == sort(d)[l])
end

r = 0 # number of Red nearest neighbors
g = 0 # number of Green nearest neighbors
for(m in 1:K)
  
  if ( Y[NN[m]] == "Red" )
  {
    r = r + 1
  } else if (Y[NN[m]] == "Green")
  {
    g = g + 1
  }

  
  if ( r>g )
  {
    color = "Red"
  } else
  {
    color = "Green"
  }
  
end


print(c("The color of a point at \n(x1,x2,x3) = (0,0,0) is ", color))





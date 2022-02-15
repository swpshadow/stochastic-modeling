

################## 3 ##################

ci<- function(X){
  X = sort(X)
  print(X[round(0.05 * length(X))])
  print(X[length(X)- round(0.05 * length(X))])
}


f <- function(x, mu){
  return(1 / ( 4 * sqrt(2 * pi))  * exp(-mu - (1/8) *(x-mu)^2 ) )
}

y = runif(100000, 0, 1)
x = runif(100000, -8, 12)
x.result = c()
y.result = c()
count = 0
itx = 1
while( count < 1000 & itx < length(x)){
  if(y[itx] < f(x[itx], 0)){
    x.result <- c(x.result, x[itx])
    y.result <- c(y.result, y[itx])
    count <- count + 1
  }
  itx <- itx + 1
}


plot(x.result,y.result)
# install.packages("modeest")
library(modeest)
mlv(x.result)
mean(x.result)
ci(x.result)


################## 3 ##################
#this is the alternate version of 3 where I assume mu = 2

y = runif(100000, 0, 0.05)
x = runif(100000, -8, 12)
x.result = c()
y.result = c()
count = 0
itx = 1
while( count < 1000 & itx < length(x)){
  if(y[itx] < f(x[itx], 2)){
    x.result <- c(x.result, x[itx])
    y.result <- c(y.result, y[itx])
    count <- count + 1
  }
  itx <- itx + 1
}



plot(x.result,y.result)
# install.packages("modeest")
# library(modeest)
mlv(x.result)

mean(x.result)
ci(x.result)




################## 4 ##################

g <- function(x){return(1 / sqrt(2 *pi) * exp(- 0.5 *(x-1)^2))}
G <- integrate(g, lower = 0, upper=5)
c <- 1 / G$value
c

new.g <- function(x){return(c * g(x))}


#4b
mean.g <- function(x){return(x * new.g(x) )}
mu<- integrate(mean.g, lower=0, upper=5)$value
mu

var.g <-function(x){return(x^2 * new.g(x) )}
sigma <- integrate(var.g, lower=0, upper=5)$value - mu^2
sigma




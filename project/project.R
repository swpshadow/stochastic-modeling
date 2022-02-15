# setwd("/Users/shadow/OneDrive - University of Tulsa/classes/stochastic modeling/project")
data<-readRDS("Project.Mixture.Data")


b_1 <-function(x, start = 0, end = 1){
  result = c()
  for(l in x){
    if ( l < 0 | l > 1){result <- c(result,0)}
    else{result <- c(result,1)}
    
  }
  return(result)
}
b_2 <-function(x, start = 0, end = 2){
  result = c()
  for(l in x){
    if ( l < 0 | l > 2){result <- c(result,0)}
    else if(l < 2){result <- c(result, x)}
    else{result <- c(result, -x + 2)}
  }
  return(result)
}

b_3 <- function(x, start = 0, end = 3){
  result = c()
  for(l in x){
    if (l <= 0 | l >= 3){return(0)}
    else if (l < 1){result = c(result, (l - start)^2 / 2) }
    else if (l < 2){result = c( -(l-(end-start)/2 )^2 + 3/4)}
    else if (l < 3) {result = c((l-end)^2 / 2)}
    
  }
  return(result)
}


alpha <- function(x, i, j, h, n, basis=b_3){
  a = 0
  for (k in seq(length(x[1,]))){
    a = a + b_3(x[1, k]/h - i, 0, 3) * b_3(x[2, k]/h - j, 0, 3)
  }
  return(a / n)
}

bee.spline <- function(dat, bin.width, x.min=0, x.max=1, y.min=0, y.max=1, basis = b_3){
  result = c()
  for(k in seq(length(dat[1,]))){
    total = 0
    for(i in seq(x.min, x.max, bin.width)){
      for(j in seq(y.min, y.max, bin.width)){
        
      if(dat[1,k]/bin.width - i > 0 & dat[1,k]/bin.width - i < 3 &  dat[2,k]/bin.width - j < 3 & dat[2,k]/bin.width - j > 0){
        a = alpha(dat, i, j, bin.width, length(dat[1,]), basis)
        total <- total + a * basis(dat[1,k]/bin.width - i, 0, 3) * basis(dat[2,k]/bin.width - j, 0, 3) / bin.width^2
      }
      }
    }
    result <- c(result, total)
  }
  return(result)
}

dat = data
h = 0.5
res = bee.spline(dat, h, min(dat[1,]) - 2,  max(dat[1,]) + 2,  min(dat[2,]) - 2,  max(dat[2,]) + 2, basis = b_3)

plot(dat[1,], res, xlab = "x", ylab = "z", main = "Density estimation")
plot(dat[2,], res, xlab = "y", ylab = "z", main = "Density estimation")

# install.packages("scatterplot3d")
# library("scatterplot3d")
scatterplot3d(x=dat[1,], y=dat[2,], z=res, angle=55, xlab="x", ylab="y", grid = TRUE, zlab = "Density estimation")







################################################## Part 2 ##################################################







#summation function to help with inf and nan
cust.sum <- function(x){
  result = c()
  for(i in seq(length(x[1,]))){
    result = c(result, sum(x[,i], na.rm = TRUE ))
  }
  return(result)
}

#a helper function to normalize the pis afterwards. For some reason I needed to do this
normalize <- function(x){
  result <- rep(0, length(x))
  total = sum(x)
  for(i in seq(length(x))){
    result[i] = x[i] / total
  }
  return(result)
}

graphing <- function(means, sigmas, pis){
  #generate points for the density plot
  nsize=10000
  s = table( sample(seq(length(pis)), size= nsize, replace=TRUE, prob=pis) )
  for (i in seq(length(means[,1]))  ) {
    if (i==1){
      results = rmvnorm(s[names(s)==i], means[i,], diag(sigmas[i,]) ) 
    }
    else{
    results = rbind(results, rmvnorm(s[names(s)==i], means[i,], diag(sigmas[i,]) ) )
    }
  }
  d <- kde2d(results[,1], results[,2])
  persp(d, xlab="x", ylab="y", box=TRUE)

  
  # d = density(results)
  # plot(d, main="EM distribution estimate")
}

em <- function(X, input.mu, input.sigmas, input.pis, epsilon)
{ 
  
  #initialize the needed variables
  n = length(X[,1])
  m = length(input.mu[,1])
  pis = input.pis
  mu = input.mu
  sigmas = input.sigmas
  r = matrix(1, ncol=m, nrow=n)
  
  #make a backup stopping criteria
  count <- 2
  #make a list to hold all my likelihood estimates and set first position as something larger than 0 so doesn't break first iteration
  l <- rep(0, 500)
  l[count] <- 10
  
  #iterate while less than max iterations and likelihood is still changing
  while(count < 400 & (abs(l[count] - l[count-1]) > epsilon)) 
  {
    # EEEEEEEEEE
    #calculate the denominator for r first
    r.denom = rep(0, n)
    for( k in seq(1, n)){
      for (i in seq(1, m)){
        r.denom[k] <- r.denom[k] + pis[i] * dmvnorm(X[k,], mu[i,], diag(sigmas[i,]) )
      }
    }
    #calculate r using the denominator. Stored as a matrix so I can use all the values I want
    #would be more optimal to sum once instead of in each loop later but it still runs quickly
    for(i in seq(1, m)){
      # print(i)
      r[,i]<- pis[i] * dmvnorm(X, mu[i,], diag(sigmas[i,]) ) / r.denom
    }
    
    # MMMMMMMMMM
    #calculate pi, means, and sigma
    for (i in seq(1, m)){
      pis[i]<-sum(r[,i])/n
      mu[i,] <- cust.sum(as.vector(r[,i]) * X)/ sum(r[,i])
      sigmas[i,] <- cust.sum(r[,i] * (X-mu[i,])^2 )  / sum(r[,i])
    }
    pis = normalize(pis)
    #loop to calculate the likelihood using the likelihood function
    total.likelihood <- 0
    for (i in seq(1, m)){
      total.likelihood = total.likelihood + sum(r[,i] * (log(pis[i]) + log(dmvnorm(X, mu[i,], diag(sigmas[i,]) ) )))
    }
    l[count+1] <- total.likelihood
    count = count+1 #update number of iterations
  }
  #returns the means, sigma, and priors (weights)
  return( list(mu, sigmas, pis, total.likelihood) )
}

data<-readRDS("Project.Mixture.Data")

X=cbind(data[1,], data[2,])

input.mu = matrix( c(-7, 1, 6, 0, -1, 1, 1, 0),ncol = 2)
input.sigmas = matrix( c(15, 15, 15,15, 15, 15, 15, 30),ncol = 2)
input.pis = normalize(c(1,1,1,1))
epsilon = 1
res = em(X, input.mu, input.sigmas, input.pis, epsilon)
res

means = matrix(unlist(res[1]),ncol = 2)
sigmas = matrix(unlist(res[2]),ncol = 2)
pis = normalize(unlist(res[3]))
l = unlist(res[4])

library(rgl)
library(MASS)
graphing(means, sigmas, pis)


#already calculating the log likelyhood in the EM algorithm so I can just use that value
AIC <- function(num.params, likelyhood){
  return(2 * num.params - 2 * likelyhood)
}

BIC <- function(k, n, l){
  return(k * log(n) - 2 * l)
}
AIC(length(means) + length(sigmas) + length(pis), l )

BIC(length(means) + length(sigmas) + length(pis), length(X[,1]), l)

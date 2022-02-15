#summation function to help with inf and nan
cust.sum <- function(x){
  return(sum(x[is.finite(x)], na.rm = TRUE ))
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
  results = c()
  nsize=10000
  for (i in seq(length(means))  ) {
    results = append(results,rnorm(nsize * pis[i], means[i],sigmas[i]) )
  }
  #plot(normalize(results))#just a cool looking plot
  d = density(results)
  plot(d)
}

em <- function(data, input.mu, sigmas, input.pis, epsilon)
{
  #initialize the needed variables
  n = length(data)
  m = length(input.mu)
  pis = input.pis
  mu = input.mu
  variances = sigmas
  r = matrix(1, ncol=m, nrow=n)
  
  #make a backup stopping criteria
  count <- 2
  #make a list to hold all my likelihood estimates and set first position as something larger than 0 so doesn't break first iteration
  l <- rep(0, 200)
  l[count] <- 1
  
  
  #iterate while less than max iterations and likelihood is still changing
  while(count < 200 & (abs(l[count] - l[count-1]) > epsilon)) 
  {
  # EEEEEEEEEE
    #calculate the denominator for r first
    r.denom = rep(0, n)
    for( k in seq(1, n)){
      for (i in seq(1, m)){
        r.denom[k] <- r.denom[k] + pis[i] * dnorm(data[k],mu[i],sqrt(variances[i]) )
      }
    }
    #calculate r using the denominator. Stored as a matrix so I can use all the values I want
    #would be more optimal to sum once instead of in each loop later but it still runs quickly
    for(i in seq(1, m)){
      # print(i)
      r[,i]<- pis[i] * dnorm(data, mu[i], sqrt(variances[i]) ) / r.denom
    }
    
    # MMMMMMMMMM
    #calculate pi
    for (i in seq(1, m)){
      pis[i]<-cust.sum(r[,i])/n
    }
    #calculate means
    for(i in seq(1, m)){
      mu[i] <- cust.sum(r[,i] * data)/ cust.sum(r[,i])
    }
    #calculate variances
    for (i in seq(1, m)){
      variances[i] <- cust.sum(r[,i] * (data-mu[i])^2 )  / cust.sum(r[,i])
    }
    #loop to calculate the likelihood using the likelihood function
    total.likelihood <- 0
    for (i in seq(1, m)){
      total.likelihood = total.likelihood + cust.sum(r[,i] * (log(pis[i]) + log(dnorm(data,mu[i],sqrt(variances[i]) )) ))
    }
    l[count+1] <- total.likelihood
    count = count+1 #update number of iterations
  }
#retunrs the means, variances, and priors (weights)
return( list(mu, variances, pis) )
}



#part b example
graphing(c(1,7,-4), c(2, 2, 1), c(0.3, 0.3, 0.4))


#### ex1: creates a mixture model with three normal distributions. 
data = rnorm(50, -5, sqrt(3) )
data = append(data, rnorm(20, 3, sqrt(1) ))
data = append(data, rnorm(30, 8, sqrt(1) ))

mu= c(0, 1, 2) #initial means
sig = rep(1, 3)
x = em(data, mu, sig, normalize(mu + 1), 0.001)
x #print results in mean, variance, pi order
#unpack results
means = unlist(x[1])
sigmas = unlist(x[2])
pis = normalize(unlist(x[3]))

#graphs the results from the em algorithm
graphing(means, sigmas, pis)


##################  ex 2. ###################


data = rnorm(500, -10, 3 )
data = append(data, rnorm(200, 0,2 ))
data = append(data, rnorm(100, 5, 1 ))

mu= c(0, 1, 2) #initial means
sig = rep(1, 3)
x = em(data, mu, sig, normalize(mu + 1), 0.001)
x #print results in mean, variance, pi order
#unpack results
means = unlist(x[1])
sigmas = unlist(x[2])
pis = normalize(unlist(x[3]))

graphing(means, sigmas, pis)


#not relevant but graphed this too for kicks
# graphing2 = function(means, variances){
#   print(means)
#   print(variances)
#   plot.new()
#   plot.window(xlim= c(min(means-3*sqrt(variances) ), max(means+ 3* sqrt(variances))), ylim=c(0,1))
#   axis(1)
#   axis(2)
#   title(main ="Mixtrue Distributions")
#   box()
#   for (i in seq(1, length(means) )){
#     std = sqrt(variances[i])
#     x = seq(means[i] - 3* std, means[i] + 3 * std, by=0.1)
#     y = dnorm(x, means[i],std)
#     points(x,y)
#   }
# }
# 
# graphing2(means, sigmas)

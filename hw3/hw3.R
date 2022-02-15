gen.sample = function(func, n, tau, lambda){
  x = runif(n * 10, 0,10)
  y = runif(n* 10, 0, 2) #4.25 is max of the function
  count = 0
  i = 1
  results.y = c()
  results.x = c()
  while (i < n * 10 & count < n) {
    if( y[i] < func(lambda, x[i], tau) ){
      results.y = append(results.y, y[i])
      results.x = append(results.x, x[i])
      count = count + 1
    }
    i = i + 1
  }
  return (list(results.x, results.y))
}


prob1 <- function(mat, in_a_row){
  n = 0 #num steps
  state = 0 #current state - 0 indexed
  #loop until in final state then break and return how many steps it took
  while(state < in_a_row){
    state = which(rmultinom(1, 1, prob = mat[state+1,] ) == 1) #sample from all states with probabilities from trans matrix
    n = n+1
  }
  return(n)
}


#initialize probabilities and numebr of heads to get in a row
head.prob = 0.5
tail.prob = 1-head.prob
heads_in_a_row = 4

#make trans matrix based on above probabilities and needed size (ugly code)
transition.matrix = matrix(0, nrow=heads_in_a_row + 1, ncol=heads_in_a_row + 1)
transition.matrix[,1] = tail.prob
for(i in seq(heads_in_a_row)){
  transition.matrix[i,i+1] = head.prob
}
transition.matrix[heads_in_a_row + 1,] = 0
transition.matrix[heads_in_a_row+ 1,heads_in_a_row + 1] = 1


#sample the number of steps required 'k' times and record the results
k <- 10000
result <- rep(0, k)
for(i in seq(k)){
  result[i]<-prob1(transition.matrix, heads_in_a_row)
}

#get statistics/metrics

#1.1
mean(result)
#1.2
var(result)
#1.3
length(which(result==10))/length(result)



########     Part 2     ########


s = c(.1, .2, .35, .5, .7, .9, 1.0)
tau = 4


prior <- function(l, tau){
  return(tau * tau * l * exp(-l * tau))
}

dens <- function(x, l){
  return(l * exp(-x * l))
}

f <- function(l, x, tau){
  total = prior(l, tau)
  total = total * dens(x, l)
  #divide by integral of f(x | lamda) f(lamda) to normalize
  return(total / (l * tau * tau * exp(-l * tau)) )
}

points =  seq(0,1,0.01)
li = rep(0, length(points))
for(x in seq(length(points))){
  li[x] <- (f(l, points[x], tau))
}

plot(points, li)


tau = 4
l = (length(s) + 1 ) / (tau + sum(s))

#mean
meanie = 1/l
meanie

# median

med = log(2)/l
med

# 95% credible interval

p = gen.sample(f, 10000, tau, l)
x.results = unlist(p[1])
# y.results = unlist(p[2])
# plot(x.results, y.results)
x.results <- sort(x.results)
x.results[round(0.025 * length(x.results))] #first point in 95% cred int
x.results[length(x.results)- round(0.025 * length(x.results))] #end point of 95% cred int

#change tau: 
tau = 10 

l = (length(s) + 1 ) / (tau + sum(s))

#mean
meanie = 1/l
meanie

# median

med = log(2)/l
med

# 95% credible interval

p = gen.sample(f, 10000, tau, l)
x.results = unlist(p[1])
x.results <- sort(x.results)
x.results[round(0.025 * length(x.results))] #first point in 95% cred int
x.results[length(x.results)- round(0.025 * length(x.results))] #end point of 95% cred int


#the mean and median increase greatly since the prior scale is much larger. 
#the credible interval grows in size and the lower endpoint is also larger as the points are spread out over a larger area. 

########     Part 3     ########


#(iv) write r func that numerically computes the limits of an equi-tailed credible interval

ci <- function(w=0.5, a1=10, b1=20, a2=20, b2=10, n=10, x=3){
  p = sample(c(rbeta(w * n, 10,20), rbeta((1-w) * n, 20,10) ))
  p = sort(p)
  
  print(p[round(0.025 * length(p))])
  print(p[length(p)- round(0.025 * length(p))]) #end point of 95% cred int
}

ci(n=1000)

#(v)
n = 10
x = 3
w = 0.5
points = sample(c(rbeta(w * n, 10,20), rbeta((1-w) * n, 20,10) ))

prior = points
posterior = sample(c(rbeta(w * n, 10 + x,20 + n - x), rbeta((1-w) * n, 20 + x, 10+ n - x) ))

plot(density(prior), type="p", col="red", xlab = "x", ylab = "density", main = "Density estimate") 
points(density(posterior))


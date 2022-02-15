
########### 2.4 ###########

#as I usually like to do, I use a perturbation of rejection sampling
count = 0
i = 1
n = 1000
results.x = c()
while (count < n) {
  x = rexp(1, 3)
  if( x <= 1){
    results.x = append(results.x, x)
    count = count + 1
  }
}
hist(results.x)



########### 2.5 ###########
mean(results.x)
var(results.x)

########### 3.3 ###########

#generate sample using rejection sampling
gen.sample = function(f, n){
  x = runif(n * 10, 0,1)
  y = runif(n* 10, 0, 4.25) #4.25 is max of the function
  count = 0
  i = 1
  results.y = c()
  results.x = c()
  while (i < n * 10 & count < n) {
    if( y[i] < f(x[i])){
      results.y = append(results.y, y[i])
      results.x = append(results.x, x[i])
      count = count + 1
    }
    i = i + 1
  }
  return (results.x)
}

#define my function with lambda1 = 2, lambda2 = 5
fun = function(x){
  return (2 * 1/4 * exp(- 2 * x) + 5 *  3 /4 * exp(- 5 * x))
}

#call function
x = gen.sample(fun, 1000)
hist(x)

#get mean and var
mean(x)
var(x)

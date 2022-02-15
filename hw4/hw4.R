

#generate data
dat <- rnorm(500, 5, 2)
amin<- min(dat)-2
bmax<- max(dat)+2

# Choose the number of basis functions
num_basis<-50
m = 3
h = 0.5
h = (bmax-amin)/(num_basis+1-m)
n=length(dat)

#b spline of degree three that is slightly modified to be more generalizable, but R is wack and I had some issues with 
# using vectorize on it so I manually made it be able to take a vector input.
n_3 <- function(x, start = 0, end = 3){
  if (length(x) > 1){
    result = c()
    for(l in x){
      if (l <= 0 | l >= 3){return(0)}
      else if (l < 1){result = c(result, (l - start)^2 / 2) }
      else if (l < 2){result = c( -(l-(end-start)/2 )^2 + 3/4)}
      else if (l < 3) {result = c((l-end)^2 / 2)}
      return(result)
    }
  }
  if (x <= 0 | x >= 3){return(0)}
  else if (x < 1){return((x - start)^2 / 2)}
  else if (x < 2){return( -(x-(end-start)/2 )^2 + 3/4)}
  else if (x < 3) {return((x-end)^2 / 2)}
}

#notes for self: B_i is just n()
#fn is sum of alpha * B_i / h
#for above function use n_3(x/h - i, 0, 3) so i don't have to do extra maths and it makes it identical to the slides.
# i intervals (so like ith bucket if histogram)

#get the alpha 
alpha <- function(x, i, h, n){
  a = 0
  for (k in seq(length(x))){
    a = a + n_3(x[k]/h - i, 0, 3)
  }
  a = a / n
  return(a)
}
#input x is a vector, n is total number of points, h is wit
f <- function(x){
  total = 0
  for(i in seq(1,num_basis)){
    # print(n_3(x/h - i, 0, 3) * alphas[i])
    total <-  total + n_3(x/h - i, 0, 3) * alphas[i] / h
  }
  return(total)
}

#get the alphas
alphas=c()
for(i in seq(num_basis)){
  alphas = c(alphas, alpha(dat, i, h, length(dat)))
}


#helper func for finding mean
me <- function(x){
  return (x * f(x))
}
b_spline_mean <- function(data){
  return(integrate(Vectorize(me), -Inf, Inf))
}
#actual mean is 5 (pretty spot on)
b_spline_mean(dat)

#helper func for finding var
v <- function(x){
  return ((x-mean(dat))^2 * f(x))
}

b_spline_var <- function(data){
  return(integrate(Vectorize(v), -Inf, Inf))
}

#actual variance is square of SD so 4 (a little more variation between data sets used but still pretty close most of the time)
b_spline_var(dat)



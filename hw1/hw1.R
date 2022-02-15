

f = function(input.list){
  output.list <-c()
  for (x in input.list){
    
    if (x >=0 & x < 1){
      output.list = append(output.list, x)
    }
    else if(x >=1 & x <=2){
      output.list = append(output.list, 2-x)
    }
    else{
      output.list = append(output.list, 0)
    }
  }
  return(output.list)
}

x <- seq(-1,3, 0.01)

print(f(x))

plot(x, f(x))

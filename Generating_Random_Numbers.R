### exponential distribution ###
random_exp=function(n,lamda){
  expd=c()
  i=1
  while (i>0 & i<=n) {
    u=runif(1,0,1)
    e= ((-1)*(1/lamda)*(log(1-u)))
    expd=append(expd,e)
    i=i+1
  }
  print(expd)
} 



random_exp(1000,0.235)

hist(random_exp(1000,0.235),main = paste("Histogram of" , 'Exponential distribution'))
plot(density(random_exp(10000,0.235)))


### Gamma Distribution ###

random_gamma = function(k,m,lamda){
  gammad = c()
  i = 1
  while (i>0 & i<=k){
    value = sum(random_exp(m,lamda))
    i = i+1
    gammad = append(gammad, value)
  }
  print(gammad)
}



k=random_gamma(1000,10,0.3)
k[1:1000]

hist(k[1:1000],main = paste("Histogram of" , 'Gamma Distribution'))
plot(density(k[1:1000]))


### CHI-Squared distribution ###

random_chi= function(k,n){
  print(random_gamma(k,n,0.5))
}


j=random_chi(200,10)

hist(j[1:200],main = paste("Histogram of" , 'CHI-squared distribution'))
plot(density(j[1:200]))


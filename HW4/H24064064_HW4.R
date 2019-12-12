#1
data.y <- c(14.52, 8.49, 11.86, 6.42, 8.41, 7.66, 10.4, 9.99, 16.49,
              6.55, 16.54, 15.53, 5.66, 14.67, 9.06, 13.69, 8.49, 12.72,
              7.86, 13.03, 13.06, 5.67, 8.18, 18.74, 7.63, 14.76, 18.28,
              15.82, 12.67, 11.72, 16.13, 11.5, 11.88, 9.3, 12.67, 10.61,
              12.35, 8.41, 11.17, 14.91, 5.58, 7.74, 12.78, 11.32, 11.12,
              12.01, 13.75, 11.36, 11.63, 10.22)



par[,1]=c(mean(data.y),var(data.y))
#(c)
gibbs1=function(freq,data.y,theta,tau_square){
  n=length(data.y)
  par=matrix(rep(NA,freq*2),nrow=2,ncol = freq)       
  par[,1]=c(1,1)
  for(i in 2:freq){
    A=(tau_square*sum(data.y)+theta*par[2,i-1])/(n*tau_square+par[2,i-1])
    B=tau_square*par[2,i-1]/(n*tau_square+par[2,i-1])
    par[1,i]=rnorm(1,A,sqrt(B))
    par[2,i]=1/rgamma(1,n/2,sum((data.y-par[1,i])^2)/2)
  }
  return(par)
}



#(d)
freq=100000
theta=11
tau_square=8
res=gibbs1(freq,data.y,theta,tau_square)

plot(res[1,seq(0.1*freq,freq)],res[2,seq(0.1*freq,freq)])



pdf <- function(data.y,mu, sigma_square,theta,tau_square){
  fxy <- matrix(NA, length(mu), length(sigma_square))
  n=length(data.y)
  for(i in 1:length(mu)){
    for(j in 1:length(sigma_square)){
      fxy[i, j] =(2*3.14*sigma_square[j])^(-n/2)*exp(-sum((data.y-mu[i])^2)/(2*sigma_square[j]))*exp(-(mu[i]-theta)^2/(2*tau_square))/sigma_square[j]
    }
  }
  return(fxy)
}
mu=seq(9,14,0.1)
sigma_square=seq(5,25,0.1)
contour(mu, sigma_square, pdf(data.y,mu,sigma_square,theta,tau_square),nlevels = 15, col = 2, add = TRUE)




#(e)
mean(res[1,seq(0.1*freq,freq)])
mean(res[2,seq(0.1*freq,freq)])




hist(data.y,probability  =T)
x=seq(floor(min(data.y)),ceiling(max(data.y)),0.01)
pdf=dnorm(x,res[1,freq],sqrt(res[2,freq]))
lines(x,pdf,col="red")

#2

gibbs2=function(n,a,b,c){
  par=matrix(rep(NA,freq*3),nrow=freq,ncol = 3)       
  par[1,]=c(1,1,1)
  for(i in 2:freq){
    par[i,1]=rexp(1,a*par[i-1,2]+b*par[i-1,3]+1)
    par[i,2]=rexp(1,a*par[i,1]+c*par[i-1,3]+1)
    par[i,3]=rexp(1,b*par[i,1]+c*par[i,2]+1)
  }
  return(par)
}

a=2
b=3
c=6
freq=30000
res=gibbs2(freq,a,b,c)
hist(res[seq(0.1*freq,freq),1],probability = T)
hist(res[seq(0.1*freq,freq),2],probability = T)
hist(res[seq(0.1*freq,freq),3],probability = T)

mean(res[seq(0.1*freq,freq),1])
mean(res[seq(0.1*freq,freq),2])
mean(res[seq(0.1*freq,freq)],3)


plot(res[seq(0.1*freq,freq),1],res[seq(0.1*freq,freq),2],col="blue")
points(res[seq(0.1*freq,freq),1],res[seq(0.1*freq,freq),2],col="black")
points(res[seq(1,0.1*freq),1],res[seq(1,0.1*freq),2],col="red")

plot(res[seq(0.1*freq,freq),2],res[seq(0.1*freq,freq),3],col="blue")
points(res[seq(0.1*freq,freq),2],res[seq(0.1*freq,freq),3],col="black")
points(res[seq(1,0.1*freq),2],res[seq(1,0.1*freq),3],col="red")


plot(res[seq(0.1*freq,freq),1],res[seq(0.1*freq,freq),3],col="blue")
points(res[seq(0.1*freq,freq),1],res[seq(0.1*freq,freq),3],col="black")
points(res[seq(1,0.1*freq),1],res[seq(1,0.1*freq),3],col="red                                                                                                                                                                                                                     ")

library("scatterplot3d") # load
scatterplot3d(res[seq(0.1*freq,freq),], angle = 55)

hist(res[seq(0.1*freq,freq),1]*res[seq(0.1*freq,freq),2]*res[seq(0.1*freq,freq),3],probability = T)
#E(XYZ)
mean(res[seq(0.1*freq,freq),1]*res[seq(0.1*freq,freq),2]*res[seq(0.1*freq,freq),3])

#3
gibbs3=function(freq,n,alpha,beta){
  par=matrix(rep(NA,freq*2),nrow=2,ncol = freq)       
  par[,1]=c(1,1)
  for(i in 2:freq){
    par[1,i]=rbinom(1,n,par[2,i-1])
    par[2,i]=rbeta(1,par[1,i]+alpha+1,n-par[1,i]+beta+1)
  }
  return(par)
}
freq=100000
n=10
alpha=3
beta=5
res=gibbs3(freq,n,alpha,beta)
plot(res[1,seq(0.1*freq,freq)],res[2,seq(0.1*freq,freq)])

pdf <- function(x, y,n,alpha,beta){
  fxy <- matrix(NA, length(x), length(y))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      fxy[i, j] =choose(n,x[i])*y[j]^(x[i]+alpha)*(1-y[j])^(n-x[i]+beta)
    }
  }
  return(fxy)
}
xx <- seq(0, 10, 1)
yy <- seq(0, 1, 0.01)
contour(xx, yy, pdf(xx, yy, n,alpha,beta),nlevels = 15, col = 2, add = TRUE)





#4
#(a)
c.normal=function(freq,mu1,mu2,sigma1,sigma2,rho){
  par=matrix(rep(NA,freq*2),nrow=2,ncol = freq)   
  par[,1]=c(0,0)
  c.sigma1=sqrt((1-rho^2)*sigma1^2)
  c.sigma2=sqrt((1-rho^2)*sigma2^2)
  for(i in 2:freq){
    c.mean1=mu1+rho*sigma1/sigma2*(par[2,i-1]-mu2)
    par[1,i]=rnorm(1,c.mean1,c.sigma1)
    c.mean2=mu2+rho*sigma2/sigma1*(par[1,i]-mu1)
    par[2,i]=rnorm(1,c.mean2,c.sigma2)
  }
  return(par)
}


gibbs4=function(freq,w1,w2,mu11,mu12,mu21,mu22,sigma11,sigma12,sigma21,sigma22,rho1,rho2){
  x=matrix(rep(NA,freq*2),nrow=2,ncol = freq) 
  res1=c.normal(freq,mu11,mu12,sigma11,sigma12,rho1)
  res2=c.normal(freq,mu21,mu22,sigma21,sigma22,rho2)
  for(i in 1:freq){
    U=runif(1)
    if(U<w1){
      x[,i]=res1[,i]
    }else{
      x[,i]=res2[,i]
    }
  }
  return(x)
}
#(b)

freq=100000
w1=0.7
w2=0.3
mu11=-5
mu12=-7
mu21=5
mu22=7
rho1=-0.7
rho2=0
sigma11=1
sigma12=1
sigma21=sqrt(2)
sigma22=sqrt(3)
x=gibbs4(freq,w1,w2,mu11,mu12,mu21,mu22,sigma11,sigma12,sigma21,sigma22,rho1,rho2)
plot(x[1,seq(0.1*freq,freq)],x[2,seq(0.1*freq,freq)])







w1=0.7
w2=0.3
mu1=matrix(c(-5,-7),ncol=1)
mu2=matrix(c(5,7),ncol=1)
cov1=matrix(c(1,-0.7,-0.7,1),nrow=2)
cov2=matrix(c(2,0,0,3),nrow=2)



pdf <- function(x1, x2,w1,w2,mu1,mu2,cov1,cov2){
  inv1=solve(cov1)
  inv2=solve(cov2)
  det1=det(cov1)
  det2=det(cov2)
  fx1x2 <- matrix(NA, length(x1), length(x2))
  for(i in 1:length(x1)){
    for(j in 1:length(x2)){
      x=matrix(c(x1[i],x2[j]),ncol=1)
      fx1x2[i, j] =w1*exp(-t(x-mu1)%*%inv1%*%(x-mu1)/2)/sqrt(det1)+w2*exp(-t(x-mu2)%*%inv2%*%(x-mu2)/2)/sqrt(det2)
    }
  }
  return(fx1x2)
}
x1x1 <- seq(-10, 10, 0.01)
x2x2 <- seq(-10, 10, 0.01)
w1=0.7
w2=0.3
mu1=matrix(c(-5,-7),ncol=1)
mu2=matrix(c(5,7),ncol=1)
cov1=matrix(c(1,-0.7,-0.7,1),nrow=2)
cov2=matrix(c(2,0,0,3),nrow=2)

contour(x1x1, x2x2, pdf(x1x1, x2x2,w1,w2,mu1,mu2,cov1,cov2),nlevels = 15, col = 2, add = TRUE)



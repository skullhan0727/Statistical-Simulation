###1
#inverse transform method
inverse_transoform=function(n){
  U=runif(n)
  X=log(U*(exp(1)-1)+1)
  return(X)
}
inverse_transoform=inverse_transoform(100000)
hist(inverse_transoform,probability = T,breaks=100)
x=seq(0,1,0.01)
fx=exp(x)/(exp(1)-1)
lines(x,fx,col="blue")

mean(inverse_transoform)

#rejection method
#找c
f.g <- function(x){
  exp(x)/(exp(1)-1)
}
x=seq(0,1,0.01)
plot(x,f.g(x),type="l")
det.c <- optim(1, f.g, lower = 0, upper = 1, method = "L-BFGS-B",control = list(fnscale = -1)) ### maximization
det.c$par ### the location of the optimum
det.c$value
c=det.c$value
#用unif(0,1)模擬
rejection=function(n){
  X=rep(NA,n)
  iter=rep(NA,n)
  for(j in 1:n){
    U=runif(1)
    Y=runif(1)
    i=1
    while(U>exp(Y)/(exp(1)-1)/c){
      U=runif(1)
      Y=runif(1)
      i=i+1
    }
    X[j]=Y
    iter[j]=i
  }
  return(list(X=X,iter=iter))
}
rejection=rejection(100000)
rejection_X=rejection$X
hist(rejection_X,probability = T,breaks=100)
x=seq(0,1,0.01)
fx=exp(x)/(exp(1)-1)
lines(x,fx,col="blue")

mean(rejection$iter)  #平均iter =c
mean(rejection_X)
###2
sim2=function(n){
  U1=runif(n)
  X=-log(U1)/2 #EXP(2)
  U2=runif(n)
  for(i in 1:n){
    if(U2[i]<0.5){
      X[i]=-X[i]
    }
  }
  return(X)
}
X_2=sim2(100000)

hist(X_2,probability = T,ylim=c(0,1),breaks=100)
x=seq(0,6,0.01)
lines(x,exp(-2*x),col="blue")
y=seq(-6,0,0.01)
lines(y,exp(2*y),col="blue")

###3
#用Ｙ＝Exp(1/2) 

#找c
x=seq(0,30,0.01)
plot(x,(1+x)*exp(-(1/2)*x))

f.g <- function(x){
  (1+x)*exp(-(1/2)*x)
}
det.c <- optim(1, f.g, lower = 0, upper = 10, method = "L-BFGS-B",control = list(fnscale = -1)) ### maximization
det.c$par ### the location of the optimum
det.c$value
c=det.c$value

rejection_3=function(n){
  X=rep(NA,n)
  iter=rep(NA,n)
  for(j in 1:n){
    Y=-log(runif(1))*2  #Ｙ＝Exp(1/2) 
    U=runif(1)
    i=1
    fx=1/2*(1+Y)*exp(-Y)
    gx=1/2*exp(-(1/2)*Y)
    while(U>fx/gx/c){
      Y=-log(runif(1))*2
      U=runif(1)
      fx=1/2*(1+Y)*exp(-Y)
      gx=1/2*exp(-(1/2)*Y)
      i=i+1
    }
    X[j]=Y
    iter[j]=i
  }
  return(list(X=X,iter=iter))
}

sim_3=rejection_3(100000)
X=sim_3$X
iter=sim_3$iter


hist(X,probability = T,ylim=c(0,1),breaks=100)
x=seq(0,20,0.01)
lines(x,1/2*(1+x)*exp(-x),col="blue")

mean(X)
mean(iter)
###4
composite=function(n){
  U=runif(n)
  U1=runif(n)
  X=rep(NA,n)
  for(i in 1:n){
    if(U[i]<1/3){
      X[i]=U1[i]
    }
    else if(U[i]<2/3){
      X[i]=(U1[i])^(1/3)
    }
    else{
      X[i]=(U1[i])^(1/5)
    }
  } 
  return(X)
}
X=composite(100000)

hist(X,probability = T,breaks=100)
x=seq(0,1,0.01)
lines(x,(1+3*x^(2)+5*x^(4))/3,col="blue")

mean(X)

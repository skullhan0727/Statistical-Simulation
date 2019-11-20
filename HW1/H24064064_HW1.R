###9

#simulation
integral<-function(n){
  ux<-runif(n)
  uy<-runif(n)
  I<-function(x,y) {ifelse(y<=x,1,0)}
  x=(1-ux)/ux
  y=(1-uy)/uy
  return(mean(exp(-(x+y))*I(x,y)/ux^2/uy^2))
}

ans=integral(100000)
ans
#exact answer
#1/2

###13

##(a)
simulation_13<-function(n){
  N<-rep(NA,n)
  for(j in 1:n){
    U<-runif(100)
    multiple<-1
    for(i in 1:100){
      multiple<-multiple*U[i]
      if(multiple<exp(-3)){
        N[j]<-i-1
        break
      }
    }
  }
  return(N)
}
mean=mean(simulation_13(100000))
mean
##(b)
N<-simulation_13(100000)
sum(N==0)/length(N) #P(N=0)
sum(N==1)/length(N) #P(N=1)
sum(N==2)/length(N) #P(N=2)
sum(N==3)/length(N) #P(N=3)
sum(N==4)/length(N) #P(N=4)
sum(N==5)/length(N) #P(N=5)
sum(N==6)/length(N) #P(N=6)

###14
x=c(23,66,rep(NA,12))
for(i in 3:14){
  x[i]<-(3*x[i-1]+5*x[i-2])%%100
}
u=x/100
u
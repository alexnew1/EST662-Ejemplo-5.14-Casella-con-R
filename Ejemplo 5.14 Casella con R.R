Fx<-function(y,tol,a,ITER)
{
  ybar<-mean(y,na.rm=TRUE)
  theta<-rnorm(1,mean=ybar,sd=sd(y,na.rm=TRUE))
  a1<-which(is.na(y),arr.ind=TRUE)
  n<-length(y)
  m<-n-length(a1)
  iter<-1
  error<-NA
  #while( iter<ITER)
  if(tol < error[iter] || iter<ITER)
  {
    theta<-c(theta,(m/n)*ybar+((n-m)/n)*
               (theta[iter]+(dnorm(a-theta[iter]))/(pnorm(a-theta[iter]))))
    iter<-iter+1
    error[iter]<- abs(theta[iter]-theta[iter-1])
  }
  z<-list(theta=theta,iter=iter, error=error)
  return(z)
}

y<-c(rnorm(20,5,1),rep(NA,10))

a<-1
Theta=Fx(y,0.00001,a,100)


Theta


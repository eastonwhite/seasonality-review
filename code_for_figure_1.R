# Created by Easton R. White
# Last updated: 21-Sep-2018


# This code solves a simple periodically-forced logistic model for different values of periodic forcing


#create a pdf, optional
#pdf(file='Fig1_seasonal_logistic_eq.pdf',width=8,height=10)

#setup plotting scheme
par(mai=c(0.1,0.5,0,0),oma=c(3,2,0.5,0.5))

aaa=matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,
             1,1,1,1,0,4,4,4,4,4,4,4,
             1,1,1,1,0,4,4,4,4,4,4,4,
             1,1,1,1,0,4,4,4,4,4,4,4,
             2,2,2,2,0,4,4,4,4,4,4,4,
             2,2,2,2,0,4,4,4,4,4,4,4,
             2,2,2,2,0,5,5,5,5,5,5,5,
             2,2,2,2,0,5,5,5,5,5,5,5,
             3,3,3,3,0,5,5,5,5,5,5,5,
             3,3,3,3,0,5,5,5,5,5,5,5,
             3,3,3,3,0,5,5,5,5,5,5,5,
             3,3,3,3,0,0,0,0,0,0,0,0)
           
           ,12,12,byrow=T)
layout(aaa)
#layout(matrix(c(1,1,0,0,0),12,12,byrow=T))

require(deSolve)
logistic <- function(t,y,p){
  r=1#+ (cos(2*pi*t))
  K=10
  dy <- r*y*(1 - (y/K))
  list(c(dy))
}

yini <- c(y=5)


#head(resourcepulse)

times <-seq(from=0,to=10,by=1/24)
#times <- sort(runif(20,0,10))
outPop <- ode(func=logistic, times=times,y=yini,parms=NULL)
plot(outPop[,1],outPop[,2],ylim=c(0,16),col='black',type='l',ylab=' ',xlab=' ',main=' ',cex.axis=1.2,cex.lab=1.2,xaxt='n',las=1)
abline(h=10,lty=2,las=1)


mtext(3,at=9.5,text = '(a)',line = -2,outer = F,cex = 1.2,font = 2)
mtext(3,at=5,text=expression(K[1] == 0),cex=1.2,outer=F,line=-2,adj = 0.5)

#Making K a seasonally forced term
logistic <- function(t,y,p){
  #r=1#+ (cos(2*pi*t))
  K=10+(cos(2*pi*t))
  dy <- r*y*(1 - (y/K))
  list(c(dy))
}

r=1;yini <- c(y=5)


times <-seq(from=0,to=10,by=1/24)
outPop <- ode(func=logistic, times=times,y=yini,parms=NULL)
plot(outPop[,1],outPop[,2],ylim=c(0,16),col='black',type='l',ylab=' ',xlab=' ',main=' ',cex.axis=1.2,cex.lab=1.2,xaxt='n', las=1)
points(times,10+(cos(2*pi*times)),col='black',lty=2,type='l')

mtext('Population size',side=2,outer=F,line=2.5,cex=1.2)
mtext(3,at=9.5,text = '(b)',line = -2,outer = F,cex = 1.2,font = 2)
mtext(3,at=5,text=expression(K[1] == 1),cex=1.2,outer=F,line=-2,adj = 0.5)

#Making K a seasonally forced term
logistic <- function(t,y,p){
  #r=1#+ (cos(2*pi*t))
  K=10+(K1*cos(2*pi*t))
  dy <- r*y*(1 - (y/K))
  list(c(dy))
}

r=1;yini <- c(y=5);K1=4


times <-seq(from=0,to=10,by=1/24)
outPop <- ode(func=logistic, times=times,y=yini,parms=NULL)
plot(outPop[,1],outPop[,2],ylim=c(0,16),col='black',type='l',ylab=' ',xlab=' ',main=' ',cex.axis=1.2,cex.lab=1.2,las=1)
points(times,10+(K1*cos(2*pi*times)),col='black',lty=2,type='l')


mtext('Time',side=1,outer=F,line=2.5,cex=1.2)
mtext(3,at=9.5,text = '(c)',line = -2,outer = F,cex = 1.2,font = 2)
mtext(3,at=5,text=expression(K[1] == 4),cex=1.2,outer=F,line=-2,adj = 0.5)

#####################
K1_vector=seq(0,6,by=0.1)
maxy=NULL
for (i in 1:61){
  logistic <- function(t,y,p){
    #r=1#+ (cos(2*pi*t))
    K=10+(K1*cos(2*pi*t))
    dy <- r*y*(1 - (y/K))
    list(c(dy))
  }
  
  r=1;yini <- c(y=5);K1=K1_vector[i]
  
  
  times <-seq(from=0,to=10,by=1/24)
  outPop <- ode(func=logistic, times=times,y=yini,parms=NULL)
  
  maxy[i]=max(outPop[,2])
  #plot(outPop[,1],outPop[,2],ylim=c(0,15),col='black',type='l',ylab='Population size',xlab='Time',main=' ',cex.axis=1.2,cex.lab=1.2,xaxt='n')
  #points(times,10+(K1*cos(2*pi*times)),col='black',lty=2,type='l')
  
}

plot(K1_vector,maxy,type='l',ylab='   ',xlab=' ',xaxt='n',cex.axis=1.2,las=1)

mtext(3,at=5.9,text = '(d)',line = -2,outer = F,cex = 1.2,font = 2)
mtext('Maximum population size',side=2,outer=F,line=3,cex=1.2)


K1_vector=seq(0,6,by=0.1)
maxy=NULL
for (i in 1:61){
  logistic <- function(t,y,p){
    #r=1#+ (cos(2*pi*t))
    K=10+(K1*cos(2*pi*t))
    dy <- r*y*(1 - (y/K))
    list(c(dy))
  }
  
  r=1;yini <- c(y=5);K1=K1_vector[i]
  
  
  times <-seq(from=0,to=10,by=1/24)
  outPop <- ode(func=logistic, times=times,y=yini,parms=NULL)
  
  maxy[i]=range(outPop[180:240,2])[2]- range(outPop[180:240,2])[1]
  #plot(outPop[,1],outPop[,2],ylim=c(0,15),col='black',type='l',ylab='Population size',xlab='Time',main=' ',cex.axis=1.2,cex.lab=1.2,xaxt='n')
  #points(times,10+(K1*cos(2*pi*times)),col='black',lty=2,type='l')
  
}

plot(K1_vector,maxy,type='l',ylab=' ',xlab=' ',ylim=c(0,2),cex.axis=1.2,las=1)
mtext(expression(K[1]),side=1,line=3,outer=F,cex=1.2)
mtext(3,at=5.9,text = '(e)',line = -2,outer = F,cex = 1.2,font = 2)
mtext('Amplitude in population size',side=2,outer=F,line=3,cex=1.2)
#dev.off()
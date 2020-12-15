## ----eval=FALSE---------------------------------------------------------------
#  library(deSolve)
#  si <- function(i0,beta,t_range,N=1e4){
#    devi <- function(t,i,parms) list(beta * i *(1-i))
#    infe <- ode(y = i0, times = t_range, func = devi, parms = NULL)
#    susc <- cbind(t_range,1-infe[,2])
#    I <- cbind(t_range,infe[,2]*N)
#    S <- cbind(t_range,susc[,2]*N)
#    if(infe[length(t_range),2]==1) in.time <- infe[infe[,2]>=1,][1,1] else
#      in.time <-'The time step is too short'
#    return(list(infe=infe, susc=susc, I=I, S=S, in.time=in.time))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  SI <- si(i0=1e-4,beta=1.2,t_range=seq(0,100,1))
#  plot(SI$infe,type='l',col='red',xlab = 'time' , ylab='percentage',main='SI model')
#  lines(SI$susc,type='l',col='blue')
#  legend('right',c('infective percentage','susceptible percentage'),col=c('red','blue'),lty=c(1,1))

## ----eval=FALSE---------------------------------------------------------------
#  library(deSolve)
#  sir <- function(i0,r0,beta,gam,t_range=seq(0,100,1)){
#    s0 <- 1-i0-r0
#    devr <- function(t,r,parms) list(gam*(1-r-s0*exp(-beta*r/gam)))
#    reco <- ode(y = r0, times = t_range, func = derivs_r, parms = NULL)
#    s <- s0*exp(-beta*reco[,2]/gam)
#    susc <- cbind(t_range,s0*exp(-beta*reco[,2]/gam))
#    infe <- cbind(t_range,pmax(1-susc[,2]-reco[,2],0))
#    t.peak <- infe[which.max(infe[,2]),1]
#    if(infe[length(t_range),2]>1e-10) t.stop <- infe[infe[,2]<1e-10,][1,1] else
#      t.stop <-'The time step is too short'
#    return(list(infe=infe, susc=susc, reco=reco,t.peak=t.peak,t.stop=t.stop))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  SIR <- sir(i0=0.01,r0=0,beta=1.2,gam=1,t_range=seq(0,100,1))
#  plot(SIR$infe,type='l',col='red',xlab = 'time' , ylab='percentage',main='SIR model')
#  lines(SIR$susc,type='l',col='blue')
#  lines(SIR$reco,type='l',col='green')
#  legend('right',c('infective percentage','susceptible percentage','recovery percentage'),col=c('red','blue','green'),lty=c(1,1,1))

## ----eval=FALSE---------------------------------------------------------------
#  library(deSolve)
#  sis <- function(i0,beta,gam,t_range=seq(0,100,1)){
#    s0 <- 1-i0
#    devi <- function(t,i,parms) list((beta-gam-beta*i)*i)
#    infe <- ode(y = i0, times = t_range, func = devi, parms = NULL)
#    susc <- cbind(t_range,1-infe[,2])
#  
#    if(infe[length(t_range),2]-infe[length(t_range)-1,2]==0){
#      t.stable <- infe[infe[length(t_range),2]-infe[,2]<1e-6,][1,1]
#      per.stable <- infe[t.stable,2]
#    } else {
#      t.stable <-'The time step is too short'
#      per.stable <-'The time step is too short'
#    }
#    return(list(infe=infe, susc=susc,t.stable=t.stable,per.stable=per.stable))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  SIS <- sis(i0=0.01,beta=1.2,gam=0.4,t_range=seq(0,100,1))
#  plot(SIS$infe,type='l',col='red',xlab = 'time' , ylab='percentage',main='SIS model')
#  lines(SIS$susc,type='l',col='blue')
#  legend('right',c('infective percentage','susceptible percentage'),col=c('red','blue'),lty=c(1,1))
#  #'


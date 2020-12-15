## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
wcoef=lm(women)$coef
plot(women,main='height-weight')

## -----------------------------------------------------------------------------
ir=data.frame(iris[1:10,])
knitr::kable(ir)

## -----------------------------------------------------------------------------
g=function(y) 2/((1-y)^(1/2))
n=1e4
set.seed(1234)
U=runif(n)
da=g(U)
hist(da,breaks=seq(0,100,0.5),freq=F)
curve(8*x^(-3),add=T,col='red')

## -----------------------------------------------------------------------------
n=1e4
set.seed(2345)
U=matrix(runif(3*n,-1,1),ncol=3)
colnames(U)=c('U1','U2','U3')
da=numeric()
for(i in 1:n){
  if(abs(U[i,3])>=abs(U[i,2]) & abs(U[i,3])>=abs(U[i,1])) da[i]=U[i,2] else
    da[i]=U[i,3]
}
hist(da,breaks = seq(-1,1,0.05),freq=F)
curve(3*(1-x^2)/4,add=T,col='red')

## -----------------------------------------------------------------------------
n=1e3
a1=rgamma(n,4,2)
a2=rexp(n,a1)
hist(a2,breaks=seq(0,20,0.5),freq = F)
curve(64*(2+x)^(-5),add = T,col='red')

## -----------------------------------------------------------------------------
set.seed(3456)
n <- 1e4                     
x <- runif(n,0,pi/3)  
g <- function(x) sin(x)*pi/3
theta.hat <- mean(g(x))       #the Monte Carlo estimate
theta <- cos(0)-cos(pi/3)    #the exact value of the integral
print(c(theta.hat,theta))

## -----------------------------------------------------------------------------
set.seed(3456)
n <- 1e4                   
x <- runif(n,0,1)  
theta_1 <- mean(exp(x))                    #theta_1: by the simple Monte Carlo estimate
m <- 5e3
y <- runif(m,0,1)
theta_2 <- mean((exp(y)+exp(1-y))/2)       #theta_2: by the antithetic variate approach
theta <- exp(1)-1                          #theta: the exact value of the integral
th <- c(theta_1,theta_2,theta)
names(th) <- c('theta_1.','theta_2.','theta.')
print(th)

## -----------------------------------------------------------------------------
set.seed(3456)
n <- 1e4                   
x <- runif(n,0,1)  
var_theta_c <- var((exp(x)+exp(1-x))/2) #the sample variance of the estimate achieved by using antithetic approach
var_theta <- var(exp(x))                #the sample variance of the estimate achieved by using simple MC
pr <- c(0.983835,1-var_theta_c/var_theta)     #pr: the percent reduction and an empirical estimate of the percent reduction
names(pr)=c('the result from Exercise5.6.','empirical estimate.')
print(pr)

## -----------------------------------------------------------------------------
m=1e4
theta_hat <- numeric(2)
se <- numeric(2)

g <- function(x) (x^2*exp(-x^2/2)/sqrt(2*pi)) * (x>=1)
f1 <- function(x) dnorm(x,1.5,1)
f2 <- function(x) dgamma(x,4,2)

par(mfrow=c(1,1))
curve(g(x),xlim = c(1,5),ylim = c(0,0.5),ylab = 'y')
curve(f1(x),xlim = c(1,5),ylim = c(0,0.5),add = T,col = 2,lty = 2)
curve(f2(x),xlim = c(1,5),ylim = c(0,0.5),add = T,col = 3,lty = 3)
legend("topright", legend = c('g(x)','f1(x)','f2(x)'),
           lty = 1:3, lwd = 2, inset = 0.02, col=1:3)

## -----------------------------------------------------------------------------
set.seed(1234)

x <- rnorm(m,1.5,1)          #using f1
fg <- g(x) / dnorm(x,1.5,1)
theta_hat[1] <- mean(fg)
se[1] <- sd(fg)

x <- rgamma(m,4,2)          #using f2
fg <- g(x) / dgamma(x,4,2)
theta_hat[2] <- mean(fg)
se[2] <- sd(fg)

k <- rbind(theta_hat,se)
colnames(k) <- c('f1','f2')

print(k)


## -----------------------------------------------------------------------------
par(mfrow=c(1,1))
curve(g(x)/g(x),xlim = c(1,4),ylim = c(0,1),ylab = 'y')
curve(g(x)/f1(x),xlim = c(1,4),ylim = c(0,1),add = T,col = 2,lty = 2)
curve(g(x)/f2(x),xlim = c(1,4),ylim = c(0,1),add = T,col = 3,lty = 3)
legend("topright", legend = c('g(x)/g(x)','g(x)/f1(x)','g(x)/f2(x)'),
           lty = 1:3, lwd = 2, inset = 0.02, col=1:3)

## -----------------------------------------------------------------------------
m <- 1e4       # sample size in each sub-interval
k <- 5         # number of the sub-intervals
g <- function(x) exp(-x)/(1+x^2)
f <- function(x) exp(-x)/(1-exp(-1))   #importance function
theta.hat <- numeric(5)
sigma2.hat <- numeric(5)

set.seed(4567)
for (j in 0:4) {
  u <- runif(m) 
  x <- -log(1-u*(1-exp(-1)))
  fg <- (g(x) * (x>j/k) * (x<(j+1)/k)) / (exp(-x) / (1-exp(-1)))
  theta.hat[j+1] <- mean(fg)
  sigma2.hat[j+1] <- var(fg)
}
theta <- sum(theta.hat)
se <- sqrt(sum(sigma2.hat)/m)

pa <- c(theta,se)
names(pa) <- c('theta_SI','se_SI')

print(pa)

## -----------------------------------------------------------------------------
n <- 20
m <- 1000
alpha <- 0.05
mu <- 1         #set logX~N(1,4) 
sigma <- 2

#general n random numbers from a lognormal distribution with unknown parameters 
U <- replicate(m, expr = {
  y <- rnorm(n,1,2)
  x <- exp(y)
  c(mean(y)-sd(y)*qt(1-alpha/2,n-1)/sqrt(n),mean(y)+sd(y)*qt(1-alpha/2,n-1)/sqrt(n))
} )

print(sum(U[1,]<1 & U[2,]>1)/m)   #empirical estimate of the confidence level.


## -----------------------------------------------------------------------------
n <- 20
m <- 1000
alpha <- 0.05
set.seed(4553)
CL <- replicate(1000, expr = {
  x <- rchisq(n,2)       #random samples of chisp^2 data
  c(mean(x)-qt(1-alpha/2,n-1)*sd(x)/sqrt(n),mean(x)+qt(1-alpha/2,n-1)*sd(x)/sqrt(n))
} )

cp <- mean(CL[2,]>2 & CL[1,]<2)   #coverage probability
print(cp)


## -----------------------------------------------------------------------------
skb <- function(x){
    #compute the sample coefficient of skewness
  xbar <- mean(x)
  b <- mean((x-xbar)^3)/(mean((x-xbar)^2))^1.5
  return(b)
}

alpha <- c(0,0.1,0.2,0.5,2,3,4,6,10,20,30,40,50,60,70,80,90,100)
al <- 0.05          #confidence level
n <- 20
m <- 1e4
N <- length(alpha)
pwr <- numeric(N)

cv <- qnorm(1-al/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))  #critical value for the skewness test

set.seed(3444)
for (j in 1:N) {    #for each alpha 
  a <- alpha[j]
  sktests <- numeric(m)
  for (i in 1:m) {   #for each replicate
    x <- rbeta(n,a,a)
    sktests[i] <- as.integer(abs(skb(x)) >= cv)
  }
  pwr[j] <- mean(sktests)
}
se <- sqrt(pwr*(1-pwr)/m)      #standard errors
p <- cbind(pwr,se)
rownames(p) <- paste(rep('alpha=',N),alpha,sep = '')
knitr::kable(p)

    #plot power vs alpha
plot(alpha, pwr, type = "b", ylim = c(0,0.06))
abline(h = .05, lty = 5, col = 2)
lines(alpha, pwr+se, lty = 3)
lines(alpha, pwr-se, lty = 3)


## -----------------------------------------------------------------------------
nu <- c(1,2,5,10,100,200,400)
alpha <- 0.05          #confidence level
n <- 30
m <- 2000
N <- length(nu)
pwr <- numeric(N)

cv <- qnorm(1-alpha/2,0,sqrt(6*(n-2)/((n+1)*(n+3))))  #critical value for the skewness test

set.seed(3444)
for (j in 1:N) {    #for each alpha 
  sktests <- numeric(m)
  for (i in 1:m) {   #for each replicate
    x <- rt(n,nu[j])
    sktests[i] <- as.integer(abs(skb(x)) >= cv)
  }
  pwr[j] <- mean(sktests)
}
se <- sqrt(pwr*(1-pwr)/m)      #standard errors
p <- cbind(pwr,se)
rownames(p) <- paste(rep('nu=',N),nu,sep = '')
knitr::kable(p)

    #plot power vs alpha
plot(nu, pwr, type = "b", ylim = c(0,1))
abline(h = .05, lty = 5, col = 2)
lines(nu, pwr+se, lty = 3)
lines(nu, pwr-se, lty = 3)


## -----------------------------------------------------------------------------
c5t <- function(x, y) {     #count five test
  outx <- sum((x-mean(x))>max((y-mean(y))))+sum((x-mean(x))<min((y-mean(y))))
  outy <- sum((y-mean(y))>max((x-mean(x))))+sum((y-mean(y))<min((x-mean(x))))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy))>5))
}
ft <- function(x, y){    #F test of equal variance
  f <- var.test(x,y)
  return(as.integer(f$p.value<0.05))
}

m <- 2000
n <- c(10,20,40,100)     #10, 20, 40 for small, medium, and large sample sizes
N <- length(n)
c5t.power <- numeric(N)
ft.power <- numeric(N)

# generate samples under H1 to estimate power
sigma1 <- 1
sigma2 <- 1.5
set.seed(2789)
for (i in 1:N) {
  k <- replicate(m, expr={
    x <- rnorm(20, 0, sigma1)
    y <- rnorm(20, 0, sigma2)
    c(ft(x, y),c5t(x,y))
  })
  ft.power[i] <- mean(k[1,])
  c5t.power[i] <- mean(k[2,])
}

p <- cbind(ft.power,c5t.power)
rownames(p) <- paste(rep('n=',3),n,sep='')
knitr::kable(p)

## -----------------------------------------------------------------------------
msk <- function(x) {    #computes the sample skewness coeff.
  hat.sigma <- cov(x)     #maximum likelihood estimator of covariance
  xbar <- apply(x,2,mean)  #Take the average of each column
  n <- nrow(x)
  b <- matrix(rep(0,n*n),ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      b[i,j] <- (t(as.matrix(x[i,]-xbar)) %*% solve(hat.sigma) %*% as.matrix(x[j,]-xbar))^3
    }
  }
  return(mean(b))
}

n <- c(10,30,50)  
#lack of the result of n=500 and n=300 which takes too much time to run the code
N <- length(n)
d <- 2   #The dimension
cv <- qchisq(0.95,d*(d+1)*(d+2)/6)       #crit. values 
#we are doing length(n) different simulations
p.reject <- numeric(N) #to store sim. results
m <- 3e3     #num. repl. each sim.

library(MASS)
mu <- c(0,0)
Sigma <- matrix(c(1,0.1,0.1,1),nrow = 2)

set.seed(4563)
for (i in 1:N) {
  sktests <- replicate(m,expr = {
    x <- mvrnorm(n[i],mu,Sigma)
    as.integer(n[i]*msk(x)/6 >= cv )
  })
  p.reject[i] <- mean(sktests) #proportion rejected
}

names(p.reject) <- paste(rep('n=',N),n,sep='')
knitr::kable(round(p.reject,5))

## -----------------------------------------------------------------------------
gen <- function(d,n,mu,sigma1,sigma2){
  x <- matrix(1:(d*n),ncol=d)
  sa <- sample(c(1, 2), replace = TRUE,size = n, prob = c(1-e, e))
  for (k in 1:n) {
    if(sa[k]==1) sigma <- sigma1 else sigma <- sigma2
    x[k,] <- mvrnorm(1,c(0,0),sigma)
  }
  return(x)
}

alpha <- 0.1
n <- 30
d <- 2
m <- 100
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv1 <- qchisq(alpha/2,d*(d+1)*(d+2)/6)
cv2 <- qchisq(1-alpha/2,d*(d+1)*(d+2)/6)

mu <- c(0,0)
sigma1 <- matrix(c(1,0,0,1),ncol=2)
sigma2 <- matrix(c(10,0,0,10),ncol=2)
library(MASS)

for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  sktests <- replicate(m,expr = {
    x <- gen(d,n,mu,sigma1,sigma2)
    as.integer(msk(x) >= cv2 | msk(x) <= cv1)
  })
  pwr[j] <- mean(sktests)
}
print(pwr)

plot(epsilon, pwr, type = "b",xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3,col = 2)


## -----------------------------------------------------------------------------
library(bootstrap)
# 15 law schools.
#jackknife estimate of bias and standard error 
n1 <- nrow(law)  #sample size
corr1.hat <- cor(law[,1], law[,2])
corr1.star <- numeric(n1)
for (i in 1:n1) {
  laww <- law[-i,]
  corr1.star[i] <- cor(laww[,1], laww[,2])
}
bias.law <- (n1-1)*(mean(corr1.star)-corr1.hat)
se.law <- (n1-1)*(sum((corr1.star-mean(corr1.star))^2))/n1
e.law <- c(bias.law,se.law)
names(e.law) <- c('bias','se')

# 82 law schools
#jackknife estimate of bias and standard error
n2 <- nrow(law82)  #sample size
corr2.hat <- cor(law82[,1], law82[,2])
corr2.star <- numeric(n2)
for (i in 1:n2) {
  laww82 <- law82[-i,]
  corr2.star[i] <- cor(laww82[,1], laww82[,2])
}
bias.law82 <- (n2-1)*(mean(corr2.star)-corr2.hat)
se.law82 <- (n2-1)*(sum((corr2.star-mean(corr2.star))^2))/n2
e.law82 <- c(bias.law82,se.law82)
names(e.law82) <- c('bias','se')

es <- rbind(e.law,e.law82)
knitr::kable(es)


## -----------------------------------------------------------------------------
library(boot)
air <- as.matrix(aircondit)
theta.mean <- mean(air)      #1/lambda

boot.lambda <- function(x,i) mean(x[i])
bs <- boot(data = air,statistic = boot.lambda,R = 1000)
ci <- boot.ci(bs,type=c("norm","basic","perc","bca"))

CI95 <- rbind(ci$normal[2:3],ci$basic[4:5],ci$percent[4:5],ci$bca[4:5])
colnames(CI95) <- c('L','U')
row.names(CI95) <- c('95% CI by the standard normal method:','95% CI by the basic method:','95% CI by the percentile method:','95% CI by the BCa method:')

knitr::kable(CI95)


## -----------------------------------------------------------------------------
library(bootstrap) #the data scor is in the package bootstrap
n <- nrow(scor)
lambda <- eigen(cov(scor))$values
theta.hat <- lambda[1]/sum(lambda)
   
theta.j <- numeric(n)
for (i in 1:n) {
  sscor <- scor[-i,]
  ssigma <- cov(sscor)
  lambda.j <- eigen(ssigma)$values
  theta.j[i] <- lambda.j[1]/sum(lambda.j)
}
theta.j.bias <- (n-1)*(mean(theta.j)-theta.hat)
theta.j.se <- sqrt((n-1)*var(theta.j)/n)

jack.p <- c(theta.j.bias,theta.j.se)
names(jack.p) <- c('bias','se')

print(jack.p)


## -----------------------------------------------------------------------------
library(DAAG)
chem <- ironslag$chemical    #变量名:x: chemical 
magn <- ironslag$magnetic    #变量名:y: magnetic

n <- length(magn)

# for n-fold cross validation
# fit models on leave-two-out samples
sa <- sample(rep(1:ceiling(n/2),2),n,replace = F) #将数据分成14类
e1 <- e2 <- e3 <- e4 <- numeric(ceiling(n/2))

for (i in 1:ceiling(n/2)) {
  x <- chem[sa!=i]
  y <- magn[sa!=i]
  x.test <- chem[sa==i]
  y.test <- magn[sa==i]
  
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * x.test
  e1[i] <- mean((y.test - yhat1)^2)
  
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * x.test +J2$coef[3] * x.test^2
  e2[i] <- mean((y.test - yhat2)^2)
  
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * x.test
  yhat3 <- exp(logyhat3)
  e3[i] <- mean((y.test - yhat3)^2)
  
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(x.test)
  yhat4 <- exp(logyhat4)
  e4[i] <- mean((y.test - yhat4)^2)
  
}

E <- c(mean(e1),mean(e2),mean(e3),mean(e4))
names(E) <- c('Linear','Quadratic','Exponential','Log-Log')
print(E)


## -----------------------------------------------------------------------------
maxn <- function(x, y) {
  #the maximum number of extreme points
  X <- x-mean(x)
  Y <- y-mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}

n1 <- 20
n2 <- 30
n <- n1+n2
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
set.seed(3432)
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
v <- maxn(x,y)
z <- c(x,y)
R <- 9999
#permutation test
perm <- replicate(R,expr = {
  sa <- sample(1:n, size = n1, replace = FALSE)
  x1 <- z[sa]-mean(z[sa])
  y1 <- z[-sa]-mean(z[-sa])
  maxn(x1,y1)
})
p <- mean(c(v,perm) >= v)
print(p)

## -----------------------------------------------------------------------------
library(RANN)    # For NN method
library(energy)  # For energy method
library(Ball)    # For Ball method
library(boot)

Tn <- function(z,ix,sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0)
  z <- z[ix,];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

## -----------------------------------------------------------------------------
n1 <- n2 <- 50
N <- c(n1,n2)
m <- 1e3
R <- 999
d <- 2
k <- 3
mu1 <- mu2 <- 0
sigma1 <- 1
sigma2 <- 0.7
p.values <- matrix(NA,m,3)

for(i in 1:m){
  x <- matrix(rnorm(n1*d,mu1,sigma1),ncol=d)
  y <- matrix(rnorm(n2*d,mu2,sigma2),ncol=d)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*1818)$p.value
}

alpha <- 0.1
pow1 <- colMeans(p.values<alpha)
names(pow1) <- c('p.NN','p.energy','p.Ball')
print(pow1)

## -----------------------------------------------------------------------------
n1 <- n2 <- 50
N <- c(n1,n2)
m <- 1e3
R <- 999
d <- 2
k <- 3
mu1 <- 0
mu2 <- 0.35
sigma1 <- 1
sigma2 <- 0.8
p.values <- matrix(NA,m,3)

for(i in 1:m){
  x <- matrix(rnorm(n1*d,mu1,sigma1),ncol=d)
  y <- matrix(rnorm(n2*d,mu2,sigma2),ncol=d)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=999)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*2626)$p.value
}

alpha <- 0.1
pow2 <- colMeans(p.values<alpha)
names(pow2) <- c('p.NN','p.energy','p.Ball')
print(pow2)

## -----------------------------------------------------------------------------
n1 <- n2 <- 50
N <- c(n1,n2)
m <- 1e3
R <- 999
d <- 2
k <- 3
mu1 <- 0
mu2 <- 0
sigma1 <- 2
sigma2 <- 1.2
p.values <- matrix(NA,m,3)
 #x:t distribution with 1 df
 #y:t bimodel distribution
for(i in 1:m){
  x <- matrix(rt(n1*d,df=1),ncol=d);
  y <- matrix(sample(c(rnorm(n2,mu1,sigma1),rnorm(n2,mu2,sigma2))),ncol=d)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*4535)$p.value
}

alpha <- 0.05
pow3 <- colMeans(p.values<alpha)
names(pow3) <- c('p.NN','p.energy','p.Ball')
print(pow3)

## -----------------------------------------------------------------------------
n1 <- 10
n2 <- 20
N <- c(n1,n2)
m <- 1e3
R <- 999
d <- 2
k <- 3
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
p.values <- matrix(NA,m,3)

for(i in 1:m){
  x <- matrix(rnorm(n1*d,mu1,sigma1),ncol=d)
  y <- matrix(rnorm(n2*d,mu2,sigma2),ncol=d)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*1234)$p.value
}

alpha <- 0.3
pow4 <- colMeans(p.values<alpha)
names(pow4) <- c('p.NN','p.energy','p.Ball')
print(pow4)

## -----------------------------------------------------------------------------
# a function to generate the chain, given the
# parameters n and σ, initial value X0, and the length of the chain, N.
rwM.gen.chain <- function(sigma,x0,N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    r <- exp(abs(x[i-1])-abs(y))
    if (u[i] <= r){
      x[i] <- y 
      k <- k + 1
      } else 
        x[i] <- x[i-1]
    }
  return(list(x=x,accept=k/N )) 
  # accept:the acceptance rates of chain
}

N <- 2000
sigma <- c(0.05,0.5,2,16)

x0 <- 5   # start = 5
ch1 <- rwM.gen.chain(sigma[1], x0, N)
ch2 <- rwM.gen.chain(sigma[2], x0, N)
ch3 <- rwM.gen.chain(sigma[3], x0, N)
ch4 <- rwM.gen.chain(sigma[4], x0, N)

accep <- c(ch1$accept, ch2$accept, ch3$accept, ch4$accept)
names(accep) <- c('sigma=0.05','sigma=0.5','sigma=2','sigma=16')
print(accep)

plot(ch1$x,type ='l',main ='sigma = 0.05')
plot(ch2$x,type ='l',main ='sigma = 0.5')
plot(ch3$x,type ='l',main ='sigma = 2')
plot(ch4$x,type ='l',main ='sigma = 16')


## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j]) for chain in i-th row of X
  # generate r.hat
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.i <- rowMeans(psi)            #row means
  B <- n*var(psi.i)                 #between variance est.
  psi.w <- apply(psi, 1, "var")     #within variances
  W <- mean(psi.w)                  #within est.
  hat.v.psi <- W*(n-1)/n + (B/n)    #upper variance est.
  r.hat <- hat.v.psi/W              #G-R statistic
  return(r.hat)
}

#use the function 'rwM.gen.chain' in answer 1 to generate the chain

sigma <- 2          #parameter of proposal distribution
k <- 4              #number of chains to generate
n <- 15000          #length of chains
b <- 1000           #burn-in length

#choose overdispersed initial values
x0 <- c(-10,-5,5,10)

#generate the chains
X <- matrix(0,nrow=k,ncol=n)
for (i in 1:k) X[i,] <- rwM.gen.chain(sigma,x0[i],n)$x

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))

for (i in 1:nrow(psi)) psi[i,] <- psi[i,]/(1:ncol(psi))
r <- Gelman.Rubin(psi)
cat('r.hat=',r)

#plot psi for the four chains

for (i in 1:k) plot(psi[i, (b+1):n], type="l",xlab=i, ylab=bquote(psi))
    #restore default

#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n) rhat[j] <- Gelman.Rubin(psi[,1:j])

plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)


## -----------------------------------------------------------------------------
sigma <- c(0.05,0.5,2,4)
k <- 4              #number of chains to generate
n <- 15000          #length of chains
b <- 1000 

x0 <- c(-10,-5,5,10)
X1 <- X2 <- X3 <- X4 <- matrix(0,nrow=k,ncol=n)
for (i in 1:k){
  X1[i,] <- rwM.gen.chain(sigma[1],x0[i],n)$x
  X2[i,] <- rwM.gen.chain(sigma[2],x0[i],n)$x
  X3[i,] <- rwM.gen.chain(sigma[3],x0[i],n)$x
  X4[i,] <- rwM.gen.chain(sigma[4],x0[i],n)$x
}
psi1 <- t(apply(X1,1,cumsum))
psi2 <- t(apply(X2,1,cumsum))
psi3 <- t(apply(X3,1,cumsum))
psi4 <- t(apply(X4,1,cumsum))

for (i in 1:nrow(psi1)){
  psi1[i,] <- psi1[i,]/(1:ncol(psi1))
  psi2[i,] <- psi2[i,]/(1:ncol(psi2))
  psi3[i,] <- psi3[i,]/(1:ncol(psi3))
  psi4[i,] <- psi4[i,]/(1:ncol(psi4))
}

rhat1 <- rhat2 <- rhat3 <- rhat4 <-rep(0, n)
for (j in (b+1):n){
  rhat1[j] <- Gelman.Rubin(psi1[,1:j])
  rhat2[j] <- Gelman.Rubin(psi2[,1:j])
  rhat3[j] <- Gelman.Rubin(psi3[,1:j])
  rhat4[j] <- Gelman.Rubin(psi4[,1:j])
}

plot(rhat1[(b+1):n], type="l", xlab="", ylab="R1")
abline(h=1.2, lty=2)
plot(rhat2[(b+1):n], type="l", xlab="", ylab="R2")
abline(h=1.2, lty=2)
plot(rhat3[(b+1):n], type="l", xlab="", ylab="R3")
abline(h=1.2, lty=2)
plot(rhat4[(b+1):n], type="l", xlab="", ylab="R4")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
 #the function of s_k(a)
sk <- function(a,k) 1-pt(sqrt(a^2*k/(k+1-a^2)),k)
 #the equation to solve the intersection points
equa <- function(a,k) sk(a,k)-sk(a,k-1) 
 #the interval that the intersection points in is (0,sqrt(k))

k = c(4:25,100,500,1000)

n <- length(k)
Ak <- numeric(n)
for (i in 1:n) {
  Ak[i] <- uniroot(function(y) {equa(y,k[i])},lower = 0.00001,upper = sqrt(k[i])-0.00001)$root
}
names(Ak) <- paste(rep('k=',n),k,sep='')

knitr::kable(Ak)

## -----------------------------------------------------------------------------
obl <- function(x){
  # Observed data likelihood
  p <- x[1]
  q <- x[2]
  r <- 1-p-q
  f <- 444*log(p^2+2*p*r) + 132*log(q^2+2*q*r) + 2*361*log(r) + 63*(log(2)+log(p)+log(q))
  return(-f)
}

e_step <- function(x,phat,qhat,rhat){
  p <- x[1]
  q <- x[2]
  r <- 1-p-q
  f <- phat*444*(log(p)-log(2*r))/(phat+2*rhat) + 444*(log(2)+log(p)+log(r)) + 
    qhat*132*(log(q)-log(2*r))/(qhat+2*rhat) + 132*(log(2)+log(q)+log(r)) + 
    2*361*log(r) +63*(log(2)+log(p)+log(q))
  return(-f)
}
theta0 <- optim(c(0.35,0.2),fn=obl)$par
theta <- list()   # a list to store phat and qhat
theta[[1]] <- theta1 <- theta0
k <- 1
judge <- T
while (judge == T) {
  theta0 <- theta1
  p.hat <- theta0[1]
  q.hat <- theta0[2]
  r.hat <- 1-p.hat-q.hat
  com.likeli <- function(x) e_step(x,p.hat,q.hat,r.hat) #E-step
  theta1 <- optim(theta0,fn=com.likeli)$par #M-step
   # judge whether to stop the iteration:if judge=T,go on
  judge <- abs(theta0[1]-theta1[1])>1e-8 | abs(theta0[2]-theta1[2])>1e-8
  k <- k+1
  theta[[k]] <- theta1
}
prtheta <- matrix(unlist(theta),byrow=T,ncol=2)
colnames(prtheta) <- c('phat','qhat')
print(prtheta)
corllv <- -sapply(theta, obl)
cat('the corresponding log-maximum likelihood values are not increasing:','\n',corllv)

## -----------------------------------------------------------------------------
attach(mtcars)
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
 #使用for循环
n <- length(formulas)
fit1 <- list()
for (i in 1:n) {
  fit1[[i]] <- lm(formula = formulas[[i]])
}
 #使用lapply()
fit2 <- lapply(formulas,lm)
detach(mtcars)

print(fit1)
print(fit2)

## -----------------------------------------------------------------------------
trials <- replicate(100,t.test(rpois(10, 10), rpois(7, 10)),simplify = FALSE)

 # Use sapply() and an anonymous function
p_value1 <- sapply(trials,function(x) x$p.value)
print(p_value1)
 # using [[ directly
p_value2 <- numeric()
for (i in 1:length(trials)) {
  p_value2[i] <- trials[[i]]$p.value
}
print(p_value2)


## -----------------------------------------------------------------------------
 # lapply() variant
valapply <- function(X,FUN,FUN.VALUE,USE.NAMES = TRUE){
  FUN <- match.fun(FUN)
  answer <- Map(FUN,X)
  vapply(answer, function(x) x, FUN.VALUE = FUN.VALUE)
}

#example:
a <- list(c(1,2),c(2,3),c(1,4),c(5,5))
 # in a vector
valapply(a,mean,numeric(1)) 
 # in a matrix
valapply(a,function(x) x^2,numeric(2)) 
 # in an array
valapply(a,function(x) x %*% t(x),matrix(0,2,2))  


## ----eval=FALSE---------------------------------------------------------------
#  List rw_MetropolisC(double sigma,double x0,int N){
#    NumericVector x(N);
#    x[0]=x0;
#    NumericVector u(N);
#    u=runif(N);
#    double y;
#    int k = 0;
#    for (int i=1;i<N;i++) {
#      y = rnorm(1,x[i-1],sigma);
#      if (u[i] <= exp(abs(x[i-1])-abs(y))){
#        x[i] = y;
#        k = k+1;
#      }
#      else {
#        x[i] = x[i-1];
#      }
#    }
#    List out(2);
#    out[0] = x;
#    out[1] = k;
#    return out;
#  }

## -----------------------------------------------------------------------------
library(Rcpp) 
cppFunction('List rw_MetropolisC(double sigma,double x0,int N){
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runif(N);
  double y;
  int k = 0;
  for (int i=1;i<N;i++) {
    y = rnorm(1,x[i-1],sigma)[0];
    if (u[i] <= exp(abs(x[i-1])-abs(y))){
      x[i] = y;
      k = k+1;
    }
    else {
      x[i] = x[i-1];
    }
  }
  List out(2);
  out[0] = x;
  out[1] = k;
  return out;
  }'
)


N = 2000
sigma = c(0.05, 0.5, 2, 16)
x0 = 20
ch1.C = rw_MetropolisC(sigma[1],x0,N)
ch2.C = rw_MetropolisC(sigma[2],x0,N)
ch3.C = rw_MetropolisC(sigma[3],x0,N)
ch4.C = rw_MetropolisC(sigma[4],x0,N)

accep.C = cbind(ch1.C[[2]], ch2.C[[2]], ch3.C[[2]], ch4.C[[2]])/N
rownames(accep.C) = 'Accept rates'
colnames(accep.C) = paste('sigma=',sigma)
knitr::kable(accep.C)
    

plot(ch1.C[[1]],type ='l', main = 'sigma = 0.05')
plot(ch2.C[[1]],type ='l', main = 'sigma = 0.5')
plot(ch3.C[[1]],type ='l', main = 'sigma = 2')
plot(ch4.C[[1]],type ='l', main = 'sigma = 16')


## ----eval=FALSE---------------------------------------------------------------
#  rwM.gen.chain <- function(sigma,x0,N) {
#    x <- numeric(N)
#    x[1] <- x0
#    u <- runif(N)
#    k <- 0
#    for (i in 2:N) {
#      y <- rnorm(1, x[i-1], sigma)
#      r <- exp(abs(x[i-1])-abs(y))
#      if (u[i] <= r){
#        x[i] <- y
#        k <- k + 1
#      } else
#        x[i] <- x[i-1]
#    }
#    return(list(x=x,accept=k/N ))
#    # accept:the acceptance rates of chain
#  }

## -----------------------------------------------------------------------------

ch1 <- rwM.gen.chain(sigma[1], x0, N)
ch2 <- rwM.gen.chain(sigma[2], x0, N)
ch3 <- rwM.gen.chain(sigma[3], x0, N)
ch4 <- rwM.gen.chain(sigma[4], x0, N)

accep <- c(ch1$accept, ch2$accept, ch3$accept, ch4$accept)
names(accep) <- c('sigma=0.05','sigma=0.5','sigma=2','sigma=16')
print(accep)

index <- 1:N

plot(index,ch1$x,type ='l', main = 'sigma = 0.05')
plot(index,ch2$x,type ='l', main = 'sigma = 0.5')
plot(index,ch3$x,type ='l', main = 'sigma = 2')
plot(index,ch4$x,type ='l', main = 'sigma = 16')


## -----------------------------------------------------------------------------
x <- rbind(ch1$x,ch2$x,ch3$x,ch4$x)
y <- rbind(ch1.C[[1]],ch2.C[[1]],ch3.C[[1]],ch4.C[[1]])

for (i in 1:4) {
  qqplot(x[i,],y[i,],xlab = 'using R',
         ylab = 'using C',main=paste('sigma=',sigma[i]))
  qqline(y[i,])
}


## -----------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(ch1=rwM.gen.chain(sigma[1], x0, N),
               ch1.C=rw_MetropolisC(sigma[1],x0,N))
microbenchmark(ch2=rwM.gen.chain(sigma[2], x0, N),
               ch2.C=rw_MetropolisC(sigma[2],x0,N))
microbenchmark(ch3=rwM.gen.chain(sigma[3], x0, N),
               ch3.C=rw_MetropolisC(sigma[3],x0,N))
microbenchmark(ch4=rwM.gen.chain(sigma[4], x0, N),
               ch4.C=rw_MetropolisC(sigma[4],x0,N))


## ----results='asis',echo=FALSE------------------------------------------------
knitr::kable(tail(mtcars))

## ----results='asis',echo=FALSE------------------------------------------------
knitr::kable(iris[10:30,])

## ----echo=FALSE---------------------------------------------------------------
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14) 
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69) 
group <- gl(2, 10, 20, labels = c("Ctl","Trt")) 
weight <- c(ctl, trt) 
lm.D9 <- lm(weight ~ group)
par(mfrow=c(2,2))
win.graph(width=4.875, height=2.5,pointsize=8)
plot(lm.D9)

## -----------------------------------------------------------------------------
n<-1e4
k<-0;j<-0
y<-numeric(n)
while(k<n)
{x<-2/runif(1)^0.5;j<-j+1
if(x<24){k<-k+1;y[k]<-x}
}
hist(y,breaks=seq(0,24,2),prob=TRUE,main=expression(f(x)==8/x^3))
lines(z<-seq(0,24,0.1),8/z^3)

## -----------------------------------------------------------------------------
k<-0
n<-1e4
e<-numeric(n)
while(k<n){
  u1<-runif(1,-1,1)
  u2<-runif(1,-1,1)
  u3<-runif(1,-1,1)
  if(abs(u3)>=abs(u2)
     &
     abs(u3)>=abs(u1)){
    k<-k+1
    e[k]<-u2
  }
  else{
    k<-k+1
    e[k]<-u3
  }
}
hist(e,prob=TRUE,main=expression(f(x)==3/4*(1-x^2)))
c<-seq(-1,1,0.1)
lines(c,3/4*(1-c^2))

## -----------------------------------------------------------------------------
k<-0
n<-1e4
y<-numeric(n)
while(k<n){
  x<-2/runif(1)^0.25-2
  if(x<10){k<-k+1;y[k]<-x}
}
hist(y,prob=TRUE)
d<-seq(0,10,0.1)
lines(d,64/(2+d)^5)

## -----------------------------------------------------------------------------
set.seed(1)
u<-runif(1e4,0,pi/3)
y<-pi/3*sin(u)
mean(y)

## -----------------------------------------------------------------------------
MC_modify_variance<- function(x, R = 10000, antithetic = TRUE) {
  u <- runif(R/2,0,x)
  if (!antithetic) v <- runif(R/2,0,x) else v <- 1 - u
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] * exp(u * x[i])
    cdf[i] <- mean(g)
  }
  cdf
}
m <- 1000
theta_1 <- theta_2 <- numeric(m)
x <- 1
for (i in 1:m) {
  theta_1[i] <- MC_modify_variance(x, R = 1000, anti = FALSE)
  theta_2[i] <- MC_modify_variance(x, R = 1000)
}
mean(theta_2)
mean(theta_1)
c(sd(theta_1),sd(theta_2),sd(theta_2)/sd(theta_1))

## -----------------------------------------------------------------------------
n<-1e4
set.seed(1)
u<-runif(n)
x_1<-(1-2*log(u))^0.5
int_1<- 1/(exp(0.5)*sqrt(2*pi))*mean(x_1)
var_int_1<- 1/(exp(1)*2*pi*n)*var(x_1)
sd_int_1<- 1/(exp(0.5)*sqrt(2*pi*n))*sd(x_1)
round(print(c(int_1,var_int_1,sd_int_1)),8)

## -----------------------------------------------------------------------------
n<-1e4
set.seed(1)
u<-runif(n)
x_2<- 1/u
t<- x_2^4*exp(-0.5*x_2^2)/sqrt(2*pi)
int_2<- mean(t)
var_int_2<- var(t)/n
sd_int_2<- sqrt(var_int_2)
round(print(c(int_2,var_int_2,sd_int_2)),8)

## -----------------------------------------------------------------------------
a<-seq(1,5,0.01)
g<- a^2/sqrt(exp(a^2)*2*pi)
f_1<- a/sqrt(exp(a^2)*2*pi)
f_2<- 1/a^2
plot(a,g,type = "l",col=1)
lines(a,f_1,col=2)
lines(a,f_2,col=3)

## -----------------------------------------------------------------------------
m<-5;n<-1e4
k<-numeric(m);theta_i<-numeric(m);var_i<-numeric(m)
y<-x<-matrix(numeric(5*n),ncol=5,nrow=n)
set.seed(1)
u<-runif(n)
for(i in 0:4){
  k[i+1]<-(exp(-i/5)-exp(-(i+1)/5))^(-1)
  x[,i+1]<- -log(exp(-i/5)-u/k[i+1])
  y[,i+1]<- (1+x[,i+1]^2)^(-1)
  theta_i[i+1]<- mean(y[,i+1])/k[i+1]
  var_i[i+1]<- var(y[,i+1])/(n*k[i+1]^2)
}
hat_theta<- sum(theta_i)
var_hat_theta<-sum(var_i)
sd_hat_theta<-sqrt(var_hat_theta)
print(round(c(hat_theta,var_hat_theta,sd_hat_theta),8))


## -----------------------------------------------------------------------------
mu<-0;sigma<-1;m<-1e4;n1<-10;n2<-100
y_1<-numeric(m);y_2<-numeric(m)
sd_1<-numeric(m);sd_2<-numeric(m)
t_0.025_9<-2.262;z_0.025<-1.96
# first case:t distribution
for(i in 1:m){
  a<-rt(n1,n1-1)
  y_1[i]<-mean(a)
  sd_1[i]<-sd(a)
}
t_theta_1<- y_1-sd_1*t_0.025_9/sqrt(n1)
t_theta_2<- y_1+sd_1*t_0.025_9/sqrt(n1)
d1<-0
for(i in 1:m){
  if(t_theta_1[i]<=mu & mu<=t_theta_2[i]){
    d1<- d1+1
  }
}
print(c(mean(t_theta_1),mean(t_theta_2))) #confidence interval
print(d1/m) #empirical coverage probability

# second case:normal distribution
for(i in 1:m){
  b<-rnorm(n2)
  y_2[i]<-mean(b)
  sd_2[i]<-sd(b)
}
n_theta_1<-y_2-sd_2*z_0.025/sqrt(n2)
n_theta_2<-y_2+sd_2*z_0.025/sqrt(n2)
d2<-0
for(i in 1:m){
  if(n_theta_1[i]<=mu & mu<=n_theta_2[i]){
    d2<- d2+1
  }
}
print(c(mean(n_theta_1),mean(n_theta_2))) #confidence interval
print(d2/m) #empirical coverage probability

## -----------------------------------------------------------------------------
m<- 1e4;n<-20;sd<-numeric(m);y<-numeric(m);t_0.025_19<- 2.093
for(i in 1:m){
  a<- rchisq(n,2)
  y[i]<- mean(a)
  sd[i]<- sd(a)
}
theta_1<- y-sd*t_0.025_19/sqrt(n)
theta_2<- y+sd*t_0.025_19/sqrt(n)
d<-0
for(i in 1:m){
  if(theta_1[i]<=2 & 2<=theta_2[i]){
    d<- d+1
  }
}# the mean of chis-quared is 2
print(d/m)

## -----------------------------------------------------------------------------
alpha<-0.05 
m<-1e4 
a<-seq(1,241,4) 
n<-length(a)
cv <- qnorm(0.975, 0, sqrt(6*(n-2) / ((n+1)*(n+3)))) 
eop<-numeric(length(a)) 
eop1<-numeric(length(a))
sk <- function(x) {
  #computes the sample skewness coeff.
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}
for(j in 1:n){
  sr<-numeric(m)
  sr1<-numeric(m)
  for(i in 1:m){
    x<-rbeta(n,a[j],a[j])
    x1<-rt(n,a[j])
    sr[i] <- as.integer(abs(sk(x)) > cv)
    sr1[i]<-as.integer(abs(sk(x1))>cv)
  }
  eop[j]<-mean(sr)
  eop1[j]<-mean(sr1)
}
eop
eop1
plot(a, eop, type = "b",ylim=c(0,1))
points(a,eop1,type="b")
lines(eop,col="DarkTurquoise",lty=1)
lines(eop1,col="DeepPink",lty=2)
print(d<-data.frame(eop,eop1))

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}
m<-1e4
sigma1 <- 1
sigma2 <- 1.5
n<-c(20,35,60) 
power<-numeric(length(n));power_f<-numeric(length(n))
for(i in 1:length(n)){
  power[i] <- mean(replicate(m, expr={
    x <- rnorm(n[i], 0, sigma1)
    y <- rnorm(n[i], 0, sigma2)
    count5test(x, y)
  }))# power of  count five test
  pvalues <- replicate(m, expr = {
    x <- rnorm(n[i], 0, sigma1)
    y <- rnorm(n[i], 0, sigma2)
    ftest <- var.test(x,y)
    ftest$p.value } )
  power_f[i]<-mean(pvalues<=0.055)# power of f test
}
k<-data.frame(power,power_f,row.names=c("small sample 20","medium sample 35","big sample 60"))
print(k)

## -----------------------------------------------------------------------------
cs<-function(data.frame){
  xbar<-numeric()
  for(k in 1:ncol(data.frame)){
    xbar[k]<-mean(data.frame[,k])
  }
  isum<-numeric()
  for(i in 1:nrow(data.frame)){
    jsum<-numeric()
    for(j in 1:nrow(data.frame)){
      jsum[j]<-(as.matrix(data.frame[i,])%*%solve(cov(data.frame))%*%as.matrix(t(data.frame[j,])))^3
    }
    isum[i]<-sum(jsum)
  }
  b<-sum(isum)/nrow(data.frame)^2
  return(b)
}

## -----------------------------------------------------------------------------
#example 6.8 for multivariate
n<-c(30,50)
alpha<-0.05
d<-3
m<-100
cv<-qchisq(1-alpha,d*(d+1)*(d+2)/6)
p_reject<-numeric(length(n))
for(i in 1:length(n)){
  tr<-numeric(m)
  for(j in 1:m){
    x1<-rnorm(n[i])
    x2<-rnorm(n[i])
    x3<-rnorm(n[i])
    s<-data.frame(x1,x2,x3)
    tr[j]<-as.integer(n[i]*cs(s)/6>=cv)
  }
  p_reject[i]<-mean(tr)
}
print(p_reject)

## -----------------------------------------------------------------------------
#example 6.10 for multivriate
epsilon <- c(0.5,0.75)
alpha <- 0.1
n <- 30
m <- 100
d<-3
pw<-numeric(length(epsilon))
cv<-qchisq(1-alpha,d*(d+1)*(d+2)/6)
for(i in 1:length(epsilon)){
  tr<-numeric(m)
  for(j in 1:m){
    sigma <- sample(c(1, 10), replace = TRUE,
                    size = n, prob = c(1-epsilon[i], epsilon[i]))
    x1 <- rnorm(n, 0, sigma)
    x2 <- rnorm(n, 0, sigma)
    x3 <- rnorm(n, 0, sigma)
    s<-data.frame(x1,x2,x3)
    tr[j]<-as.integer(n*cs(s)/6>=cv)
  }
  pw[i]<-mean(tr)
}
print(pw)

## -----------------------------------------------------------------------------
#jackknife estimate of the bias of the correlation statistic in Example 7.2.
LSAT<-c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
GPA<-c(339, 330, 281, 303, 344, 307, 300, 343, 336, 313, 312, 274, 276, 288, 296)
cor_jack<-numeric(length(LSAT))#generate a container of values
for(i in 1:length(LSAT)){
  cor_jack[i]<-cor(LSAT[-i],GPA[-i])
}#compute the jackknife replicates, leave-one-out estimates
bais_jack<-(length(LSAT)-1)*(mean(cor_jack)-cor(LSAT,GPA))
print(bais_jack)
#jackknife estimate of the standard error of the correlation statistic in Example 7.2.
se <- sqrt((length(LSAT)-1) *mean((cor_jack-mean(cor_jack))^2))
print(se)

## -----------------------------------------------------------------------------
library(boot)
time<-c(3,5,7,18,43,85,91,98,100,130,230,487)
boot_obj_1 <- boot(time, R = 2000,
                 statistic = function(x, i){return(mean(x[i]))})
print(boot.ci(boot_obj_1, type=c("basic","norm","perc","bca")))

## -----------------------------------------------------------------------------
library(bootstrap)
data(scor, package = "bootstrap")
a<-eigen(cov(scor))
ht<-a$values[1]/sum(a$values)
ht_jack<-numeric(nrow(scor))
for(i in 1:nrow(scor)){
  ht_jack[i]<-eigen(cov(scor[-i,]))$values[1]/sum(eigen(cov(scor[-i,]))$values)
}
bias_jack<-(nrow(scor)-1)*(mean(ht_jack)-ht)
se_jack<-sqrt((nrow(scor)-1)*mean((ht_jack-mean(ht_jack))^2))
print(c(bias_jack,se_jack))

## -----------------------------------------------------------------------------
library("DAAG")
data(ironslag,package="DAAG")
magnetic<-ironslag$magnetic
chemical<-ironslag$chemical
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)
# fit models on leave-two-out samples
for (k in 1:n-1) {
  y <- magnetic[-c(k,k+1)]
  x <- chemical[-c(k,k+1)]
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[c(k,k+1)]
  e1[k] <- abs(magnetic[k] - yhat1[1])+abs(magnetic[k+1]-yhat1[2])
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[c(k,k+1)] +
    J2$coef[3] * chemical[c(k,k+1)]^2
  e2[k] <- abs(magnetic[k] - yhat2[1])+abs(magnetic[k+1]-yhat1[2])
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[c(k,k+1)]
  yhat3 <- exp(logyhat3)
  e3[k] <- abs(magnetic[k] - yhat3[1])+abs(magnetic[k+1]-yhat1[2])
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[c(k,k+1)])
  yhat4 <- exp(logyhat4)
  e4[k] <- abs(magnetic[k] - yhat4[1])+abs(magnetic[k+1]-yhat1[2])
}
y_1<-magnetic[-c(1,n)]
x_1<-chemical[-c(1,n)]
J1_1<-lm(y_1~x_1)
yhat1_1<-J1_1$coef[1] + J1_1$coef[2] * chemical[c(1,n)]
e1[n]<-abs(magnetic[1]-yhat1_1[1])+abs(magnetic[n]-yhat1_1[2])
J2_1<-lm(y_1~x_1+I(x_1^2))
yhat2_1<-J2_1$coef[1] + J2_1$coef[2] * chemical[c(1,n)] +
  J2_1$coef[3] * chemical[c(1,n)]^2
e2[n]<-abs(magnetic[1]-yhat2_1[1])+abs(magnetic[n]-yhat2_1[2])
J3_1 <- lm(log(y_1) ~ x_1)
logyhat3_1 <- J3_1$coef[1] + J3_1$coef[2] * chemical[c(1,n)]
yhat3_1 <- exp(logyhat3_1)
e3[n]<-abs(magnetic[1]-yhat3_1[1])+abs(magnetic[n]-yhat3_1[2])
J4_1 <- lm(log(y_1) ~ log(x_1))
logyhat4_1 <- J4_1$coef[1] + J4_1$coef[2] * log(chemical[c(1,n)])
yhat4_1 <- exp(logyhat4_1)
e4[n] <- abs(magnetic[1]-yhat4_1[1])+abs(magnetic[n]-yhat4_1[2])
print(c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2)))

## -----------------------------------------------------------------------------
print(L2 <- lm(magnetic ~ chemical + I(chemical^2)))

## -----------------------------------------------------------------------------
m<-999;n1<- 10;n2<- 15
set.seed(12345)
x1<-rnorm(n1);x2<-rnorm(n2)
z<-c(x1,x2)
out1 <- sum(x1 > max(x2)) + sum(x1 < min(x2))
out2 <- sum(x2 > max(x1)) + sum(x2 < min(x1))
t0<-max(c(out1, out2))
d<-numeric(m)
for(i in 1:m){
  q<-sample(1:25,size=10,replace=FALSE)
  y1<-z[q]
  y2<-z[-q]
  out_1 <- sum(y1 > max(y2)) + sum(y1 < min(y2))
  out_2 <- sum(y2 > max(y1)) + sum(y2 < min(y1))
  d[i]<- max(c(out_1, out_2))
}
p<-mean(c(t0,d)>5)
print(p)

## -----------------------------------------------------------------------------
library(boot);library(RANN);library(energy);library(Ball)
set.seed(1);n<-50
p.value_nn<-p.value_energy<-p.value_ball<-numeric(5)# the container of p.value
Tn3 <- function(z, ix, sizes) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  o <- rep(0, length(z))
  z <- as.data.frame(cbind(z, o))
  z <- z[ix, ]
  NN <- nn2(z, k=4)
  block1 <- NN$nn.idx[1:n1, ]
  block2 <- NN$nn.idx[(n1+1):n, ]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  return((i1 + i2) / (4 * n))
}
## for question 1
x1<-rnorm(n,0,1);y1<-rnorm(n,0,2)#Unequal variances and equal expectations
x <- as.vector(x1)
y <- as.vector(y1)
z <- c(x, y)
# as for nn
N <- c(50,50)
boot.obj <- boot(data = z, statistic = Tn3,
                 sim = "permutation", R = 999, sizes = N)
tb <- c(boot.obj$t, boot.obj$t0)
p.value_nn[1]<-mean(tb >= boot.obj$t0)
# as for energy
boot.obs <- eqdist.etest(z, sizes=N, R=999)
p.value_energy[1] <- boot.obs$p.value
# as for ball
obq <- bd.test(x = x, y = y, R=999)
p.value_ball[1]<-obq$p.value
## for question 2
x2<-rnorm(n,1,1);y2<-rnorm(n,0,2)#Unequal variances and expectations
x <- as.vector(x2)
y <- as.vector(y2)
z <- c(x, y)
# as for nn
N <- c(50,50)
boot.obj <- boot(data = z, statistic = Tn3,
                 sim = "permutation", R = 999, sizes = N)
tb <- c(boot.obj$t, boot.obj$t0)
p.value_nn[2]<-mean(tb >= boot.obj$t0)
# as for energy
boot.obs <- eqdist.etest(z, sizes=N, R=999)
p.value_energy[2] <- boot.obs$p.value
# as for ball
obq <- bd.test(x = x, y = y, R=999)
p.value_ball[2]<-obq$p.value
## for the first part of question 3
x3_1<-rt(n,1);y3_1<-rt(n,1)#t distribution with 1 df
x <- as.vector(x3_1)
y <- as.vector(y3_1)
z <- c(x, y)
# as for nn
N <- c(50,50)
boot.obj <- boot(data = z, statistic = Tn3,
                 sim = "permutation", R = 999, sizes = N)
tb <- c(boot.obj$t, boot.obj$t0)
p.value_nn[3]<-mean(tb >= boot.obj$t0)
# as for energy
boot.obs <- eqdist.etest(z, sizes=N, R=999)
p.value_energy[3] <- boot.obs$p.value
# as for ball
obq <- bd.test(x = x, y = y, R=999)
p.value_ball[3]<-obq$p.value
## for the second part of question 3
#suppose the mixture of two normal distributions is 0.2*N(1,1)+0.8*N(0,2)
mean<-sample(c(0,1),replace=TRUE,size=n,prob=c(0.8,0.2))
sigma<-sample(c(1,2),replace=TRUE,size=n,prob=c(0.2,0.8))
x3_2<-rnorm(n,mean,sigma);y3_2<-rnorm(n,mean,sigma)#mixture of two normal distributions
x <- as.vector(x3_2)
y <- as.vector(y3_2)
z <- c(x, y)
# as for nn
N <- c(50,50)
boot.obj <- boot(data = z, statistic = Tn3,
                 sim = "permutation", R = 999, sizes = N)
tb <- c(boot.obj$t, boot.obj$t0)
p.value_nn[4]<-mean(tb >= boot.obj$t0)
# as for energy
boot.obs <- eqdist.etest(z, sizes=N, R=999)
p.value_energy[4] <- boot.obs$p.value
# as for ball
obq <- bd.test(x = x, y = y, R=999)
p.value_ball[4]<-obq$p.value
## for question 4
x4<-rnorm(n);y4<-rnorm(n-10)#Unbalanced samples
x <- as.vector(x4)
y <- as.vector(y4)
z <- c(x, y)
# as for nn
N <- c(50,40)
boot.obj <- boot(data = z, statistic = Tn3,
                 sim = "permutation", R = 999, sizes = N)
tb <- c(boot.obj$t, boot.obj$t0)
p.value_nn[5]<-mean(tb >= boot.obj$t0)
# as for energy
boot.obs <- eqdist.etest(z, sizes=N, R=999)
p.value_energy[5] <- boot.obs$p.value
# as for ball
obq <- bd.test(x = x, y = y, R=999)
p.value_ball[5]<-obq$p.value
# to sum up
condition<-c("Unequal variances","Unequal variances and mean","t distribution with 1 df"
     ,"mixture of two normal distributions","Unbalanced samples")
s<-data.frame(condition,p.value_nn,p.value_energy,p.value_ball)
print(s)

## -----------------------------------------------------------------------------
rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(abs(x[i-1])-abs(y)))
      x[i] <- y 
    else {
        x[i] <- x[i-1]
        k <- k + 1
      } }
  return(list(x=x, k=k))
}
N <- 2000 #the length of chain
sigma <- c(0.05, 0.5, 2, 16) # parameters of normal distribution
x0<-1
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)
#number of candidate points rejected
print(c(rw1$k/N, rw2$k/N, rw3$k/N, rw4$k/N)) # k/N denotes rejection rate.
par(mfrow=c(2,2))
win.graph(width=4.875, height=2.5,pointsize=8)
plot(rw1$x,type="l",main="",xlab="sigma=0.05",ylab = "x1")
plot(rw2$x,type="l",main="",xlab="sigma=0.5",ylab = "x2")
plot(rw3$x,type="l",main="",xlab="sigma=2",ylab = "x3")
plot(rw4$x,type="l",main="",xlab="sigma=16",ylab = "x4")

## -----------------------------------------------------------------------------
# the first step:generate laplace chains
dn<-function(x,y){
  z<-exp(abs(x)-abs(y))
  return(z)
}#
laplace.chain <- function(sigma, N, X1) {
  #Implement a random walk Metropolis sampler for generating the
  #standard Laplace distribution
  #with Normal(X[t], sigma) proposal distribution
  #and starting value X1
  x <- numeric(N)
  x[1] <- X1
  u <- runif(N)
  for (i in 2:N) {
    xt <- x[i-1]
    y <- rnorm(1, xt, sigma) #candidate point
    if (u[i] <= dn(x[i-1],y))
      x[i] <- y 
    else {
      x[i] <- x[i-1]
    }
  }
  return(x)
}
# the second step:compute Gelman.Rubin statistic(grs)
# The scalar summary statistic is the
# mean of the ith chain up to time j.
grs <- function(psi) {
  # psi[i,j] is the sum of X[i,1:j]
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}
# the third step:set relevant parameter
sigma <- 2 #parameter of proposal distribution
#according to the exercise above,there is the best performance
#when sigma is equal to 2.
k <- 4 #number of chains to generate
n <- 15000 #length of chains
b <- 1000 #burn-in length
#choose overdispersed initial values
x0 <- c(-10, -5, 5, 10)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- laplace.chain(sigma, n, x0[i])
#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(grs(psi))
#plot psi for the four chains
par(mfrow=c(2,2))
win.graph(width=4.875, height=2.5,pointsize=8)
for (i in 1:k)
  plot(psi[i, (b+1):n], type="l",
       xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1),pin=c(3,3)) #restore default
win.graph(width=4.875, height=2.5,pointsize=8)
#plot the sequence of R-hat statistics
rhat <- numeric(n)
for (j in (b+1):n)
  rhat[j] <- grs(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)
abline(h=1.2,lty=2)

## -----------------------------------------------------------------------------
k<-c(4:25,100,500,1000)
s<-numeric(length(k))
library(rootSolve)
# the third exercise
for(i in 1:length(k)){
  f<-function(a){
    y1<-sqrt(a^2*k[i]/(k[i]+1-a^2))
    y2<-sqrt(a^2*(k[i]-1)/(k[i]-a^2))
    a1<-pt(y1,k[i],log.p = FALSE)
    a2<-pt(y2,k[i]-1,log.p=FALSE)
    return(a1-a2)
  }
  s[i]<- uniroot(f,c(1e-3,sqrt(k[i]-1e-3)))$root
}
print(s)

## -----------------------------------------------------------------------------
na<-444;nb<-132;nab<-63;noo<-361
n<-na+nb+nab+noo
pf<-function(p,q){
  y<-na*p/(p+2*(1-p-q))
  yy<-(y+na+nab)/(2*n)
  return(yy)
}
qf<-function(p,q){
  y<-nb*q/(q+2*(1-p-q))
  yy<-(y+nb+nab)/(2*n)
  return(yy)
}
p1<-0.5;q1<-0.3
p<-numeric();i<-1;q<-numeric()
while(p1 != pf(p1,q1) | q1 != qf(p1,q1)){
  p[i]<-pf(p1,q1)
  q[i]<-qf(p1,q1)
  i<-i+1
  p1<-pf(p1,q1)
  q1<-qf(p1,q1)
}
print(p1)
print(q1)
print(p)
print(q)
# compute the corresponding
#log-maximum likelihood values
ez1<-na*p/(p+2*(1-p-q))
ez2<-nb*q/(q+2*(1-q-p))
k1<-nab+na+ez1
k2<-nab+nb+ez2
k3<-2*noo+na+nb-ez1-ez2
elnf<-k1*log(p)+k2*log(q)+k3*log(1-p-q)
print(elnf)
plot(elnf,type="l")

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

## -----------------------------------------------------------------------------
attach(mtcars)
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
formulas<- as.character(formulas)
for (i in 1: length(formulas)){
  t<-lm(formulas[i])
  print(t)
}
lapply(formulas,function(x) lm(x))
detach(mtcars)

## -----------------------------------------------------------------------------
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)

## -----------------------------------------------------------------------------
sapply(trials,function(x) x$p.value)
# extra challenge
x<-numeric()
for(i in 1:length(trials)){
  x[i]<-trials[[i]]$p.value
}
print(x)

## -----------------------------------------------------------------------------
attach(mtcars)
f <- function(X, FUN, FUN.VALUE, simplify = FALSE){
  out <- Map(function(x) vapply(x, FUN, FUN.VALUE), X)
  if(simplify == TRUE){return(simplify2array(out))}
  out
}
f(list(mtcars),sum, numeric(1))
detach(mtcars)


###Analysis of Enviromental Data
###Deterministic functions -- R script

###1. Set up your R work session
source("https://michaelfrancenelson.github.io/eco_602_634_2020/mc_garigal_data/biostats_mcGarigal.R")
dat_dir = "https://michaelfrancenelson.github.io/eco_602_634_2020/mc_garigal_data/"

###3. Linear function -- Oregon birds example
#examine the data
birds<-read.csv(paste0(dat_dir,'bird.sub.csv'),header=TRUE)
hab<-read.csv(paste0(dat_dir,'hab.sub.csv'),header=TRUE)
str(birds)
str(hab)
birdhab<-merge(hab,birds,by=c('basin','sub'))
str(birdhab)
plot(birdhab$ls,birdhab$BRCR)
plot(birdhab$ls,birdhab$BRCR,xlim=c(0,120),ylim=c(0,1.2),
  xlab='large sawtimber',ylab='Brown Creeper abundance',
  main='linear model example',cex=1.5,cex.lab=1.5,cex.main=2,
  pch=19,col='gray')

#linear function
a<-0
b<-.01
x<-50
y<-a + b*x
y
xvec<-seq(0,100)
y<-a + b*xvec
y

#plot function using curve
plot(birdhab$ls,birdhab$BRCR)
curve(0+.01*x,from=0,to=100,add=TRUE)
curve(.05+.007*x,from=0,to=100,lty=2,add=TRUE)
linear<-function(x,a=0,b=1) a + b*x
curve(linear(x,a=.05,b=.008),from=0,to=100,lty=3,add=TRUE)
legend(5,0.9,legend=c('a=0,b=.01','a=.05,b=.007','a=.05,b=.008'),
  lty=c(1,2,3),bty='n',cex=1.5)


###4. Logistic function -- Oregon birds example
#examine the data
birds<-read.csv(file.path(dat_dir, 'bird.sta.csv'),header=TRUE)
hab<-read.csv(file.path(dat_dir, 'hab.sta.csv'),header=TRUE)
str(birds)
str(hab)
birdhab<-merge(hab,birds,by=c('basin','sub','sta'))
bin<-data.trans(birdhab,method='power',var='AMCR:YRWA',exp=0,plot=FALSE)
plot(bin$ba.tot,bin$BRCR)

#logistic function
a<--2
b<-.05
x<-50
y<-(exp(a + b*x)/(1 + exp(a + b*x)))
y
xvec<-seq(0,200)
y<-(exp(a + b*xvec)/(1 + exp(a + b*xvec)))
y

#plot function using curve
plot(bin$ba.tot,bin$BRCR)
logistic<-function(x,a,b) (exp(a + b*x)/(1 + exp(a + b*x)))
curve(logistic(x,a=-2,b=.05),from=0,to=200,add=TRUE)

#derivatives
logis<-expression(exp(a + b*x)/(1 + exp(a + b*x)))
dfun<-deriv(logis,'x',function.arg=TRUE)
xvec<-seq(0,200)
y<-dfun(xvec)
plot(xvec,attr(y,'gradient'),type='l',ylab='derivative')
abline(h=0,col='red')

#extract x value at maximum first derivative
d1<-attr(y,'gradient')
(inflection<-xvec[d1==max(d1)])
#alternative simpler method
(inflection<-xvec[y==0.5])

#plot x value at maximum first derivative
abline(v=inflection,col='blue',lty=2)


###5. Ricker function -- Stripped bass example
#examine the data
striper<-read.csv(file.path(dat_dir, 'striperSR.csv'),header=TRUE)
str(striper)
plot(striper$stock,striper$recruits)

#ricker function
a<-0.5
b<-1/50000
x<-20000
y<-a*x*exp(-b*x)
y
xvec<-seq(0,80000,length=100)
y<-a*xvec*exp(-b*xvec)
y

#plot function using curve
plot(striper$stock,striper$recruits)
ricker<-function(x,a=1,b=1) a*x*exp(-b*x)
curve(ricker(x,a=.7,b=.00002),from=0,to=80000,add=TRUE)

#derivatives
rick<-expression(a*x*exp(-b*x))
dfun<-deriv(rick,'x',function.arg=TRUE)
xvec<-seq(0,80000,length=100)
y<-dfun(xvec)
plot(xvec,attr(y,'gradient'),type='l',ylab='derivative')
abline(h=0,col='red')

#extract x value at zero first derivative
d1<-attr(y,'gradient')
(inflection<-xvec[d1>=-.001 & d1<=.001])

#plot x value at zero first derivative
abline(v=inflection,col='blue',lty=2)


###6. Bestiary of deterministic functions

#polynomial functions
linear<-function(x,a,b) a + b*x
quadratic<-function(x,a,b,c) a + b*x + c*x^2
cubic<-function(x,a,b,c,d) a + b*x + c*x^2 + d*x^3

#piecewise polynomials
threshold<-function(x,a1,a2,s) ifelse(x<s,a1,a2)
hockey<-function(x,a,b,s) ifelse(x<s,a+b*x,a+b*s)
hockey2<-function(x,a,s) ifelse(x<s,a*x,a*s)
piecewise<-function(x,a,b,c,s) ifelse(x<s,a+b*x,a+b*s+c*(x-s))

#rational functions (polynomials in fractions)
hyperbolic<-function(x,a,b) a/(b+x)
bevholt<-function(x,a,b) a*x/(b+x)
holling3<-function(x,a,b) a*x^2/(b^2 + x^2)
holling4<-function(x,a,b,c) a*x^2/(b + c*x + x^2)

#simple exponentials
exponential<-function(x,a,b) a*exp(b*x)
monomolecular<-function(x,a,b) a*(1-exp(-b*x))

#combinations of exponentials with other functions
ricker<-function(x,a,b) a*x*exp(-b*x)
powricker<-function(x,a,b,alpha) b*(x/a*exp(1-x/a))^alpha	
tricker<-function(x,a,b,t,min=1e-04) {
	ifelse(x<t,min,b*((x-t)/a*exp(1-(x-t)/a)))
	}
logistic<-function(x,a,b) exp(a + b*x)/(1 + exp(a + b*x))
modlogistic<-function(x,eps,beta,phi) exp(eps*(phi-x))/(1+exp(beta*eps*(phi-x)))
norm2<-function(x,mu,sigma) 1/(sqrt(2*pi)*sigma)*exp(-((x-mu)^2/(2*sigma^2)))
norm3<-function(x,mu,sigma,hgt) hgt*exp(-((x-mu)^2)/(2*sigma^2))
halfnorm<-function(x,sigma,hgt) hgt*exp(-(x^2)/(2*sigma^2))

#power laws
power<-function(x,a,b) a*x^b
vonbert<-function(x,a,d,k) a*(1 - exp(-k*(a-d)*x))^(1/(1-d))
shepherd<-function(x,a,b,c) a*x /(b + x^c)
hassell<-function(x,a,b,c) a*x /((b+x)^c)))

















my_bool_vec= (==my_vec)


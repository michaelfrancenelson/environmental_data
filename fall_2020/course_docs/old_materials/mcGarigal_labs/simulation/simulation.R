###Analysis of Environmental Data
###Stochastic simulation -- R script

###1. Set up your R work session
setwd('c:/work/stats/ecodata/lab/simulation/')
source('c:/work/r/scripts/biostats.R')


###3.1. Simulation -- linear model

#examine the real data
birds<-read.csv('bird.sub.csv',header=TRUE)
hab<-read.csv('hab.sub.csv',header=TRUE)
birdhab<-merge(hab,birds,by=c('basin','sub'))
plot(birdhab$ls,birdhab$BRCR,pch=19)

#fit linear model with normal errors (OLS)
fit1<-lm(BRCR~ls,data=birdhab)
fit1
abline(reg=fit1)
ci.lines(fit1)

#simulate linear model with normal errors
xvec<-birdhab$ls
a<-coef(fit1)[1]
b<-coef(fit1)[2]
y.det<-a + b*xvec
y.error<-summary(fit1)$sigma
ysim<-rnorm(length(xvec),mean=y.det,sd=y.error)
points(xvec,ysim,add=TRUE,col='red')

#fit linear model with gamma errors (MLE)
library(bbmle)
gammaNLL<-function(a,b,shape){
	y.mean<-a + b*birdhab$ls
	-sum(dgamma(birdhab$BRCR+.000001,shape=shape,scale=y.mean/shape,log=TRUE))
	}
fit2<-mle2(gammaNLL,start=list(a=.09,b=.005,shape=2),method='BFGS')
fit2

#simulate linear model with gamma errors
xvec<-birdhab$ls
a<-coef(fit2)[1]
b<-coef(fit2)[2]
ydet<-a + b*xvec
ysim<-rgamma(length(xvec),shape=coef(fit2)[3],scale=ydet/coef(fit2)[3])
points(xvec,ysim,add=TRUE,col='red')

###3.2 Power analysis for the linear model

#simulate linear model with normal errors
xvec<-birdhab$ls
a<-coef(fit1)[1]
b<-coef(fit1)[2]
y.det<-a + b*xvec
y.error<-summary(fit1)$sigma
ysim<-rnorm(length(xvec),mean=y.det,sd=y.error)
fit<-lm(ysim~xvec)
summary(fit)$coefficients['xvec','Pr(>|t|)']

#simulate linear model many times
nsim<-1000
pval<-numeric(nsim)
for(i in 1:nsim){
  y.det<-a + b*xvec
  ysim<-rnorm(length(xvec),mean=y.det,sd=y.error)
  fit<-lm(ysim~xvec)
  pval[i]<-summary(fit)$coefficients['xvec','Pr(>|t|)']
}

#calcluate power
sum(pval<0.05)/nsim

#calculate power for a gradient in slope values
nsim<-1000
pval<-numeric(nsim)
bvec<-seq(-.01,.01,by=0.001)
power.b<-numeric(length(bvec))
for(j in 1:length(bvec)){
  b<-bvec[j]
  for(i in 1:nsim){
    y.det<-a + b*xvec
    ysim<-rnorm(length(xvec),mean=y.det,sd=y.error)
    fit<-lm(ysim~xvec)
    pval[i]<-summary(fit)$coefficients['xvec','Pr(>|t|)']
  }
  power.b[j]<-sum(pval<0.05)/nsim
}

#plot power as a function of effect size (slope)
plot(bvec,power.b,type='l',xlab='Effect size',ylab='Power')
abline(v=coef(fit1)[2],lty=2,col='red')

#calculate power for a gradient in sample sizes
b<-coef(fit1)[2]
nsim<-1000
pval<-numeric(nsim)
nvec<-seq(10,50)
power.n<-numeric(length(nvec))
for(j in 1:length(nvec)){
  xvec<-seq(0,100,length.out=nvec[j])
  for(i in 1:nsim){
    y.det<-a + b*xvec
    ysim<-rnorm(length(xvec),mean=y.det,sd=y.error)
    fit<-lm(ysim~xvec)
    pval[i]<-summary(fit)$coefficients['xvec','Pr(>|t|)']
  }
  power.n[j]<-sum(pval<0.05)/nsim
}

#plot power as a function of sample size
plot(nvec,power.n,type='l',xlab='Sample size',ylab='Power')
abline(v=length(birdhab$ls),lty=2,col='red')

#calculate power for gradient in both slope and sample size
nsim<-1000
pval<-numeric(nsim)
bvec<-seq(-.01,.01,by=0.001)
nvec<-seq(10,50)
power.bn<-matrix(nrow=length(bvec),ncol=length(nvec))
for(k in 1:length(bvec)){
  b<-bvec[k]
  for(j in 1:length(nvec)){
    xvec<-seq(0,100,length.out=nvec[j])
    for(i in 1:nsim){
      y.det<-a + b*xvec
      ysim<-rnorm(length(xvec),mean=y.det,sd=y.error)
      fit<-lm(ysim~xvec)
      pval[i]<-summary(fit)$coefficients['xvec','Pr(>|t|)']
    }
    power.bn[k,j]<-sum(pval<0.05)/nsim
  }
}

#contour plot of power as a function of slope and sample size
contour(x=bvec,y=nvec,z=power.bn)

#perspective plot of power as a function of slope and sample size
persp(x=bvec,y=nvec,z=power.bn,col='lightblue',theta=30,phi=30,expand=.75,ticktype='detailed')
                                
#enhanced perspective plot of power as a function of slope and sample size
jet.colors<-colorRampPalette(c("blue","green")) #select ends of color gradient
nbcol<-100 #specify number of color levels
color<-jet.colors(nbcol) #generate the desired number of colors from this palette
nrz<-nrow(power.bn) #compute the z-value at the facet centres
ncz<-ncol(power.bn)
zfacet<-power.bn[-1,-1] + power.bn[-1,-ncz] + power.bn[-nrz,-1] + power.bn[-nrz,-ncz]
facetcol<-cut(zfacet,nbcol) #recode facet z-values into color indices
persp(x=bvec,y=nvec,z=power.bn,col=color[facetcol],theta=30,phi=30,expand=.75,ticktype='detailed')


###4. Simulating dynamic processes -- population matrix model

#function to calculate fecundity
calc.fec<-function(fec.mean=10,fec.sd=10){
	result<-rnorm(1,mean=fec.mean,sd=fec.sd)
	if(result<0) result<-0
	return(result)
}

#function to calculate adult survival
calc.adult.surv<-function(adult.surv.mean=0.61,adult.surv.sd=0.05){
	result<-rnorm(1,mean=adult.surv.mean,sd=adult.surv.sd)
	if(result<0) result<-0
	return(result)
}

#function to calculate juvenile survival
calc.juv.surv<-function(juv.surv.mean=0.10,juv.surv.sd=0.03){
	result<-rnorm(1,mean=juv.surv.mean,sd=juv.surv.sd)
	if(result<0) result<-0
	return(result)
}

#function to build transition matrix
build.transition.matrix<-function(fm=10,fs=10,asm=0.61,ass=0.05,jsm=0.1,jss=0.03){
	t<-matrix(0,nrow=4,ncol=4)
	t[1,4]<-calc.fec(fec.mean=fm,fec.sd=fs)
	t[4,4]<-calc.adult.surv(adult.surv.mean=asm,adult.surv.sd=ass)
	t[4,3]<-1
	t[3,2]<-1
	t[2,1]<-calc.juv.surv(juv.surv.mean=jsm,juv.surv.sd=jss)
	return(t)
}

#build trial transition matrix
build.transition.matrix()
build.transition.matrix(fm=10,fs=10,asm=0.61,ass=0.05,jsm=0.1,jss=0.03)
                          
#simulate population change
tsteps<-100 #simulation length
pop<-c(40,2,2,20) #initial population age struccture
output<-matrix(NA,nrow=tsteps,ncol=4) #create object to store results
colnames(output)<-c('juv','sub1','sub2','adult') 
for(i in 1:tsteps){
	t<-build.transition.matrix() #build random transition matrix
	pop<- t %*% pop #multiply transition matrix by population vector
	if(pop[1]>300) pop[1]<-300 #impose cieling on year 1 cohort size
	output[i,]<-pop # store result
}
matplot(1:tsteps,output,type='l',lty=1,col=5:1,main='Population simulation',
  xlab='Time step',ylab='Count')

#function to simulate population change
popsim<-function(tsteps=100,pop=c(40,2,2,20),ceiling=300,
  fm=10,fs=10,asm=0.61,ass=0.05,jsm=0.1,jss=0.03){
  output<-matrix(NA,nrow=tsteps,ncol=4)
  colnames(output)<-c('juv','sub1','sub2','adult') 
  for(i in 1:tsteps){
  	t<-build.transition.matrix(fm=fm,fs=fs,asm=asm,ass=ass,jsm=jsm,jss=jss) 
  	pop<- t %*% pop 
  	if(pop[1]>ceiling) pop[1]<-ceiling 
  	output[i,]<-pop 
  }
  total<-apply(output,1,sum)
  output<-cbind(output,total)
  matplot(1:tsteps,output,type='l',lty=1,col=5:1,
    main='Population simulation',xlab='Time step',ylab='Count')
  legend('topright',inset=c(.01,.01),legend=colnames(output),lty=1,col=5:1)
}

popsim()
popsim(asm=0.4,jsm=0.05)



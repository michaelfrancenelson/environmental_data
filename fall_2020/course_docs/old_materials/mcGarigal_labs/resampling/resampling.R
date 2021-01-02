###Analysis of Environmental Data
###Resampling -- R script

###1. Set up your R work session
setwd('c:/work/stats/ecodata/lab/resampling/')
source('c:/work/r/scripts/biostats.R')


###2. Bootstrap

#2.1 Bootrap confidence interval
moths<-read.csv('moths.csv',header=TRUE)
moths
hist(moths$anst)
anst<-moths$anst
m.obs<-mean(anst)
m.obs

#parametric confidence interval
(ci.obs<-qt(.975,df=length(anst)-1)*sd(anst)/sqrt(length(anst)))
m.obs-ci.obs
m.obs+ci.obs

ci.obs<-1.96*sd(anst)/sqrt(length(anst))
m.obs-ci.obs
m.obs+ci.obs

#bootstrap confidence interval
m<-10000
result<-numeric(m)
for(i in 1:m){
	result[i]<-mean(sample(anst,replace=TRUE))
}
mean(result)
quantile(result,c(0.025,0.975))

library(boot)
mymean<-function(anst,i) mean(anst[i])
myboot<-boot(anst,mymean,R=10000)
myboot

mean(anst)
mean(myboot$t)-mean(anst)
sd(myboot$t)
quantile(myboot$t,c(0.025,0.975))

#2.2 A more complex example -- rarefaction curves
data<-moths[,-1]
data
n<-nrow(data)
m<-10000
result<-matrix(nrow=m,ncol=n)
for(i in 1:m){
	for(j in 1:n){
		t1<-data[sample(1:n,size=j,replace=TRUE),]
		t2<-apply(t1,2,sum)
		result[i,j]<-sum(t2>0)
	}
}
rare.mean<-apply(result,2,mean)
rare.quant<-apply(result,2,quantile,probs=c(0.025,0.975))
rare<-t(rbind(rare.mean,rare.quant))
matplot(rare,type='l',xlab='Number of samples',ylab='Species richness',main='Rarefaction Curve')
legend('bottomright',legend=c('mean','2.5%','97.5%'),lty=c(1,2,3),col=c(1,2,3),inset=c(.1,.1))


###3. Randomization test

#3.1 A simple example -- linear regression
birds<-read.csv('bird.sub.csv',header=TRUE)
hab<-read.csv('hab.sub.csv',header=TRUE)
birdhab<-merge(birds,hab,by=c('basin','sub'))

plot(birdhab$s.sidi,birdhab$b.sidi)
abline(reg=lm(b.sidi~s.sidi,data=birdhab))
ci.lines(lm(b.sidi~s.sidi,data=birdhab),lty=2)
slope.obs<-coef(lm(b.sidi~s.sidi,data=birdhab))[[2]]
slope.obs
 
data<-subset(birdhab,select=c(b.sidi,s.sidi))

perm<-data
perm$b.sidi<-perm[sample(1:nrow(perm)),1]

plot(perm$s.sidi,perm$b.sidi)
abline(reg=lm(b.sidi~s.sidi,data=perm))
ci.lines(lm(b.sidi~s.sidi,data=perm),lty=2)
coef(lm(b.sidi~s.sidi,data=perm))[[2]]

perm<-data
m<-10000
result<-numeric(m)
for(i in 1:m){
  perm$b.sidi<-perm[sample(1:nrow(perm)),1]
  result[i]<-coef(lm(b.sidi~s.sidi,data=perm))[[2]]
}

hist(result)
abline(v=slope.obs,col='red',lty=2)
quantile(result,c(.05)) #one-side lower test
sum(result<=slope.obs)/m #exact p-value


#3.2 A more complex example -- mantel test
library(vegan)
dist.birds<-data.dist(birds[,4:101],method='bray')
dist.space<-data.dist(hab[,3:4],method='euclidean')

cor.obs<-cor(dist.space,dist.birds)
cor.obs
cor.test(dist.space,dist.birds)

n<-attributes(dist.birds)$Size
m<-10000
result<-numeric(m)
for(i in 1:m){
	rand<-sample(1:n)
	temp<-as.dist(as.matrix(dist.space)[rand,rand])
	result[i]<-cor(temp,dist.birds)
}

hist(result)
abline(v=cor.obs,col='red',lty=2)
quantile(result,c(.95)) #one-side upper test
sum(result>=cor.obs)/m #exact p-value

mantel(dist.space,dist.birds)


###4. Exercise

#4.1 Rarefaction curves -- Oregon birds
birds<-read.csv('bird.sta.csv',header=TRUE)
unique(birds[,1:2])
data<-birds[birds$basin=='D' & birds$sub=='AL', 4:81]

#try this for all 30 subbasins, but you better have lots of patience
data<-birds[,c(1:2,4:81)]
data$id<-paste(data$basin,data$sub,sep='')
subs<-unique(data$id)
result<-matrix(nrow=30,ncol=38)
for(k in 1:length(subs)){
    temp<-data[data$id==subs[k],4:80]
    n<-nrow(temp)
    m<-1000
    res<-matrix(nrow=m,ncol=n)
    for(i in 1:m){
    	for(j in 1:n){
    		t1<-temp[sample(1:n,size=j,replace=TRUE),]
    		t2<-apply(t1,2,sum)
    		res[i,j]<-sum(t2>0)
    	}
    }               
    result[k,1:n]<-apply(res,2,mean)
}
rare<-t(result)
colnames(rare)<-subs
matplot(rare,xlim=c(1,45),type='l',lty=1,col=seq(1,30),xlab='Number of samples',ylab='Species richness',main='Rarefaction Curves')
legend('right',legend=subs,lty=1,col=seq(1:30),cex=.75,inset=c(.01,.01))

#4.2 Randomization test -- Tree seedling response to understory treatment
data<-read.csv('vegdata.csv',header=TRUE)
data
plot(pine~treatment,data=data)

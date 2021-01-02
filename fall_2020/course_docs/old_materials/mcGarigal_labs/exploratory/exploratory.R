###Analysis of Environmental Data -- Lab
###Data Exaploration, Screening & Adjustments -- R script

###1. Set up your R work session
setwd('c:/work/stats/ecodata/lab/exploratory/')

source('c:/work/r/packages/biostats/r/biostats.R')

birds<-read.csv('bird.sub.csv',header=TRUE)
hab<-read.csv('hab.sub.missing.csv',header=TRUE)

str(birds)
str(hab)


###2. Summary statistics
col.summary(hab[,3:7]) #use table() for factors
table(hab[,1:2])

##############################################################################
#supplemental material
##############################################################################

#example using apply function
apply(hab[,3:7],2,mean)

#example using custom function
cv<-function(x) sd(x)/mean(x)*100
apply(hab[,3:7],2,function(x) sd(x)/mean(x)*100)

##############################################################################

#alternative using more complex sum.stats function
sum.stats(birds,var='AMCR:AMRO',margin='column')
sum.stats(birds,var='AMCR:AMRO',margin='row')


###3. Missing data
temp<-na.omit(hab)
mean(hab$s.sidi)
mean(hab$s.sidi,na.rm=TRUE)
temp<-hab
temp[is.na(temp)]<-0
temp$s.sidi
temp<-hab
temp$s.sidi[is.na(temp$s.sidi)]<-mean(temp$s.sidi,na.rm=TRUE)
temp$s.sidi
temp<-hab
temp$s.sidi[is.na(temp$s.sidi)]<-median(temp$s.sidi,na.rm=TRUE)
temp$s.sidi
temp<-hab
temp<-replace.missing(hab,var='sub.lat:w')
temp$s.sidi
temp<-hab
temp<-replace.missing(hab,var='sub.lat:w',method='mean')
temp$s.sidi
temp<-drop.var(hab,var='sub.lat:w',pct.missing=5)
str(temp)


###4. Frequency of occurrence and abundance plots
foa.plots(birds,var='AMCR:YRWA')


###5. Dropping variables
temp<-drop.var(birds,var='AMCR:YRWA',min.fo=3)
names(birds)[!names(birds) %in% names(temp)]
temp<-drop.var(birds,var='AMCR:YRWA',max.po=95)
temp<-drop.var(birds,var='AMCR:YRWA',min.cv=5)


###6. Single variable distributions

#6.1 Empirical (cumulative) distribution functions
edf.plots(birds,var='AMCR:YRWA')
ecdf.plots(birds,var='AMCR:YRWA')
ecdf.plots(birds,var='AMCR:YRWA',by='basin')

#6.2 Histograms
hist.plots(birds,var='AMCR:YRWA')
hist.plots(birds,var='AMCR:YRWA',by='basin')

#6.3 Box-and-whisker plots
box.plots(birds,var='AMCR:YRWA')
box.plots(birds,var='AMCR:YRWA',by='basin')

##############################################################################
#supplemental material
##############################################################################

library(ggplot2)
p<-ggplot(birds,aes(factor(basin),BRCR))
p+geom_violin()
p+geom_violin()+geom_jitter(height=0,width=0.1)
p+geom_violin(scale="width")+geom_jitter(height=0,width=0.1)
p+geom_violin(trim=FALSE)+geom_jitter(height=0,width=0.1)
p+geom_violin(adjust=0.5)+geom_jitter(height=0,width=0.1)
p+geom_violin(draw_quantiles=c(0.25, 0.5, 0.75))+geom_jitter(height=0,width=0.1)

##############################################################################

#6.4 Normal quantile-quantile plots
qqnorm.plots(birds,var='AMCR:YRWA')
qqnorm.plots(birds,var='AMCR:YRWA',by='basin')

#6.5 Four-in-one plots
uv.plots(birds,var='AMCR:YRWA')


###7. Relationships between pairs of variables

#7.1 Correlations

#pearson's correlations
cor(birds[,-c(1:3)])
#example with smaller set of variables
cor(birds[,c(4:8)])

#spearman's correlations
cor(birds[,-c(1:3)],method='spearman')
#example with smaller set of variables
cor(birds[,c(4:8)],method='spearman')

##############################################################################
#supplemental material
##############################################################################

#example manual calculation of cov
cov(birds$AMGO,birds$WIWR)
sum((birds$AMGO-mean(birds$AMGO))*(birds$WIWR-mean(birds$WIWR)))/(nrow(birds)-1)

#example manual calculation of pearson's cor
cor(birds$AMGO,birds$WIWR)
zAMGO<-(birds$AMGO-mean(birds$AMGO))/sd(birds$AMGO)
zWIWR<-(birds$WIWR-mean(birds$WIWR))/sd(birds$WIWR)
sum((zAMGO-mean(zAMGO))*(zWIWR-mean(zWIWR)))/(nrow(birds)-1)

#example manual calculation of spearman's cor
cor(birds$AMGO,birds$WIWR,method='spearman')
rAMGO<-rank(birds$AMGO)
rWIWR<-rank(birds$WIWR)
zAMGO<-(rAMGO-mean(rAMGO))/sd(rAMGO)
zWIWR<-(rWIWR-mean(rWIWR))/sd(rWIWR)
sum((zAMGO-mean(zAMGO))*(zWIWR-mean(zWIWR)))/(nrow(birds)-1)

##############################################################################

#7.2 Scatterplots
plot(hab$ls,birds$BRCR)
lines(lowess(hab$ls,birds$BRCR))
birdhab<-merge(hab,birds,by=c('basin','sub'))
scatter.plots(birdhab,y='AMCR:AMGO',x='ls')

#7.3 Scatterplot matrix
pairs(hab[,9:16])
pairs(hab[,9:16],lower.panel=panel.smooth,upper.panel=panel.cor,method='spearman')

#7.4 Coplots
coplot(BRCR~ls|sub.elev,data=birdhab,panel=panel.smooth)

#7.5 Redundancy plots
redun.plot(birds[,-c(1:3)])
redun.plot(birds[,-c(1:3)],var='AMCR:YRWA')

##############################################################################
#supplemental material
##############################################################################

#7.6 Interaction plots
veg<-read.csv('vegdata.csv',header=TRUE)
interaction.plot(veg$block,veg$treatment,response=veg$fern,fun=mean,
                 type="l",legend=TRUE,col=1:4,lwd=2)

##############################################################################


###8. Outliers
uv.outliers(hab,id=c('basin','sub'),var='sub.lat:w')
mv.outliers(birds,var='AMCR:YRWA',method='bray')
#mv.outliers(birds,var='AMCR:YRWA',method='mahalanobis')


###9. Data transformations

#9.1 Log transformation
data.trans(birds,var='AMCR:YRWA',method='log',plot=FALSE)

#9.2 Power transformation
data.trans(birds,var='AMCR:YRWA',method='power',exp=.5)
data.trans(birds,var='AMCR:YRWA',method='power',exp=0)

#9.3 Logit and Arcsin square root transformation
data.trans(hab,var='s.sidi',method='logit')
data.trans(hab,var='s.sidi',method='asin')


###10. Data standardizations
data.stand(birds,var='AMCR:YRWA',method='normalize',margin='row')


###11. Dissimilarity matrices
data.dist(birds[,-c(1:3)],method='euclidean')
data.dist(birds[,-c(1:3)],method='bray')

##############################################################################
#supplemental material
##############################################################################

#illustrate computation of euclidean distance
test<-birds[1:5,c(6,7,9)]
test
data.dist(test,method='euclidean')
dist1.2<-sqrt((test[1,1]-test[2,1])^2 + (test[1,2]-test[2,2])^2 + (test[1,3]-test[2,3])^2)
dist1.2

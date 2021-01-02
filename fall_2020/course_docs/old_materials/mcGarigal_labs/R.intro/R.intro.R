###Analysis of Environmental Data
###Introduction to R -- R script

###7. R objects and data types

number.species<-137
pi<-3.14159

small.value<-1.0e-10
species.name<-'American robin'
conifer<-TRUE
species.name + 37

###8. Data Structures

#8.1 Vectors and matrices
demo.vector1<-c(1,4,2,6,12)
demo.vector1
(demo.vector1<-c(1,4,2,6,12))
demo.vector1[4]
demo.vector2<-c(4,2,1,2,4)
demo.matrix<-cbind(demo.vector1,demo.vector2)
demo.matrix[4,2]
demo.matrix[,2]
demo.matrix[5,]
demo.matrix[]
demo.vector1[1:3] 
demo.vector1[-1]
demo.matrix[1:3,2]
demo.matrix[c(1,2,5),2]


#8.2 Data Frames
setwd('c:/work/stats/ecodata/lab/R.intro/')
birds<-read.csv('birds.csv',header=TRUE)
birds$AMRO
attach(birds)
AMRO
detach(birds)

#8.3 Lists
list.demo<-list(demo.vector1,names(birds))
names(list.demo)<-c('species.per.plot','field.names')
list.demo

#8.4 Checking objects
str(birds)
head(birds,10)
fix(birds)

###9. R Operators

max(birds[,5])
max(birds$AMRO)
sum(birds[,5])
log(birds[,4:10]+1)
sum(birds[,5]>0)
sum(birds[,5][birds[,5]>1])
max(birds[,5][birds$BHGR==5])


###Exercises

#1 - What is the total count (i.e., sum) of AMRO 
#across sites in BASIN = 'D' and BLOCKS <=5?


#2 - In how many BASINs = 'D' is ARMO present 
#(i.e., count > 0)?


#9.1 Missing Values
x<-birds[,4]
y<-x[!is.na(x)]
is.na( )
!is.na( )
!is.na(x)
x[!is.na(x)]
x[x!='NA']
y<-na.omit(x)

###Exercises

#1 - What is the total abundance of BHGR in sites where
#BGWA is missing (NA)?

#2 - What is the total abundance of BHGR in sites where 
#BGWA is missing (NA) or absent?


###10. Creating Subsets of a Matrix or Data Frame

birds.new<-birds[,4:10]
birds.new<-birds[,-c(1:3)]
birds.new<-subset(birds,select=AMGO:BHGR)
birds.new<-birds[,c(4,5,10)]
birds.new<-subset(birds,select=c(AMGO,AMRO,BHGR))
birds.new<-birds[1:4,]
birds.new<-birds[-c(5:10),]
birds.new<-birds[c(4,5,10),]
birds.new<-subset(birds,subset=SUB=='AL')
birds.new<-subset(birds,subset=BLOCK>10)
birds.new<-subset(birds,subset=BLOCK>10&SUB=='AL')

#Exercise - extract from birds sites (rows) where
#SUB = "AL" and AMRO>0, and keep only variables (cols)
#BCCH and BHGR


###11. Row or Column Operations on a Matrix or Data Frame

bird.max<-apply(birds[,4:10],2,max)
bird.max
bird.max<-apply(birds[,4:10],2,max,na.rm=TRUE) 
bird.max
bird.sum<-apply(birds[,4:10],1,sum,na.rm=TRUE)
bird.sum
bird.sum<-apply(birds[,4:10]>0,2,sum,na.rm=TRUE)
bird.sum

#Exercise - How many species are present (count > 0)
#at > 3 sites?


###12. Functions in R

sum(x,na.rm=FALSE)
sum(birds[,4:10])
sum(birds[,4:10],na.rm=TRUE)
apply(X=birds[,4:10],MARGIN=2,FUN=sum)
apply(birds[,4:10],2,sum)
apply(birds[,4:10],FUN=sum,MARGIN=2)

###13. Getting Data Into and Out of R

birds<-read.table('c:/work/stats/ecodata/lab/R.intro/birds.dat',header=TRUE)
setwd('c:/work/stats/ecodata/lab/R.intro/')
#bogus<-read.table('bogus.dat',header=TRUE,na.strings='-999')
birds<-read.table('birds.csv',header=TRUE,sep=',')
birds<-read.csv('birds.csv',header=TRUE)
birds<-read.csv('birds.csv',header=FALSE,skip=1)
names(birds)<-c('BASIN','SUB','BLOCK','AMGO','AMRO','BCCH','BEKI','BEWR','BGWA','BHGR')
birds<-read.csv('birds.rowids.csv',header=TRUE,row.names=1)

birds$BLOCK<-as.factor(birds$BLOCK)
str(birds)
write.table(birds,'out.csv',row.names=FALSE,sep=',')
write.table(birds,'c:/work/stats/ecodata/lab/R.intro/out.csv',row.names=FALSE,sep=',')
save(birds,file='c:/work/stats/ecodata/lab/R.intro/birds.RData')
load('c:/work/stats/ecodata/lab/R.intro/birds.RData')

###14. Plotting in R

?Devices  
help(Devices)
x<-1:50
y<-rnorm(50,mean=0,sd=1)
plot(x,y)
plot(x,y,type='o')
plot(x,y,type='o',lty=2)
plot(x,y,type='o',col='blue')
plot(x,y,type='o',pch=2)
plot(x,y,type='o',cex=2)
plot(x,y,type='o',lwd=2)
help(par)
par(mfrow=c(2,3))
par(new=TRUE)
par(mai=c(0.6,0.5,0.1,0.5))
par(mfrow=c(2,3),new=TRUE,mai=c(0.6,0.5,0.1,0.5))
par(mfrow=c(1,1))

###15. Getting Help in R

help(function) or
?function
help.search('mahalanobis distance')
??'mahalanobis distance'
RSiteSearch('mahalanobis distance')

###16. Libraries and Packages

library(MASS)
require(MASS)

###17. R workspaces

###18. Tutorials for learning R

###Extra. Writing functions
histplots<-function(data,var=NULL,save.plot=FALSE,...){
  
  old.par<-par(no.readonly=TRUE) #save original par settings
  par(mar=c(5,5,4,2)) #change plot margins
  
  if(!is.null(var)){ #if variable have been selected using the var= argument
    y<-subset(data,select=var) #select variables to summarize
    y<-as.data.frame(y) #make sure object is data frame
  }
  else y<-as.data.frame(data) # if selecting all variables by leaving var=null
  
  for(i in 1:ncol(y)){ #loop thru variables (columns) indexed by 'i'
    hist(y[,i], #histogram of column 'i'
         xlab=names(y[i]), #set x-axis lab to column name
         main=paste('Histogram of ',names(y[i]),sep=''),...) #add plot title
    
    if(save.plot==TRUE){ #if you selected to save the plots to file
      dev.print(jpeg,file=paste('hist.',names(y[i]),'.jpg',sep=''),width=800,height=600)
    } #end save
    
    if(!i==ncol(y)) readline("Press return for next plot ") #pause between plots
    
  } #end loop thru variables
  
  par(old.par) #change par settings back to original
}

histplots(birds[,4:10])
histplots(birds,var=c('AMRO','BHGR'))
histplots(birds,var=c('AMRO','BHGR'),col='gray',cex.lab=1.5,cex.axis=1.5,cex.main=2)
histplots(birds,var=c('AMRO','BHGR'),save.plot=TRUE)
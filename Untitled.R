getwd()
setwd("/Users/rsklanu/R")
##install library
install.packages("MASS")
library(MASS)
##input
input.data=Cars93
head(input.data)
head(input.data,10)
tail(input.data)
tail(input.data,10)
str(input.data)
summary(input.data)
names(input.data)

##assigning value
x=3
x
y=x+5
y


rolls<-function()  {
  mydierolling <- sample(die,size=1)
  mydierolling
   }
m="money"
m

##vectors
v1=c(1,2,3,4,5,6,7,8,9,10)
class(v1)

mean(v.a)
v3=c("Introduction","To","R")
class(v3)

v2=c("Intro","to R",x)
class(v2)

##create sequence
s1=1:5
s1

s2=seq(from=1,to=15,by=2)
s2


s4=rep(7,15)
s4
##vector arithmetic


v.a=c(1,1,4,5,3,2)
v.b=c(4,1,5,5,1,2)
v.a==v.b


v.a[3]

v.a[c(1,5)]
v.a[-3]


z=c("rrr","ssss","tttt")
l1=list(2,"a",z)
l1
class(l1)
l1[c(2,3)]
l1[[2]]

##factors

factorVa=factor(v.a)
factorVa


wday=c("m","t","w","th","f","s")
Factorwday=factor(wday)
Factorwday

##ordered factor


##update package
update.packages("MASS")
##Remove packages
detach(package::MASS)
data()


help("mean")
example("mean")
?subset
args(subset)
help(package=MASS)
##DATA FRAME
v.a=c(1,1,4,5,3,2)
v.b=c(4,1,5,5,1,2)
dataframe1=data.frame(v.a,v.b,v3)
dataframe1
##adding row to dataframe
new.row=data.frame(v.a=9,v.b=1,v3="New value")
dataframe2=rbind(dataframe1,new.row)
dataframe2
##adding column
new.col=c("its","time","to","add","newcolumn","hg","sd")
dataframe3=cbind(dataframe2,new.col)
dataframe3

##Renaming columns
colnames(dataframe3)<-c("session","students","topic","bla")
help("colnames")
example("colnames")



hist(input.data$Price)
hist(input.data$Price,main="Price histogram",xlab="Price",ylab="count")
hist(input.data$Price,col=rainbow(3),main="Price histogram",xlab="Price",ylab="count")
install.packages("ggplot2")
library(ggplot2)

library(rsconnect)
deployApp()

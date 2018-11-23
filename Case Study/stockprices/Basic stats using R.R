rm(list=ls())

library(reshape)

setwd('/Users/rsklanu/RBasics/Statistical Modeling using R Bengaluru/stockprices')


### Read the data file
stockreturn<-read.csv("stockpricedata.csv", header=TRUE)

### Check for the structure of the data
str(stockreturn)

### Summarize the data
summary(stockreturn)



###Mean
meanrtongc<-mean(stockreturn$rtongc, trim=0.5, na.rm=FALSE)

### Median
medianrtongc<-median(stockreturn$rtongc, trim=0, na.rm=FALSE)

### Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modertogc<-getmode(stockreturn$rtongc)

### Variance
varrtongc<-var(stockreturn$rtongc)

###Standard Deviation
sdrtongc<-sd(stockreturn$rtongc)

### Quartiles
s<-quantile(stockreturn$rtongc)

### Interquartile Range
IQR(stockreturn$rtongc)

### Interquartile range using quantile command
s[4]-s[2]

### Histogram - A histogram is a type of bar chart
### that graphically displays the frequencies of a data set.
hist(stockreturn$rtongc)

### A box plot, also called a box-and-whisker plot, is a chart
### that graphically represents the five most important descriptive 
### values for a data set. These values include the minimum value, 
### the first quartile, the median, the third quartile, and the maximum value.
boxplot(stockreturn$rtongc)


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

y1 <- remove_outliers(stockreturn$rtongc)

par(mfrow = c(1, 3))
boxplot(stockreturn$rtongc)
boxplot(y1)
boxplot(y1, range=0.5)

y2<-remove_outliers(stockreturn$rtairtel)
y3<-remove_outliers(stockreturn$rtsuzuki)
y4<-remove_outliers(stockreturn$rtsuzlon)
y4[y4 < -0.2] <- NA
y4[y4 > 0.2] <- NA
#### Histogram plots provide only the range of frequency of observations 
#### while box plots are better in telling where several parameters of a 
### distribution lie, example mean and variances that bar plots cannot. 
### Box plots are thus used as an effective comparative tool if one has several distributions.


#### Plotting multiple box plots This is the advantage

library(reshape)
data<-data.frame(y1,y2,y3,y4)

meltData <- melt(data)
boxplot(data=meltData, value~variable, na.rm=TRUE)


#### Pathologies, Multiple Modes
data2<-read.csv("boxandhist.csv", header=TRUE)
summary(data2)
x
par(mfrow=c(2,1))
hist(data2$Height.of.students)
boxplot(data2$Height.of.students, horizontal=TRUE)




##### when is the skewness too much?  The rule of thumb seems to be:
##### If the skewness is between -0.5 and 0.5, the data are fairly symmetrical
##### If the skewness is between -1 and - 0.5 or between 0.5 and 1, the data are moderately skewed
##### If the skewness is less than -1 or greater than 1, the data are highly skewed

library(e1071)
skewness(stockreturn$rtongc)
library(moments)
skewness(stockreturn$rtongc)
skewness(data2$Height.of.students)

### Covariance and Correlation

f<-stockreturn[,6:9]
g<-cov(f)
h<-cor(f)

#### Coefficient of Variation
attach(stockreturn)
cvrtongc<-sd(rtongc)/mean(rtongc)
cvrtairtel<-sd(rtairtel)/mean(rtairtel)
cvrtsuzuki<-sd(rtsuzuki)/mean(rtsuzuki)
cvrtsuzlon<-sd(rtsuzlon)/mean(rtsuzlon)


### Probability Distributions
### http://www.stat.umn.edu/geyer/old/5101/rlook.html

#### Normal Distribution
pnorm(27.4, mean=50, sd=20)
qnorm(0.25, mean=50, sd=20)
qnorm(0.25,0,1)
dnorm(27.4, mean=50, sd=20)
randvalues<-rnorm(100, mean=50, sd=20)

hist(randvalues, probability=FALSE)
hist(randvalues, probability=TRUE)


####Binomial Distribution

dbinom(15, size=30, prob=0.5)
pbinom(15, size=30, prob=0.5)
qbinom(0.25,size=100, prob=0.25)
randbinom<-rbinom(100,n=50, prob=0.75)
hist(randbinom)
hist(randbinom, probability=TRUE)

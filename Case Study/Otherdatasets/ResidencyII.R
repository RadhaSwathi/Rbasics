rm(list=ls())

library(MASS) ### Modern Applied Statistics with S

setwd("F:/Sridhar/PGPBA/Courses/SMDM/SMDM Mumbai/SMDM Jan 2018/Hypothesis Testing Residency 2/Case Study/Otherdatasets")

#### Internet Data
mobiletime<-read.csv("InternetMobileTime.csv",header=TRUE)
######## One-Sample (Large) two-tail Test


t.test(mobiletime$Minutes, mu=144)

t.test(mobiletime$Minutes, mu=144, alternative="greater")

t.test(mobiletime$Minutes, mu=250, alternative="less")

####Two Sample Test Using Luggage data
luggage<-read.csv("Luggage.csv", header=TRUE)

###Step 1. Test of Variances. Uses ratio of two variances, so it is an F Test.
var.test(luggage$WingA,luggage$WingB)

####Step 2. Test of Means with Equal variances
t.test(luggage$WingA,luggage$WingB, var.equal=TRUE, paired=FALSE)

####Step 2. Test of Means with unequal variances
t.test(luggage$WingA,luggage$WingB)


### Paired Sample Test:
concrete<-read.csv("Concrete1.csv",header=TRUE)
t.test(concrete$Two.days,concrete$Seven.days, paired=TRUE)

#### Using Cash Transfer Data
cashtr<-read.csv("cashtransfer.csv",header=TRUE)


#### One-sample test
#### Test whether before and after CT the calorific consumption of apl and bpl households is 2300

t.test(cashtr$bplcaloriebeforect, mu=2300,conf.level=0.95,alternative="two.sided")

t.test(cashtr$aplcaloriebefore, mu=2300,conf.level=0.95,alternative="two.sided")

t.test(cashtr$bplcalorieafterct, mu=2300,conf.level=0.95,alternative="two.sided")

t.test(cashtr$aplcalorieafter, mu=2300,conf.level=0.95,alternative="two.sided")

### Test whether there are any differences in average calorific consumption between before ct and after ct for both apl and bpl groups

t.test(cashtr$bplcalorieafterct,cashtr$bplcaloriebeforect, paired=TRUE,conf.level=0.95,alternative="two.sided")


t.test(cashtr$aplcalorieafter,cashtr$aplcaloriebefore, paired=TRUE,conf.level=0.95,alternative="two.sided")

### Conduct a t-test to study the difference between apl and bpl on the consumption.

a<- cashtr$bplcalorieafterct-cashtr$bplcaloriebeforect
b<- cashtr$aplcalorieafter-cashtr$aplcaloriebefore
var.test(a,b)
t.test(a,b, paired=FALSE,conf.level=0.95,alternative="two.sided")

### What are your conclusions?

#### Titanic Dataset for test of association

titanic<-read.csv("titanic.csv", header=TRUE)

#### The variable "Survived" has two levels 0 and 1. It is a categorical variable. 0 stands for died and 1 stands for survived.
### We decalre that the variable "survived" is a categorical variable by using the command 'factor'.
### The variable pclass stands for passenger class. It is also a categorical variable with three levels 1,2 and 3.
### The varible sex is a categorical variable.
survived<-factor(titanic$survived,levels=c(0,1),labels=c('Died','Survived'))

pclass<-factor(titanic$pclass,levels=c(1,2,3),labels=c('First','Second','Third'))

sex<-factor(titanic$sex,levels=c(0,1),labels=c('Male','Female'))

###Test whether the fare across the survivors and nonsurvivors is the same. 
### This is another way of specifying t.test The symbol ~ stands for "on". 
### fare~sex implies fare on sex.conduct a t-test of fare on sex.
### In other words conduct a t-test to check whether there are any fare differences between males and females.
var.test(fare ~ survived, data=titanic[titanic$survived %in% c(0,1),])
t.test(fare ~ survived, data=titanic[titanic$survived %in% c(0,1),])
t.test(fare ~ survived, data=titanic)
t.test(fare ~ sex, data=titanic)

##### Test of Association
cross<-table(titanic$survived,titanic$pclass)

round(100*prop.table(cross,2),digits=4)

barplot(prop.table(cross,2)*100,xlab='Class',ylab='Percentages',main="Percentage survival by Class",beside=T,col=c("gray","black"), legend=rownames(cross), args.legend = list(x = "topleft"))

chisq.test(table(survived,pclass))

result<-chisq.test(table(survived,pclass))
result$expected

### Exercise: Conduct Chi-sq test for survived and sex



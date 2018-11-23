rm(list=ls())

library(MASS) ### Modern Applied Statistics with S

setwd("/Users/rsklanu/RBasics/Statistical Modeling using R Bengaluru/Otherdatasets")

titanic<-read.csv("titanic.csv", header=TRUE)

survived<-factor(titanic$survived,levels=c(0,1),labels=c('Died','Survived'))

pclass<-factor(titanic$pclass,levels=c(1,2,3),labels=c('First','Second','Third'))

gender<-factor(titanic$Gender,levels=c(0,1),labels=c('Male','Female'))


cross<-table(titanic$survived,titanic$pclass)

round(100*prop.table(cross,2),digits=4)


describe(titanic$survived)

barplot(prop.table(cross,2)*100,xlab='Class',ylab='Percentages',main="Percentage survival by Class",beside=T,col=c("gray","black"), legend=rownames(cross), args.legend = list(x = "topleft"))

chisq.test(table(survived,pclass))

result<-chisq.test(table(survived,pclass))
result$expected



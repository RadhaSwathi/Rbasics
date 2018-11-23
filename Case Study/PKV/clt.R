####https://www2.stat.duke.edu/courses/Spring02/sta103/labs/lab3.html
rm(list=ls())

setwd('F:/SMDM Hyderabad 2425032018/Case Study/PKV')

income<-read.csv("Central Limit Theorem.csv", header=TRUE)

j=1000 ### repetitions
samplesize=25

s<- data.frame(matrix(0, ncol = j, nrow = samplesize))
d<- data.frame(matrix(0, ncol = 1, nrow = j))
for (i in 1:j){
  
  s[,i]<- (sample(income[i,1], replace=TRUE, size=samplesize))
  d[i,1]<-mean(s[,i])
}

colnames(d) <- "meansofsamples"

hist(d$meansofsamples)



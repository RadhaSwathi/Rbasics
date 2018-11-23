
####http://ucanalytics.com/blogs/bivariate-analysis-leverage-regression-case-study-example-part-3/

rm(list=ls())

setwd('/Users/rsklanu/RBasics/Statistical Modeling using R Bengaluru/SMDM Hyderabad 2425032018')

data<-read.csv("Houseprice-Analysis-Data.csv", header=TRUE)
summary(data)

data_without_missing<-data[complete.cases(data),]

nrow(data) - nrow(data_without_missing)

options(scipen = 100) # this will print the numbers without scientific notation

boxplot(data_without_missing$House_Price, col = "Orange",main="Box Plot of House Price")


#### Outliers
data_without_missing[data_without_missing$House_Price>10^8,]
boxplot(scale(data_without_missing[data_without_missing$House_Price<10^8,c(2:6,9:10)]),col="Orange")

data_without_outliers<-data_without_missing[data_without_missing$House_Price<10^8,]


###Mean
meanhouse<-mean(data_without_outliers$House_Price, trim=0, na.rm=FALSE)

### Median
medianhouse<-median(data_without_outliers$House_Price, trim=0, na.rm=FALSE)

### Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modehouse<-getmode(data_without_outliers$House_Price)

### Variance
varhouse<-var(data_without_outliers$House_Price)

###Standard Deviation
sdhouse<-sd(data_without_outliers$House_Price)

### Quartiles
s<-quantile(data_without_outliers$House_Price)

### Interquartile Range
IQR(data_without_outliers$House_Price)

### Interquartile range using quantile command
s[4]-s[2]

### Histogram - A histogram is a type of bar chart
### that graphically displays the frequencies of a data set.
hist(data_without_outliers$House_Price)

# Box plot and pair wise t-test
boxplot(data_without_outliers$House_Price/10^6~data_without_outliers$City_Category,
        xlab= "City Category",ylab="House Price (in Millions)",ylim=c(1,13),
        main="House Price by City Category",col=c("Orange","cornflowerblue","grey"))

text(1, 13, paste("P(A = B) = ",
                  round(pairwise.t.test(data_without_outliers$House_Price,data_without_outliers$City_Category)$p.value[1],2)),col = "red")

text(2, 13, paste("P(B = C) = ",
                  round(pairwise.t.test(data_without_outliers$House_Price,data_without_outliers$City_Category)$p.value[2],2)),col = "red")

text(3, 13, paste("P(A = C) = ",
                  round(pairwise.t.test(data_without_outliers$House_Price,data_without_outliers$City_Category)$p.value[4],2)),col = "red")
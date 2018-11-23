example(boxplot)
help("boxplot")
help("hist")
example(hist)


die<-1:6
sample(die)
sample(die,size=1)
rolls<-function()  {
  mydierolling <- sample(die,size=1,prob = c(1/8,1/8,1/8,1/8,1/8,3/8))
  mydierolling
}
reprolls<-replicate(5000,rolls())
qplot(reprolls,binwidth=1)
library(readxl)
LungCap_Dataset <- read_excel("~/Downloads/LungCap Dataset.xls")
View(LungCap_Dataset)

library(ggplot2)
Lc<-LungCap_Dataset$`LungCap(cc)`
age<- LungCap_Dataset$`Age( years)`
gender<- LungCap_Dataset$Gender
Smoker<-LungCap_Dataset$Smoke
caesarean= LungCap_Dataset$Caesarean
height<- LungCap_Dataset$`Height(inches)`
labels <- c(yes = "smoker", no = "Non-smoker")
labelC<-  c(yes = "Caeserean", no = "Not Caeserean")
p1<-ggplot(LungCap_Dataset,aes(y=Lc,x=age,color=gender))+geom_point()+xlab("Age")+ylab("Lung Capacity")+facet_grid(Smoke~Caesarean,labeller=labeller(Smoke = labels,Caesarean=labelC))
p1

p1<-ggplot(LungCap_Dataset,aes(y=Lc,x=age,color=gender))+geom_boxplot()+xlab("Age")+ylab("Lung Capacity")+facet_grid(Smoke~Caesarean,labeller=labeller(Smoke = labels,Caesarean=labelC))
p1
p1<-ggplot(LungCap_Dataset,aes(y=Lc,x=age,color=gender))+geom_line()+xlab("Age")+ylab("Lung Capacity")+facet_grid(Smoke~Caesarean,labeller=labeller(Smoke = labels,Caesarean=labelC))
p1

p2<-ggplot(LungCap_Dataset,aes(y=Lc,x=height,color=gender))+geom_point()+xlab("Height")+ylab("Lung Capacity")+facet_grid(Smoke~Caesarean,labeller=labeller(Smoke = labels,Caesarean=labelC))
p2


p2<-ggplot(LungCap_Dataset,aes(y=Lc,x=height,color=gender))+geom_boxplot()+xlab("Height")+ylab("Lung Capacity")+facet_grid(Smoke~Caesarean,labeller=labeller(Smoke = labels,Caesarean=labelC))
p2

p2<-ggplot(LungCap_Dataset,aes(y=Lc,x=height,color=gender))+geom_line()+xlab("Height")+ylab("Lung Capacity")+facet_grid(Smoke~Caesarean,labeller=labeller(Smoke = labels,Caesarean=labelC))
p2


p3<-ggplot(LungCap_Dataset,aes(y=age,x=height,color=gender))+geom_line()+xlab("Height")+ylab("Age")
p3

p3<-ggplot(LungCap_Dataset,aes(y=age,x=height,color=gender))+geom_boxplot()+xlab("Height")+ylab("Age")
p3
p3<-ggplot(LungCap_Dataset,aes(y=age,x=height,color=gender))+geom_point()+xlab("Height")+ylab("Age")
p3


##############################
####### Assignment 5 #########
##############################
# Weiyang Wang

print("Weiyang Wang")
print("wwang65@ucsc.edu")
print(1505028)

library(ggplot2)
library(foreign)
# Question 1
# part a
d1<-ggplot(data=diamonds,
       aes(x = x*y*z, y = price))
d1+ geom_point(aes(colour = clarity, size = carat)) + scale_x_log10() + scale_y_log10()
# part b
d2<-ggplot(data=diamonds,
           aes(x=carat))
d2+geom_histogram(aes(fill=clarity, y=..ndensity..))+facet_grid(cut~.)
levels(diamonds$cut)
# part c
d3<-ggplot(data=diamonds,
           aes(x=cut,y=price))
d3+geom_violin()+geom_jitter(alpha=0.02)

# Question 2
require(dplyr)
# part a
org<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
org1<-org %>%
  group_by(year,month) %>%
  summarise(
    Median.RW=median(rw,na.rm=T),
    firstQ=quantile(rw,0.25,na.rm=T),
    thirdQ=quantile(rw,0.75,na.rm=T),
    firstD=quantile(rw,0.1,type=5,na.rm=T),
    ninthD=quantile(rw,0.9,type=5,na.rm=T)
    ) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d"))
d4<-ggplot(org1
           ,aes(x=date,y=Median.RW))
d4+geom_line()+geom_ribbon(
  aes(ymin=firstD, ymax=ninthD),alpha=0.2
  )+geom_ribbon(aes(ymin=firstQ, ymax=thirdQ),alpha=0.5)
# part b
org2<- org %>%
  group_by(year,month,educ) %>%
  summarize(
    Median.RW=median(rw,na.rm=T)) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d"))
d5<-ggplot(org2,
           aes(x=date,y=Median.RW,group=educ))
d5+geom_line(aes(colour=educ))

rm(list=ls())

#Import the dot data that is saved as a .csv file
mydata<-read.csv("dots.csv",header=T)

#Place all dot data in one vector named "DOTS"
DOTS<-c(mydata[,1],mydata[,2],mydata[,3],mydata[,4],mydata[,5])

#Create a factor name vector "methods"
methods=factor(rep(LETTERS[1:5],each=10))

#Evaluate the similarity of the variances in all 5 sets
bartlett.test(DOTS,methods)

#Plot the dot data:
boxplot(DOTS~methods)

#Use an ANOVA test on the dot data
aov.dots=aov(DOTS~methods)

#execute the ANOVA test
# ADDED SUMMARY
summary(aov.dots)

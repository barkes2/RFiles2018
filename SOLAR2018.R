rm(list=ls())
require(astsa)
solar=read.table("sunspots.txt", header=T)
yearS=NULL
#counter or pointer
y=1
#reduce data to anual data from monthly.
for(i in seq(1,3216,12)){
	yearS[y]=sum(solar[i:(i+11),3])/12
	y=y+1
}
#look at the data.
yearS
#data covers years 1749-2016
dateS=c(1749:2016)
solarY=data.frame(dateS,yearS)
#autocorrelation function of the data (Periodiogram)
acf(yearS,type="correlation",main="Annual Mean Sunspots: Autocorrelation")
#Useful for getting a feel of data; would never use to report cyclicity or any other type.
model1=lm(yearS~dateS)
plot(solarY,type="l")#l=line
abline(model1,col="red")
summary(model1)
summary(aov(model1))

par(mfrow=c(2,1))
plot(resid(model1),type="l")
plot(diff(yearS),type="l")
#differencing appears to take care of the trend so we are ready for more robust analysis.Create vector of differenced values
dsolar=diff(yearS)

#===========================================================================================================================

dsolar=ts(dsolar,start=1749)
plot(dsolar,type="l")
par(mfrow=c(2,1))
acf(dsolar,type="correlation",main="Annual Mean Sunspots: Autocorrelation")

#perform spectral analysis
solar.per=mvspec(dsolar) #Unsmoothed
abline(v=1/11,lty="dotted")#Known solar cycle
#Is that peak statistically significant
which.min(abs(solar.per$freq-1/11))
1/solar.per$freq[25]
solar.per$df

k=kernel("modified.daniell",c(3,3))
solar.sm=mvspec(dsolar,k)
par(new=T)
solar.per=mvspec(dsolar)
solar.sm$df

yearSM=function(x,n=5){filter(x,rep(1/n,n),sides=2)}
yearSM.d=yearSM(yearS)
plot(yearSM.d)
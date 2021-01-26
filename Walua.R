rm(list=ls())
require(astsa)

# Start with functions --------------------------------------------

# Smoothing function
yearSM <- function(x, n = 7) { filter(x, rep(1/n, n), sides = 2) }

# Red Noise Function
# timeS is the time series data for which we want red noise.
# rho is the autocorrelation at a lag of 1 (AC[1]).
# very is the variance of the original data. Used to scale the noise.
rednoise <- function(timeS, rho, very) {
	print(c(rho, very))   # Print to console just to check input.
	ff <- timeS$freq
	Rnoise <- NULL
	for(i in 1:length(ff)) {
		Rnoise[i] <- very * (1 - rho^2) / ((1 + rho^2) - 2 * rho * cos(2 * pi * ff[i]))
	}
	return(Rnoise)	
}
# --------- End red noise function.

# Plot the results.
# spec is the spectral estimate performed elsewhere.
# redN is the red noise spectrum.
# crit95 is the significance threshold calculated from red noise.
# ptitle is the title to add to the plot.
# Huge range of values necessitates plotting with log axes.
plotSPEC <- function(spec, redN, crit95, ptitle) {
	Ylim <- c(0.0001, 1.2)      # Limits of y-axis.
	# The data are normalized by the maximum variance before plotting. This causes all values to be
	# between 0 and 1. Makes it easier to compare plots.
	plot(spec$freq, spec$spec / max(spec$spec), type = "o", xlab = "Frequency", ylab = "Power", main = ptitle, ylim = Ylim, log = "xy")
	par(new = TRUE)    # Add another set of data to the same plot.
	plot(spec$freq, redN / max(spec$spec), type ="l", xlab = "", ylab = "", ylim = Ylim, col = "red", log = "xy") # Red noise
	par(new = TRUE)    # Add another set of data to the same plot.
	plot(spec$freq, crit95 / max(spec$spec), type ="l", xlab = "", ylab = "", ylim = Ylim, col = "blue", log = "xy") # Critical line.
}
# --------- End plotting function.

# End of functions --------------------------------------------

wail=read.csv("wailua_months.csv",header=T)
plot(wail[,3],wail[,4],type="l")
n.months=length(wail[,1])
wail.m=matrix(0,nrow=n.months,ncol=3)
wail.m[,1]=wail[,3] #Months
wail.m[,2]=wail[,4] #Discharge
wail.m[,3]=log10(wail[,4]) #log10(Discharge)
par(mfrow=c(2,1))
plot(wail.m[,1],wail.m[,2],type="l")
plot(wail.m[,1],wail.m[,3],type="l")
hist(wail.m[,2])
#check for a trend
plot(wail.m[,1],wail.m[,3],type="l")
modelS=lm(wail.m[,3]~wail.m[,1])
abline(modelS,col"red")
summary(modelS)
summary(aov(modelS))
#getvarianceoftimeseriesforrednoisecalculations
wailVAR=var(wail.m[,3])

#makeatimeseries.
wailT=ts(wail.m[,3],start=1)

#perform autocorrelation [AC] analysis.
wailACF=acf(wailT,lag.max=400)
rho.lag1=wailACF$acf[2]
rho.lag1

wailSPEC=spec.pgram(wailT,plot=F)

#calcrednoisebycallingfunc tion
myRS=rednoise(wailSPEC,rho.lag1,wailVAR)

#calculate critical curve from red noise spectrum
myRS95=myRS/2*qchisq(0.99,2)
par(mfrow=c(1,1))
plotSPEC(wailSPEC,myRS,myRS95,"raw data")
#find stat sig peaks
sparkles=cbind(wailSPEC$freq,wailSPEC$spec,myRS95)
peaks=NULL
for(g in 1:length(sparkles[,1])){
	if(sparkles[g,2]>sparkles[g,3]){
		peaks=c(peaks,sparkles[g,1])
	}
}
print(1/peaks)
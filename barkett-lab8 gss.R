rm(list=ls())
require(car)
mydata=read.table("BRINE.TXT",header=T)
attach(mydata)
Y=NA.
X=CL
plot(X,Y,cex=2)
#Regression
model1=lm(Y~X)
# Y = m*X + b (equation for a line)
# Y = 0.6187*X + 53.6934
summary(model1)

# Step 1
# Look at model and fit.
plot(X,Y,xlab="X",ylab="Y",main="Model Trendline")
abline(model1)
segments(X,fitted(model1),X,Y)

# Step 2
# Look at residuals.
plot(X,resid(model1))

# Step 3
# Look at residuals, leverage, and influence.
par(mfrow=c(2,2)) # Window with 2x2 plots.
plot(model1)

# Step 4
# Check influence (Cook's distance).
influenceIndexPlot(model1,id.n=3)

# Step 5
# Look at hat values.
plot(hatvalues(model1))

# Step 6
# Perform outlier test.
outlierTest(model1)

# Step 7 (when needed)
# IF outlier exists...
# Remove outlier and run again.
rm(list=ls())
# MISSING SCRIPT even after resubmission.
require(car)
mydata=read.table("BRINE.TXT",header=T)
attach(mydata)

par(mfrow=c(1,1))
X<-NA.
Y<-CL
n = length(X)
X2 <- c(X[1:8],X[10:n])
Y2 <- c(Y[1:8],Y[10:n])
plot(X,Y)

# Regression
model2=lm(Y2~X2)
# Y = m*X2 + b (equation for a line)
# Y = 0.6187*X2 + 53.6934
summary(model2)

# Step 1
# Look at model and fit.
plot(X2,Y2,xlab="X2",ylab="Y2",main="Model Trendline")
abline(model2)
segments(X2,fitted(model2),X2,Y2)

# Step 2
# Look at residuals.
plot(X2,resid(model2))

# Step 3
# Look at residuals, leverage, and influence.
par(mfrow=c(2,2)) # Window with 2x2 plots.
plot(model2)

# Step 4
# Check influence (Cook's distance).
influenceIndexPlot(model2,id.n=3)

# Step 5
# Look at hat values.
plot(hatvalues(model2))

# Step 6
# Perform outlier test.
outlierTest(model2)

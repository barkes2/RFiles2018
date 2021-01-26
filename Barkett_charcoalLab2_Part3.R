rm(list=ls())
# Read in the data into "char".
char <- read.csv("charcoal-data.csv", header = TRUE)

# char is initially a data frame with 1 column.
# R interprets char[,1] as get the data in the first column [,1]
char <- char[,1]

# Sort char by size with the highest values first (decreasing = TRUE).
order(char[,1], decreasing=TRUE)

# Create a new vector containing log10 values of char.
# Name the vector charLOG10.
charLOG10 <- c(log10(char))

# cbind the log10 values with char to create an enlarged char data frame.
char.table1<-cbind(charLOG10, char)
# Name the columns "Conc" and "LOG10(Conc)".
colnames(char.table1) <- c("LOG10(Conc)","Conc")
char.table1


# Calculate and show the means, medians, and standard deviations of two columns in char.
c.mean <- mean(char.table1[,1])
c.mean

c.median <- median(char.table1[,1])
c.median

c.sd <- sd(char.table1[,1])
c.sd

c.log.mean <- mean(char.table1[,2])
c.log.mean

c.log.median <- median(char.table1[,2])
c.log.mean

c.log.sd <- sd(char.table1[,2])
c.log.sd

# Make histograms of the two columns, but do not show them yet.
# Name the containiers hist.char and hist.charLOG10.
hist.char <- hist(char,plot=F)
hist.charLOG10 <- hist(charLOG10,plot=F)

# The command below creates a window that will show side-by-side plots.
# pty="s" tells R to make the plots square shaped.
par(mfrow=c(1,2), pty="s")

# Using plot, show the two containers (histograms).
# Use x limits of 0 to 2.5 for the untransformed data.
# Use x limits of  -1 to 0.5 for the log10 values.
# Title the plots "Charcoal Concentrations (mm2/cm2)" and "Log(Concentrations)".
y1 <- c(20,20)
plot(hist.char,main="Charcoal Concentrations (mm2/cm2)", xlim=c(0,2.5),ylim=c(0,35))
y2 <- c(20,20)
plot(hist.charLOG10, main="Log(Concentraions)", xlim=c(-1,0.5), ylim=c(0,35))



# Calculate the probability of a charcoal sample having a concentration
# equal to or greater than 0.8 mm2/cm2 
pnorm(0.8,c.mean,c.sd,lower.tail=FALSE)

# Perform the same calculation for its log10 value.
pnorm(0.8,c.log.mean,c.log.sd, lower.tail=FALSE)

# What is the probability of a sample being greater than 1.5 mm2/cm2?
# Calculate p using only the log10 values because only they are normally distributed.

prob.g1.5 <- c(1-(pnorm(log10(1.5),c.log.mean,c.log.sd, lower.tail=FALSE)))
prob.g1.5
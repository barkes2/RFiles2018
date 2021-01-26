humble.txt <- c(rm(list=ls()))
# PUT YOUR LASTNAME HERE: Barkett
# We can calculate the probability of a value being
# larger or smaller than x using pnorm.
# We enter pnorm(x, mean, stdev, lower.tail = _____ )
# lower.tail = T tells R to calculate the area under the LEFT tail. 
# lower.tail = F tells R to calculate the area under the RIGHT tail.
# Other commands to use:
# read.table, mean, sd, log10, cbind, data.frame, rownames, and colnames
# Follow the instructions below and fill in the blank lines below each instruction.

# Use the line below to read the data into a table called humble.
humble <- read.delim("humble.txt", header = TRUE)

# Calculate the mean of the permeability data and write it to p.mean
p.mean <- mean(humble[,2])
# Calculate the s.d. of the permeability data and write it to p.sd
p.sd <- sd(humble[,2])

# Calculate the probability of a value being <100 as p.100
pnorm(100,p.mean,p.sd, lower.tail = T)

# Calculate the probability of a value being >2000 as p.2000
pnorm(2000,p.mean,p.sd,lower.tail = F)

# Calculate the probability of a value being 100 < x < 2000 as p.100to2000
pnorm(2000,p.mean,p.sd, lower.tail = T) - pnorm(100,p.mean,p.sd, lower.tail = T)

# Use log10 to transform the permeability as plog
plog <- log10(humble[,2])

# Combine humble and plog into one table.
Table1 <- cbind(plog,humble)
Table1

# Calculate the mean of the log10 data and write it to plog.mean
plog.mean <- mean(plog)
plog.mean

# Calculate the s.d. of the log10 data and write it to plog.sd
plog.sd <- sd(plog)
plog.sd

# Calculate the probability of a value being <2 as p.2
p.2 <- pnorm(2,plog.mean,plog.sd, lower.tail = T)

# Calculate the probability of a value being >3.30 as p.3.3
p.3.3 <- pnorm(3.30,plog.mean,plog.sd, lower.tail = F)

# Calculate the probability of a value being 2 < x < 3.3 as p.2to3.3
p.2to3.3 <- pnorm(3.30,plog.mean,plog.sd, lower.tail = T) - pnorm(2,plog.mean,plog.sd, lower.tail = T)

# Put p.100, p.2000, and p.100to2000 into the vector perm.p
perm.p <-c(p.2to3.3,p.3.3,p.2)

# Put p.2, p.3.3, and p.2to3.3 into the vector plog.p
plog.p <- c(perm.p)

# Make the data frame rezults with columns perm.p and plog.p
table2 <- data.frame(perm.p, plog.p)

# Label the columns in rezults using colnames.
# Use the lables "Perm." and "log(Perm.)"
colnames(table2) <- c("Perm","log(Perm.)")

# Label the rows in rezults using rownames.
# Use the lables "<100", ">2000", and "100<x<2000"
rownames(table2) <- c("<100", ">2000", "100<x<2000")


# Run the line below to view rezults in the console.
table2
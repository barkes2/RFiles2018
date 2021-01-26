rm(list=ls())
# FILL IN BLANKS
# Simulate rolling a six-sided die multiple times.
# Complete by replacing the ____ with the correct numbers and variables.

# Start by creating a vector containing the 6 sides of a die.
die.faces <-  c(1, 2, 3, 4, 5, 6)

# Simulate rolling the die 1200 times.
n <- 1200
die.results <-  sample(die.faces, n, replace = TRUE)

# Write the results to vector.
die.table <- table(die.results)

# View results.
die.table

# You are done!
# Set directory
setwd("/Users/alejandro/Documents/Formacion Profesional/R Programming/Assignment 3")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Get the first rows
head(outcome)

# Get the number of columns
ncol(outcome)

# Get the names of the columns
names(outcome)

# To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset), run

outcome[,11] <- as.numeric(outcome[,11])

# Draw Histogram

hist(outcome[, 11])

View(outcome)
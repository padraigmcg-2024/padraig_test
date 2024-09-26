# Load Require Libraries
library(tidyverse)
library(readxl)

# Set path to data file
xls_file <- "Coded_Data.xlsx"

# Read in the Excel data file
rel_distance <-read_excel(xls_file)

# Convert file to a data frame
dfdata <- data.frame(rel_distance)

# List the variables in the dataset
str(dfdata)

#Calculate summary statistics for Relative Distance
summary(dfdata$Relative_Distance)

sample.mean <- mean(dfdata$Relative_Distance)
sample.n <- length(dfdata$Relative_Distance)
sample.sd <- sd(dfdata$Relative_Distance)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha <- 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom, lower.tail=F)
print(t.score)


margin.error <- t.score * sample.se

lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error

table(dfdata$Game.Type)


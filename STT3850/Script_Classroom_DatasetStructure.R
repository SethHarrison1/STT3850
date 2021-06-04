# Functions for discovering structural information about a dataset
# Replace the URL in read.csv() with the actual URL of your dataset
# Use ?read.csv to learn more about inputting comma separated data
# Last updated 08.23.2018 @ 02:15pm

# Specify the URL of the data set and read it into R Studio

datasetname <- read.csv(file = url("http://www.appstate.edu/~thomleyje/datafiles/instructor/anthropometric.csv"))

# Structure of the dataset (#observations, variables, variable types)
str(datasetname)

# Print brief summaries of all variables in the dataset, including NA
summary(datasetname)

# List the attributes (column names, class, rownames) of the dataset
# If the rows do not have names, then the rownames are their numbers
attributes(datasetname)

# Find the row and column dimensions (the number of rows and columns)
dim(datasetname)

# Row dimension only
nrow(datasetname)
dim(datasetname)[1]

# Column dimension only
ncol(datasetname)
dim(datasetname)[2]

# Variable (column) names
names(datasetname)
attributes(datasetname)[1]

# Class of each variable
# Output is a list vector
lapply(datasetname, class)

# Class of each variable
# Output is a char vector
sapply(datasetname, class)

# Find the number of rows that contain NA (i.e., missing data)
sum(!complete.cases(datasetname))

# Find the number of rows that do not contain NA (missing data)
sum(complete.cases(datasetname))

# Print the first few rows of the dataset
# Default is six rows, input other values
head(datasetname)
head(datasetname, 5)

# Print the last few rows of the dataset
tail(datasetname)
tail(datasetname, 5)

# Print the whole dataset (be careful if it is very large!)
print(datasetname)

# Structure using the dplyr package
# How do str() and glimpse() differ?
library(dplyr)
glimpse(datasetname)
# Script for Kaggle springleaf

######################################################
# Source files and libraries
######################################################
source('source.R')

######################################################
# Raw Data
######################################################
raw <- read.csv('train.csv',header = TRUE)

######################################################
# Cleaning & Interpolation
######################################################
data.clean <- remove.na(raw) # Raw data without rows with NA entries
x.clean <- as.matrix(data.clean[,2:(ncol(data.clean)-1)],dimnames = list(rownames = data.clean[,1],colnames = names(raw)))
y.clean <- data.clean[,ncol(data.clean)]

print(dim(x.clean))
print(length(y.clean))

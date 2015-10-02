# Script for Kaggle springleaf

######################################################
# Source files and libraries
######################################################
source('source.R')

######################################################
# Raw Data
######################################################
load('test_raw.rdata')

######################################################
# Cleaning & Interpolation
######################################################
# clean_data1 <- clean(raw)
# clean_data1[,ncol(clean_data1)] <- mutateResponse(clean_data1[,ncol(clean_data1)])
# clean_data <- removeZeroes(clean_data1,nrow(clean_data1),ncol(clean_data1))
# save(clean_data, file = 'clean_data.rdata')

test1 <- clean(test_raw)
test1[,ncol(test1)] <- mutateResponse(test1[, ncol(test1)])
clean_test <- removeZeroes(test1, ncol(test1))

save(clean_test, file = 'clean_test.rdata')

######################################################
# Partition of data
######################################################


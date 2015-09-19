# Script for Kaggle springleaf

######################################################
# Source files and libraries
######################################################
source('source.R')

######################################################
# Raw Data
######################################################
raw <- load('raw_data.rdata')

######################################################
# Cleaning & Interpolation
######################################################
clean_data1 <- clean(raw)
clean_data1[,ncol(clean_data1)] <- mutateResponse(clean_data1[,ncol(clean_data1)])
clean_data <- removeZeroes(clean_data1,nrow(clean_data1),ncol(clean_data1))
save(clean_data, file = 'clean_data.rdata')

######################################################
# Partition of data
######################################################

groups <- group2six(clean_data)
one <- groups$one
one <- groups$one
one <- groups$oneone <- groups$one

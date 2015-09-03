# Source file for Kaggle  springleaf

#######################################################
# remove.na takes in data.frame or matrix and return
# a matrix omitting the rows with na entries
#######################################################
remove.na <- function(data){
    data <- as.matrix(data)
    nobs <- nrow(data); np <- ncol(data);
    index <- integer(0);
    for(i in 1:nobs){
        row <- data[i,]
        if(any(is.na(row))){
            index <- c(index,i)
        } else {
            next
        }
    }
    out <- data[-index,]
    return(out)
}

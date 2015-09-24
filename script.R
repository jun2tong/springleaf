# Script for Kaggle springleaf

######################################################
# Source files and libraries
######################################################
source('source.R')

######################################################
# Raw Data
######################################################
load('raw_data.rdata')
k <- 2

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

groups <- group2six(clean_data,shuffle = TRUE)
one <- groups$one
two <- groups$two
three <- groups$three
four <- groups$four
five <- groups$five
six <- groups$six

np <- ncol(one)
y1 <- one[,np]
x1 <- one[,-np]
y2 <- two[,np]
x2 <- two[,-np]
y3 <- three[,np]
x3 <- three[,-np]
y4 <- four[,np]
x4 <- four[,-np]
y5 <- five[,np]
x5 <- five[,-np]
y6 <- six[,np]
x6 <- six[,-np]

####################################################
# Calculating CRE  
####################################################
  betas.global <- rep(0,((k-1)*(p+1)))
  indices <- 1:6
  lambda.global <- 0
  for(i in 1:6){
    # Test set
    y_test <- get(paste('y',i,sep=''));
    x_test <- as.matrix(get(paste('x',i,sep='')));
      
    train_errors <- rep.int(x = 0, times = 41);
    
    for(j in setdiff(1:6,i)){
      # Tuning Set
      y_tune <- get(paste('y',j,sep=''));
      x_tune <- as.matrix(get(paste('x',j,sep='')));
      
      rest <- setdiff(1:6,c(i,j));
      
      y_train <- unlist(mget(paste('y',rest,sep='')));
      x_train <- as.matrix(do.call(rbind,lapply(paste('x',rest,sep=''),get)));
      
      p_prime <- ncol(x_train); n_prime <- nrow(x_train);
      
      for(ii in -20:20){
        lambda_temp <- 2^(ii)
        t_temp <- optim(par = rep(0,((k-1)*(p.prime+1))),fn = fr,gr = grr,x = x_train, y = y_train, p=p_prime, n=n_prime, k = k, lambda=lambda_temp, method="BFGS") $par;
        
        fhat_temp <- pred.vertex(x = x_tune, t = t_temp, k = k);
        
        error_temp <- length(which(fhat_temp != y_tune))/length(y_tune);
        
        train_errors[ii] <- train_errors[ii] + error_temp;
      }
      
    }
    
    lambdas <- 2^(-20:20);
    
    lambda_min <- min(train_errors);
    
    lambdahat <- lambdas[as.numeric(max(which(train_errors == lambda_min)))];
    
    cv_index <- setdiff(indices,i);
    
    y_cv <- unlist(mget(paste('y',cv_index,sep='')));
    x_cv <- as.matrix(do.call(rbind,lapply(paste('x',cv_index,sep=''),get)));
    
    n <- nrow(x_cv); p <- ncol(x_cv);
    
    betas <- optim(par = rep(0,((k-1)*(p+1))),fn = fr,gr = grr,x = x_cv, y = y_cv, p=p, n=n, k = k, lambda=lambdahat, method="BFGS") $par;
    
    ###########################################################
    # Finding Betas and refitting step
    fhat <- pred.vertex(x=x_test,t = betas, k = k)
    fhat.prob <- prob.vertex(x = x_test, t = betas, k = k)
    
    fit <- refit(x.train = x_cv, y.train = y_cv, x.test = x_test, y.test = y_test, k = k, betas = betas)
    
    refit.prob <- fit$class.probability
  }



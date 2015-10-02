<<<<<<< HEAD
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

=======
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
  indices <- 1:6
  lambda.global <- 0
  betas.global <- rep(0,((k-1)*(p+1)))
  etas.global <- rep(0,((k-1)*(k-1)))
  for(i in 1:6){
    # Test set
    y_test <- get(paste('y',i,sep=''));
    x_test <- as.matrix(get(paste('x',i,sep='')));
      
    train_errors <- rep.int(x = 0, times = 41);
    
    for(j in setdiff(1:6,i)){
      # Tuning Set
        indices <- 1:6
lambda_global <- 0
betas_global <- rep(0,((k-1)*(p+1)))
etas_global <- rep(0,((k-1)*(k-1)))
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
    betas_global <- betas_global + betas
    ###########################################################
    # Finding Betas and refitting step
    fhat <- pred.vertex(x=x_test,t = betas, k = k)
    fhat.prob <- prob.vertex(x = x_test, t = betas, k = k)
    
    fit <- refit(x.train = x_cv, y.train = y_cv, x.test = x_test, y.test = y_test, k = k, betas = betas)
    etas <- fit$etas
    etas_global<- etas_global + etas
    refit.prob <- fit$class.probability
  }

betas_global <- betas_global/6
etas_global <- etas_global/6

##########################################################################################
# Building model with test set
#####################################################################################
# Define appropriate beta and eta for test
beta_matrix <- matrix(betas_global,nrow = np+1, ncol = k-1)
beta_test <- beta_matrix[1:np,]
beta0_test <- beta_matrix[np+1,]

eta_matrix <- matrix(etas_global, nrow = k+1, ncol = k-1)
eta_test <- eta_matrix[1:k,]
eta0_test <- eta_matrox[k+1,]

# Building the model
model <- t(t(t(t(x %*% beta_test) + beta0_test) %*% eta_test) + eta0_test)

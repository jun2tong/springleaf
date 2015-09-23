# Script for Kaggle springleaf

######################################################
# Source files and libraries
######################################################
source('source.R')

######################################################
# Raw Data
######################################################
load('raw_data.rdata')

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
  errors <- rep.int(x=0,times = 4)
  indices <- 1:6
  lambda.global <- 0
  for(i in 1:6){
    # Test set
    y.test <- get(paste('y',i,sep=''));
    x.test <- as.matrix(get(paste('x',i,sep='')));
      
    train.errors <- rep.int(x = 0, times = 41);
    
    for(j in setdiff(1:6,i)){
      # Tuning Set
      y.tune <- get(paste('y',j,sep=''));
      x.tune <- as.matrix(get(paste('x',j,sep='')));
      
      rest <- setdiff(1:6,c(i,j));
      
      y.train <- unlist(mget(paste('y',rest,sep='')));
      x.train <- as.matrix(do.call(rbind,lapply(paste('x',rest,sep=''),get)));
      
      p.prime <- ncol(x.train); n.prime <- nrow(x.train);
      
      for(ii in -20:20){
        lambda.temp <- 2^(ii)
        t.temp <- optim(par = rep(0,((k-1)*(p.prime+1))),fn = fr,gr = grr,x = x.train, y = y.train, p=p.prime, n=n.prime, k = k, lambda=lambda.temp, method="BFGS") $par;
        
        fhat.temp <- pred.vertex(x = x.tune, t = t.temp, k = k);
        
        error.temp <- length(which(fhat.temp != y.tune))/length(y.tune);
        
        train.errors[ii] <- train.errors[ii] + error.temp;
      }
      
    }
    
    lambdas <- 2^(-20:20);
    
    lambda.min <- min(train.errors);
    
    lambdahat <- lambdas[as.numeric(max(which(train.errors == lambda.min)))];
    
    cv.index <- setdiff(indices,i);
    
    y.cv <- unlist(mget(paste('y',cv.index,sep='')));
    x.cv <- as.matrix(do.call(rbind,lapply(paste('x',cv.index,sep=''),get)));
    
    n <- nrow(x.cv); p <- ncol(x.cv);
    
    betas <- optim(par = rep(0,((k-1)*(p+1))),fn = fr,gr = grr,x = x.cv, y = y.cv, p=p, n=n, k = k, lambda=lambdahat, method="BFGS") $par;
    
    ###########################################################
    # Finding Betas and refitting step
    fhat <- pred.vertex(x=x.test,t = betas, k = k)
    fhat.prob <- prob.vertex(x = x.test, t = betas, k = k)
    
    fit <- refit(x.train = x.cv, y.train = y.cv, x.test = x.test, y.test = y.test, k = k, betas = betas)
    
    refit.prob <- fit$class.probability
    
    list.fit <- list.refit <- numeric(0)
    for(ii in 1:length(y.test)){
      list.fit <- c(list.fit, fhat.prob[ii,y.test[ii]])
      list.refit <- c(list.refit, refit.prob[ii,y.test[ii]])
    }
    
    err.temp <- c(- sum(log(list.fit))/length(y.test),- sum(log(list.refit))/length(y.test))
    
    errors <- errors + c(length(which(fhat!= y.test))/(6*length(y.test)),fit$error/6,err.temp)
    
    print(Sys.time())
    print(errors)
  }
  
  ave.err <- ave.err + errors
  print(ave.err)
}
ave.err <- ave.err/100



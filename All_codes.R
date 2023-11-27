### Firstly, we need to transfer our data into a numeric matrix
### This step is very important! Or the function behind will
### not work! The format must be numeric matric! List or
### dataframe will not make these functions work!
mydata <- read.csv("expenses.csv")

mydata$sex <- factor(mydata$sex, 
                     levels = c("male","female"), labels = c(0,1))
mydata$smoker <- factor(mydata$smoker, levels = c("yes","no"), labels = c(1,0))
mydata$region_northeast <- ifelse(mydata$region=="northeast",1,0)
mydata$region_northwest <- ifelse(mydata$region=="northwest",1,0)
mydata$region_southeast <- ifelse(mydata$region=="southeast",1,0)
mydata <- mydata[,-6]

mydata <- as.matrix(mydata)
mydata <- as.numeric(mydata)
class(mydata)
summary(mydata)
mydata <- matrix(mydata, ncol=9)

X <- mydata[, c(1:4,6:9)]
Y <- mydata[, 5]
# The example is from a data-set on canvas, and in this situation,
# binary response y is whether a person is a smoker, 0 represesnts
# no, and 1 represesnts yes. The other variables are predictors.

# The first step
# Create a function which can get our initial values of beta
# The method is least square method
# The function name is "init"

init <- function(resp, pred){
  a <- nrow(pred)
  c <- rep(1, nrow(pred))
  x_mat <- as.matrix(cbind(c, pred))
  beta_vec <- solve(t(x_mat)
                    %*%(x_mat))%*%t(x_mat)%*%resp
  return(beta_vec)
}

# Example
init_beta_demo <- init(resp = Y, pred= X) 
# the initial beta are (4.648504e-01, -7.873921e-03, -2.118781e-02,
# -1.046147e-02, -1.329449e-02, 2.997526e-05, -2.108683e-02,
# -1.855320e-02, 2.314974e-02)
# In this function, we need input two things, one is the 
# predictor, its format must be a numeric matrix, the other
# thing is response, it must be a numeric vector. If not, the 
# function will not work.

# Function 2
# Bootstrap Confidence intervals
# Firstly, we need construct a function of loss value
# The input are pred, resp and beta. pred must be a numeric 
# matrix, resp and beta must be numeric vectors. And this 
# function would give a value which is the value of loss
# function.
loss_value <- function(pred, resp, beta){
  a <- nrow(pred)
  c <- rep(1,a)
  x_mat <- cbind(c, pred)
  d <- rep(0,a)
  for(i in 1:a){
    d[i] <- x_mat[i,]%*%beta
  }
  loss <- rep(0,a)
  for(i in 1:a){
    loss[i] <- ((-resp[i])*log(1/(1+exp(-d[i])))-(1-resp[i])*log(exp(-d[i])/(1+exp(-d[i]))))
  }
  return(sum(loss))
}

# Example
loss_value_demo <- loss_value(pred= X,
                              resp= Y,beta=init_beta_demo)
# we get a value of 877.4893, this value if the loss value if
# we use our initial beta.


# Now,we try use the function of optim to get a demo estimate
# for the optimal beta
optim_beta_demo <- optim(init_beta_demo, fn=loss_value, 
                         pred= X,
                         resp= Y)$par
# as we can see, in this demo, we would get a vector of 9
# elements, these values would be the best estimators of beta
# which could get the minimum value of the loss function

# Now we create a function that get the Bootstrap Confidence 
# intervals of beta
# The default value of n is 20, which is the number of
# Bootstraps. 
# The default value of alpha is 0.05, which is the value of
# significance level
# The function name would be Boot_CI
Boot_CI <- function(n=20, alpha=0.05, pred, resp){
  whole <- cbind(pred, resp)
  a <- nrow(pred)
  c <- ncol(pred)+1
  b <- matrix(0, nrow=n, ncol=c)
  for (i in 1:n){
    whole_star <- whole[sample(1:a, replace = TRUE),]
    init_beta <- init(resp = whole[,c], 
                      pred= whole[, -c]) 
    b[i,] <- optim(init_beta, fn=loss_value, 
                   pred=whole_star[, -c],
                   resp=whole_star[,c])$par
  }
  final <- matrix(0, nrow = c, ncol=2)
  for(i in 1:c){
    final[i,] <- quantile(b[,i], c(alpha/2, 1 - alpha/2))
  }
  return(final)
}

# Example
Boot_CI_demo <- Boot_CI(n=20, alpha=0.05, 
                        pred= X, 
                        resp= Y)
# As we can see, in this sample, we get a matrix of 9*2, the 
# row represents the estimate values from beta_0 to beta_8,
# the columns represents the upper_bound and lower bound of
# each estimated beta. In this demo, the number of Bootstraps
# is 20 and the significance level is 0.05.

# Function 3
# We would create a function which would draw the fitted 
# logistic curve to the responses
# The y-axis is the binary response y
# The x-axis represents a sequence of values from the range 
# of fitted value y_hat, y_hat = x%*%beta
# The function name is "log_plot"
# In this function, we need to input two data, one is our
# predictor, it should be a numeric matrix. The other is
# our response, it must be a vector of binary response.
log_plot <- function(pred, resp){
  b <- rep(0, nrow(pred))
  whole <- cbind(pred, resp, b)
  c <- ncol(whole)
  # get the start value of beta
  
  h <- rep(1, nrow(pred))
  x_mat <- cbind(c, pred)
  init_beta <- solve(t(x_mat)
                     %*%(x_mat))%*%t(x_mat)%*%resp
  
  optim_beta_demo <- optim(init_beta, 
                           fn=loss_value, pred=whole[,c(1:(c-2))], resp=whole[,(c-1)])$par
  
  whole[,c] <- exp(-x_mat %*% optim_beta_demo)
  whole <- whole[order(whole[,c]),]
  plot(whole[,(c-1)] ~ whole[,c])
  aaa <- cbind(whole[,c] , whole[,(c-1)])
  aaa <- as.data.frame(aaa)
  plot(aaa[,2]~aaa[,1], xlab="fitted value y_hat"
       , ylab="binary response y")
  logistic_model <- glm(aaa[,2]~aaa[,1], data=aaa, 
                        family=binomial)
  Predicted_data <- data.frame(var2=seq(
    min(aaa[,1]), max(aaa[,1]),len=nrow(pred)))
  Predicted_data$var1 = predict(
    logistic_model, Predicted_data, type="response")
  lines(var1 ~ var2, Predicted_data, lwd=2, col="green")
}

# Example
log_plot(pred= X, 
         resp= Y)
# Therefore, we get a plot of fitted logistic curve 
# to the responses

# Function 4. Confusion Function
# The function name is "conf_mat"
# The input is still two data, one is our
# predictor, it should be a numeric matrix. The other is
# our response, it must be a vector of binary response.

conf_mat <- function(pred, resp){
  b <- rep(0, nrow(pred))
  whole <- cbind(pred, resp, b)
  c <- ncol(whole)
  # get the start value of beta
  
  h <- rep(1, nrow(pred))
  x_mat <- cbind(c, pred)
  init_beta <- solve(t(x_mat)
                     %*%(x_mat))%*%t(x_mat)%*%resp
  
  optim_beta_demo <- optim(init_beta, 
                           fn=loss_value, pred=whole[,c(1:(c-2))], resp=whole[,(c-1)])$par
  
  whole[,c] <- (1+exp(-x_mat %*% optim_beta_demo))^(-1)
  whole <- whole[order(whole[,c]),]
  aaa <- cbind(whole[,c] , whole[,(c-1)])
  
  #Create the confusion matrix
  confusion <- matrix(rep(0,9),3,3)
  for(i in 1:nrow(pred)){
    if(aaa[i,1] >= 0.5 & aaa[i,2]==1){
      confusion[2,2] <- confusion[2,2]+1
    }else if(aaa[i,1] < 0.5 & aaa[i,2]==1){
      confusion[2,3] <- confusion[2,3]+1
    }else if(aaa[i,1] > 0.5 & aaa[i,2]==0){
      confusion[3,2] <- confusion[3,2]+1
    }else{
      confusion[3,3] <- confusion[3,3]+1
    }
  }
  confusion[1,1] <- (confusion[2,2]+confusion[2,3]
                     +confusion[3,2]+confusion[3,3])
  confusion[1,2] <- (confusion[2,2]+confusion[3,2])
  confusion[1,3] <- (confusion[2,3]+confusion[3,3])
  confusion[2,1] <- (confusion[2,2]+confusion[2,3])
  confusion[3,1] <- (confusion[3,2]+confusion[3,3])
  
  
  #Print such metrics
  Prevalence <- confusion[2,1]/confusion[1,1]
  cat(sprintf("The Prevalence is %f\n", Prevalence))
  Accuracy <- (confusion[2,2]+confusion[3,3])/confusion[1,1]
  cat(sprintf("The Accuracy is %f\n", Accuracy))
  Sensitivity <- confusion[2,2]/confusion[2,1]
  cat(sprintf("The Sensitivity is %f\n", Sensitivity))
  Specificity <- confusion[3,3]/confusion[3,1]
  cat(sprintf("The Specificity is %f\n", Specificity))
  False_Discovery_Rate <- confusion[3,2]/(confusion[3,2]+confusion[2,2])
  cat(sprintf("The False Discovery Rate is %f\n", False_Discovery_Rate))
  LR_plus <- (confusion[2,2]/confusion[2,1])/(confusion[3,2]/confusion[3,1])
  LR_minus <- (confusion[2,3]/confusion[2,1])/(confusion[3,3]/confusion[3,1])
  Diagnostic_Odds_Ratio <- LR_plus/LR_minus
  cat(sprintf("The Diagnostic Odds Ratio is %f\n", Diagnostic_Odds_Ratio))
  
  return(confusion)
}

# Example
confusion_matrix_demo <- conf_mat(pred=X, 
                                  resp=Y)
# As we see, the confusion matrix is a 3*3 matrix, and we also
# out put the metrics of Prevalence, Accuracy, Sensitivity,
# Specificity, False Discovery Rate and Diagnostic Odds Ratio




# Function 5. plot of any of metrics based on different cut-off
# values from 0.1 to 0.9 with steps of 0.1

# In order to solve this problem, we need to calculate all the 
# 9 situations with the different cut-off values. This would
# be a comprehensive work. We need an array to store our 
# 9 confusion matrix and print all the metrics in all the 
# situations of different cut-off values

# the function name is "compre_cal".

# The input would still be pred and resp. Pred is our
# predictor, it should be a numeric matrix. The other is
# our response, it must be a vector of binary response.

# Firstly, we could create a new function where users could
# select cut-off values freely, The function name is
# "conf_mat_free", compared to the function "conf_mat",
# we would input another thing, it is the cut-off value

conf_mat_free <- function(pred, resp, value){
  b <- rep(0, nrow(pred))
  whole <- cbind(pred, resp, b)
  c <- ncol(whole)
  # get the start value of beta
  
  h <- rep(1, nrow(pred))
  x_mat <- cbind(c, pred)
  init_beta <- solve(t(x_mat)
                     %*%(x_mat))%*%t(x_mat)%*%resp
  
  optim_beta_demo <- optim(init_beta, 
                           fn=loss_value, pred=whole[,c(1:(c-2))], resp=whole[,(c-1)])$par
  
  whole[,c] <- (1+exp(-x_mat %*% optim_beta_demo))^(-1)
  whole <- whole[order(whole[,c]),]
  aaa <- cbind(whole[,c] , whole[,(c-1)])
  
  #Create the confusion matrix
  confusion <- matrix(rep(0,9),3,3)
  for(i in 1:nrow(pred)){
    if(aaa[i,1] >= value & aaa[i,2]==1){
      confusion[2,2] <- confusion[2,2]+1
    }else if(aaa[i,1] < value & aaa[i,2]==1){
      confusion[2,3] <- confusion[2,3]+1
    }else if(aaa[i,1] > value & aaa[i,2]==0){
      confusion[3,2] <- confusion[3,2]+1
    }else{
      confusion[3,3] <- confusion[3,3]+1
    }
  }
  confusion[1,1] <- (confusion[2,2]+confusion[2,3]
                     +confusion[3,2]+confusion[3,3])
  confusion[1,2] <- (confusion[2,2]+confusion[3,2])
  confusion[1,3] <- (confusion[2,3]+confusion[3,3])
  confusion[2,1] <- (confusion[2,2]+confusion[2,3])
  confusion[3,1] <- (confusion[3,2]+confusion[3,3])
  
  
  #Print such metrics
  cat(sprintf("When cut-off value is %f\n", value))
  Prevalence <- confusion[2,1]/confusion[1,1]
  cat(sprintf("The Prevalence is %f\n", Prevalence))
  Accuracy <- (confusion[2,2]+confusion[3,3])/confusion[1,1]
  cat(sprintf("The Accuracy is %f\n", Accuracy))
  Sensitivity <- confusion[2,2]/confusion[2,1]
  cat(sprintf("The Sensitivity is %f\n", Sensitivity))
  Specificity <- confusion[3,3]/confusion[3,1]
  cat(sprintf("The Specificity is %f\n", Specificity))
  False_Discovery_Rate <- confusion[3,2]/(confusion[3,2]+confusion[2,2])
  cat(sprintf("The False Discovery Rate is %f\n", False_Discovery_Rate))
  LR_plus <- (confusion[2,2]/confusion[2,1])/(confusion[3,2]/confusion[3,1])
  LR_minus <- (confusion[2,3]/confusion[2,1])/(confusion[3,3]/confusion[3,1])
  Diagnostic_Odds_Ratio <- LR_plus/LR_minus
  cat(sprintf("The Diagnostic Odds Ratio is %f\n", Diagnostic_Odds_Ratio))
  
  return(confusion)
}

# Example

confusion_matrix_demo2 <- conf_mat_free(pred=X, 
                                        resp=Y, value = 0.8)
# As we can see, we see when cut-off value is 0.8, we get a 
# matrix of confusion_matrix_demo2 which is a 3*3 matrix and 
# print all the metrics when cut-off value is 0.8

# This function is same as "conf_mat_free" function but does 
# not print out anything
conf_mat_new <- function(pred, resp, value){
  b <- rep(0, nrow(pred))
  whole <- cbind(pred, resp, b)
  c <- ncol(whole)
  # get the start value of beta
  
  h <- rep(1, nrow(pred))
  x_mat <- cbind(c, pred)
  init_beta <- solve(t(x_mat)
                     %*%(x_mat))%*%t(x_mat)%*%resp
  
  optim_beta_demo <- optim(init_beta, 
                           fn=loss_value, pred=whole[,c(1:(c-2))], resp=whole[,(c-1)])$par
  
  whole[,c] <- (1+exp(-x_mat %*% optim_beta_demo))^(-1)
  whole <- whole[order(whole[,c]),]
  aaa <- cbind(whole[,c] , whole[,(c-1)])
  
  #Create the confusion matrix
  confusion <- matrix(rep(0,9),3,3)
  for(i in 1:nrow(pred)){
    if(aaa[i,1] >= value & aaa[i,2]==1){
      confusion[2,2] <- confusion[2,2]+1
    }else if(aaa[i,1] < value & aaa[i,2]==1){
      confusion[2,3] <- confusion[2,3]+1
    }else if(aaa[i,1] > value & aaa[i,2]==0){
      confusion[3,2] <- confusion[3,2]+1
    }else{
      confusion[3,3] <- confusion[3,3]+1
    }
  }
  confusion[1,1] <- (confusion[2,2]+confusion[2,3]
                     +confusion[3,2]+confusion[3,3])
  confusion[1,2] <- (confusion[2,2]+confusion[3,2])
  confusion[1,3] <- (confusion[2,3]+confusion[3,3])
  confusion[2,1] <- (confusion[2,2]+confusion[2,3])
  confusion[3,1] <- (confusion[3,2]+confusion[3,3])
  return(confusion)
}


# Then, we could build a comprehensive one, it would save all
# the data into an array, and print out all the metrics in
# different situations

# The function name is "compre_cal"

compre_cal <- function(pred, resp){
  my_array <- array(NA, dim = c(3, 3, 9))
  for(i in 1:9){
    v <- (0.1)*i
    my_array[,,i] <- conf_mat_free(pred, resp, value = v)
  }
  return(my_array)
}

# Example
array_demo <- compre_cal(pred=X, 
                         resp=Y)
# Finally, we get an array of 3*3*9 dimension which stores all
# the confusion matrix in different situations. And we also
# print all the metrics in different situations. It is a 
# comprehensive calculation.

compre_cal_new <- function(pred, resp){
  my_array <- array(NA, dim = c(3, 3, 9))
  for(i in 1:9){
    v <- (0.1)*i
    my_array[,,i] <- conf_mat_new(pred, resp, value = v)
  }
  return(my_array)
}


# Now we create a function that could plot any metrics over 
# a grid of cut-off values for prediction going from 0.1 
# to 0.9 with steps of 0.1.
# The function name is "plot_metric"
# We have three input, one is our predictors, the other is
# response, the metric should be c("Prevalence", "Accuracy",
# "Sensitivity", "Specificity", "False Discovery Rate",
# "Diagnostic Odds Ratio")
plot_metric <- function(resp, pred, metric){
  data_array <- compre_cal_new(pred, resp)
  if(metric == "Prevalence"){
    prevalence <- rep(0, 9)
    step <- seq(0.1, 0.9, by = 0.1)
    for(i in 1:9){
      prevalence[i] <- (data_array[2,1,i]/data_array[1,1,i])
    }
    plot(prevalence~step, xlab="cut-off value"
         , ylab="Prevalence")
  }
  if(metric == "Accuracy"){
    Accuracy <- rep(0, 9)
    step <- seq(0.1, 0.9, by = 0.1)
    for(i in 1:9){
      Accuracy[i] <- ((data_array[2,2,i]+data_array[3,3,i])/data_array[1,1,i])
    }
    plot(Accuracy~step, xlab="cut-off value"
         , ylab="Accuracy")
  }
  if(metric == "Sensitivity"){
    Sensitivity <- rep(0, 9)
    step <- seq(0.1, 0.9, by = 0.1)
    for(i in 1:9){
      Sensitivity[i] <- (data_array[2,2,i]/data_array[2,1,i])
    }
    plot(Sensitivity~step, xlab="cut-off value"
         , ylab="Sensitivity")
  }
  if(metric == "Specificity"){
    Specificity <- rep(0, 9)
    step <- seq(0.1, 0.9, by = 0.1)
    for(i in 1:9){
      Specificity[i] <- (data_array[3,3,i]/data_array[3,1,i])
    }
    plot(Specificity~step, xlab="cut-off value"
         , ylab="Specificity")
  }
  if(metric == "The False Discovery Rate"){
    The_False_Discovery_Rate <- rep(0, 9)
    step <- seq(0.1, 0.9, by = 0.1)
    for(i in 1:9){
      The_False_Discovery_Rate[i] <- (data_array[3,2,i]/(data_array[3,2,i]+data_array[2,2,i]))
    }
    plot(The_False_Discovery_Rate~step, xlab="cut-off value"
         , ylab="The False Discovery Rate")
  }
  if(metric == "The Diagnostic Odds Ratio"){
    The_Diagnostic_Odds_Ratio <- rep(0, 9)
    step <- seq(0.1, 0.9, by = 0.1)
    vec_1 <- rep(0,9)
    vec_2 <- rep(0,9)
    for(i in 1:9){
      vec_1[i] <- (data_array[2,2,i]/data_array[2,1,i])/(data_array[3,2,i]/data_array[3,1,i])
      vec_2[i] <- (data_array[2,3,i]/data_array[2,1,i])/(data_array[3,3,i]/data_array[3,1,i])
      The_Diagnostic_Odds_Ratio[i] <- (vec_1[i]/vec_2[i])
    }
    plot(The_Diagnostic_Odds_Ratio~step, xlab="cut-off value"
         , ylab="The Diagnostic Odds Ratio")
  }
}

plot_metric(resp=Y, pred=X, metric = "Prevalence")
# Now, we get a plot of Prevalence

plot_metric(resp=Y, pred=X, metric = "Accuracy")
# Now, we get a plot of Accuracy

plot_metric(resp=Y, pred=X, metric = "Sensitivity")
# Now, we get a plot of Sensitivity

plot_metric(resp=Y, pred=X, metric = "Specificity")
# Now, we get a plot of Specificity

plot_metric(resp=Y, pred=X, metric = "The False Discovery Rate")
# Now, we get a plot of The False Discovery Rate

plot_metric(resp=Y, pred=X, metric = "The Diagnostic Odds Ratio")
# Now, we get a plot of The Diagnostic Odds Ratio

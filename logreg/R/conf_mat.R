#' @title Confusion Matrix
#'
#' @description This function will create a "confusion matrix" using a cut-off value for prediction at 0.5. It will also print the Prevalence, Accuracy, Sensitivity, Specificity, False Discovery Rate, and Diagnostic Odds Ratio metrics when run which can be calculated by values within the matrix.
#' @param pred A \code{matrix}(numeric) containing variables that will be used to predict the response.
#' @param resp A \code{vector}(numeric) containing values that represent a response variable.
#' @return A \code{matrix} that gives the confusion matrix which represents the performance of the logistic regression run in this package.
#' @author Yizhong Zhang, Alexander Miller
#' @importFrom stats
#' @export
#' @examples
#' mydata <- read.csv("expenses.csv")
#' mydata$sex <- factor(mydata$sex, levels = c("male","female"), labels = c(0,1))
#' mydata$smoker <- factor(mydata$smoker, levels = c("yes","no"), labels = c(1,0))
#' mydata$region_northeast <- ifelse(mydata$region=="northeast",1,0)
#' mydata$region_northwest <- ifelse(mydata$region=="northwest",1,0)
#' mydata$region_southeast <- ifelse(mydata$region=="southeast",1,0)
#' mydata$region_northeast <- ifelse(mydata$region=="northeast",1,0)
#' mydata$region_northwest <- ifelse(mydata$region=="northwest",1,0)
#' mydata$region_southeast <- ifelse(mydata$region=="southeast",1,0)
#' mydata <- mydata[,-6]
#' mydata <- as.matrix(mydata)
#' mydata <- as.numeric(mydata)
#' mydata <- matrix(mydata, ncol=9)
#' X <- mydata[, c(1:4,6:9)]
#' Y <- mydata[, 5]
#' conf_mat(pred = X, resp = Y)
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

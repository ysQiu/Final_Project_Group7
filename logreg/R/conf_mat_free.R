#' @title Confidence Interval Free
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

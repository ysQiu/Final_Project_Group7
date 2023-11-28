#' @title Loss Function
#'
#' @description This is the "loss function" that can be used to optimize the chosen beta's based on the value given by this function. This will be used for logistic regression.
#' @param resp A \code{vector}(numeric) containing values that represent a response variable.
#' @param pred A \code{matrix}(numeric) containing variables that will be used to predict the response.
#' @param beta A \code{vector}(numeric) containing beta's or coefficients for each variable in the pred matrix that predict the resp vector.
#' @return A \code{value}(numeric) computed by this loss function.
#' @author Yizhong Zhang, Alexander Miller
#' @importFrom stats
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

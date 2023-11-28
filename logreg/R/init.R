#' @title Starting Values for Beta
#'
#' @description This function creates a vector to be the starting point for optimization using the least squares formula.
#' @param resp A \code{vector}(numeric) containing values that represent a response variable.
#' @param pred A \code{matrix}(numeric) containing variables that will be used to predict the response.
#' @return A \code{vector} containing beta's or coefficients for each variable in the pred matrix that predict the resp vector.
#'
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
#' init_beta_demo <- init(resp = Y, pred= X)
init <- function(resp, pred){
  a <- nrow(pred)
  c <- rep(1, nrow(pred))
  x_mat <- as.matrix(cbind(c, pred))
  beta_vec <- solve(t(x_mat)
                    %*%(x_mat))%*%t(x_mat)%*%resp
  return(beta_vec)
}

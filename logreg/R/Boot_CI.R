#' @title Bootstrap Confidence Intervals
#'
#' @description This function will conduct a bootstrap routine for each predictor or beta value for the logistic regression curve. The user is able to choose the amount of bootstraps to conduct as well as the confidence level for the intervals.
#' @param n A \code{value}(integer) which will be the number of bootstraps run in the function. The default value is 20.
#' @param alpha A \code{value}(numeric) that can be between 0 and 1, but is usually closer to 0. The default value is 0.05.
#' @param pred A \code{matrix}(numeric) containing variables that will be used to predict the response.
#' @param resp A \code{vector}(numeric) containing values that represent a response variable.
#' @return A \code{matrix}(numeric) where the number of rows is equal to the amount of betas and 2 columns. Each row represents a confidence interval for each predictor at the chosen confidence level.
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
#' Boot_CI_demo <- Boot_CI(n=20, alpha=0.05, pred= X, resp= Y)
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

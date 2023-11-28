#' @title Plot of the Logistic Curve
#'
#' @description This function will plot the optimized logistic curve for the given data.
#' @param pred A \code{matrix}(numeric) containing variables that will be used to predict the response.
#' @param resp A \code{vector}(numeric) containing values that represent a response variable.
#' @return A \code{plot} that shows the fitted logistic curve to the responses, where the y-axis is the binary response y while the x-axis represents a sequence of values from the range of fitted values from the logistic regression.
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
#' log_plot(pred= X, resp= Y)
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

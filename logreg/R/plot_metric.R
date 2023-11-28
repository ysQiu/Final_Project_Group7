#' @title Metric Plot
#'
#' @description This function conducts logistic regression on data and plots a given metric that visualizes the performance of the regression.
#' @param pred A \code{matrix}(numeric) containing variables that will be used to predict the response.
#' @param resp A \code{vector}(numeric) containing values that represent a response variable.
#' @param metric This can be any of the following metrics: "Prevalence", "Accuracy","Sensitivity", "Specificity", "False Discovery Rate","Diagnostic Odds Ratio"
#' @return A \code{plot} that shows
#' @author Yizhong Zhang, Alexander Miller
#' @importFrom
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
#' plot_metric(resp=Y, pred=X, metric = "Prevalence")
#' plot_metric(resp=Y, pred=X, metric = "Accuracy")
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

#' @title A function used to get interval estimation of linear regression parameters
#' @description This function is used to calculate the interval estimation of linear regression parameters
#' @importFrom stats qt
#' @param fm The regression model calculated by function lm
#' @param alpha  alpha is the confidence level
#' @return  confidence intervals of linear regression parameters and the left(right) is the left(right) endpoint of the confidence interval
#' @examples
#' x<-c(0.10, 0.11, 0.12, 0.13, 0.14, 0.15,0.16, 0.17, 0.18, 0.20, 0.21, 0.23)
#' y<-c(42.0, 43.5, 45.0, 45.5, 45.0, 47.5,49.0, 53.0, 50.0, 55.0, 55.0, 60.0)
#' lm.sol<-lm(y ~ 1+x+I(x^2))
#' getinterval(lm.sol)
#' @export
getinterval <- function(fm,alpha=0.05){
  A<-summary(fm)$coefficients
  df<-fm$df.residual
  left<-A[,1]-A[,2]*qt(1-alpha/2, df)
  right<-A[,1]+A[,2]*qt(1-alpha/2, df)
  rowname<-dimnames(A)[[1]]
  colname<-c("Estimate", "Left", "Right")
  matrix(c(A[,1], left, right), ncol=3,
         dimnames = list(rowname, colname ))
}


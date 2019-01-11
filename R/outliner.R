#' @title A function used to draw the outline graph of P variables
#' @description This function is used to draw the outline graph of P variables
#' @importFrom  graphics lines plot
#' @param x is a matrix or a dataframe
#' @return the outline graph of P variables
#' @examples
#' x <- replicate(4,sample(seq(60,100),10))
#' outliner(x)
#' @export
outliner <- function(x){
  if (is.data.frame(x) == TRUE)
    x <- as.matrix(x)
  m <- nrow(x); n <- ncol(x)
  plot(c(1,n), c(min(x),max(x)), type = "n",
       main = "The outline graph of Data",
       xlab = "Number", ylab = "Value")
  for(i in 1:m){
    lines(x[i,], col=i)
  }
}


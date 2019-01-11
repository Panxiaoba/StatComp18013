#' @title A function used to get the root by the method of bisection
#' @description This function is used to calculate the root of a equation in a interval and you need promise the signs at both ends of the interval are opposite or it will fail
#' @param f  the equation which We need to find  the root
#' @param a  The right endpoint of the interval at the root of the equation
#' @param b The right endpoint of the interval at the root of the equation
#' @param eps Iteration is stopped when both ends of the interval are less than eps
#' @return a root of the equation
#' @examples
#' f<-function(x) x^3-x-1
#' fzero(f, 1, 2, 1e-6)
#' \dontrun{
#' fzero(f, 0, 1, 1e-6)
#' }
#' @export
fzero <- function(f, a, b, eps=1e-5){
  if (f(a)*f(b)>0)
    list(fail="finding root is fail!")
  else{
    repeat {
      if (abs(b-a)<eps) break
      x <- (a+b)/2
      if (f(a)*f(x)<0) b<-x else a<-x
    }
    list(root=(a+b)/2, fun=f(x))
  }
}


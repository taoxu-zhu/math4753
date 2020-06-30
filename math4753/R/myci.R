#' course 4753
#'
#' Use rnorm to find confident interval
#'
#' @param x  to find confident interval.
#'
#' @return two values for confident interval.
#'
#' @examples
#' x = rnorm(30,mean=10,sd=12)
#' myci(x)
#'
#'
#'
#'@export
myci <- function(x){
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  e <- qt(0.975,n-1)*sd/sqrt(n)
  lower <- mean-e
  higher <- mean+e
  ci <- c(lower,higher)
  ci
}

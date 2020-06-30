#' course 4753
#'
#' making a plot with the value of n.
#'
#' @param x making a plot with the value of n.
#'
#' @return a histogram of the data provide.
#'
#' @examples
#' newfunction(n=10,iter=1000)
#'
#'
#' @export
newfunction <- function(n,iter){y=runif(n*iter,0,5)
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
sm=apply(data,2,sum)
hist(sm)
sm
}

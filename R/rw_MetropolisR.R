#' @title A random walk Metropolis sampler using R
#' @description A random walk Metropolis sampler for generating the standard Laplace distribution using R
#' @importFrom stats rnorm runif
#' @param sigma given standard deviation
#' @param x0 the initial value given
#' @param N the length of the resulting chain
#' @return a list of a chain of size \code{n} and the accepte rate during the process of using this method
#' @examples
#' \dontrun{
#' N <- 2000
#' rwk <- rw_MetropolisR(2,20,N)
#' plot(rwk$x,type='l')
#' accept.rate <- rwk$k/N
#' }
#' @export
rwM.gen.chain <- function(sigma,x0,N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    r <- exp(abs(x[i-1])-abs(y))
    if (u[i] <= r){
      x[i] <- y 
      k <- k + 1
    } else 
      x[i] <- x[i-1]
  }
  return(list(x=x,accept=k/N )) 
  # accept:the acceptance rates of chain
}

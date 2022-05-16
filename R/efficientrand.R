#' En funktion til beregning af efficient rand
#'
#' @param mu En vektor med forventede afkastrater
#' @param sigma covariansmatrix
#'
#' @return En tekststreng der specificerer sammenhæng mellem en porteføljes varians og dens forventede afkast. output skal læses som at mu^i = koefficienten foran den i'te potens af forventet afkast
#' @export
#'
#' @examples mu <- c(0.03,0.05,0.06)
#' sigma <- matrix(c(0.04,0.01,0.01,0.01,0.09,0.01,0.01,0.01,0.16), ncol =3)
#' efficientrand(mu,sigma)
efficientrand <- function(mu,sigma){
  mu1 <- matrix(c(mu,rep(1,length(mu))),ncol = 2)
  A <- t(mu1)%*%solve(sigma)%*%mu1
  a <- A[1,1]
  b <- A[1,2]
  c <- A[2,2]
  denom <- a*c-b^2
  return(paste("mu^0 =", a/denom, ",mu^1 =" ,-2*b/denom, ",mu^2 =", c/denom, "          ", "sigma_gmv =" ,1/sqrt(c), ",mu_gmv =", b/c))
}

#' Beregner porteføljevægte for minimum varians-portefølje givet en specificeret afkastrate
#'
#' @param mu En vektor med forventede afkastrater
#' @param sigma covariansmatrix
#' @param mu_p Krævet afkastrate
#'
#' @return En vektor af porteføljevægte 
#' @export
#'
#' @examples mu <- c(0.03,0.05,0.06)
#' sigma <- matrix(c(0.04,0.01,0.01,0.01,0.09,0.01,0.01,0.01,0.16), ncol =3)
#' mu_p <-0.08
pfweights <- function(mu,sigma,mu_p){
  mu1 <- matrix(c(mu,rep(1,length(mu))),ncol = 2)
  A <- t(mu1)%*%solve(sigma)%*%mu1
  return(solve(sigma)%*%mu1%*%solve(A)%*%c(mu_p,1))
}
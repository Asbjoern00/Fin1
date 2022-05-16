#' En funktion til beregning af vægtene for tangentporteføljen
#'
#' @param mu En vektor med forventede afkastrater
#' @param sigma covariansmatrix
#' @param riskfree renten på det risikofri aktiv
#'
#' @return vægtene for tangentporteføljen
#' @export
#'
#' @examples mu <- c(0.03,0.05,0.06)
#' sigma <- matrix(c(0.04,0.01,0.01,0.01,0.09,0.01,0.01,0.01,0.16), ncol =3)
#' riskfree <- 0.01
#' tangentpf(mu, sigma,riskfree)
#'
tangentpf <- function(mu, sigma,riskfree){
  w <- (1/(t(rep(1,length(mu)))%*%solve(sigma)%*%(mu-riskfree))[1,1]*(solve(sigma)%*%(mu-riskfree)))
  return(as.vector(w))
}

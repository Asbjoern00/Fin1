#' En funktion til beregning af beta som det fremg?r i prop 5/6
#'
#' @param sigma covariansmatrix
#' @param mv v?gte for markedsportef?ljen
#' @param pf v?gte for portef?ljen man er interesseret i at unders?ge
#' @return beta
#' @export
#'
#' @examples sigma <- matrix(c(.25,.10,.10,.36),ncol=2)
#' tangent <- c(0.2857143, 0.7142857)
#' betaprop5(sigma, tangent, c(1,0))
betaprop5 <- function(sigma, mv, pf){
  sigmamv <- t(mv)%*%sigma%*%mv
  covar <- t(mv)%*%sigma%*%pf
  beta <- covar/sigmamv
  return(beta[1,1])
}

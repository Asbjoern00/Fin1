#' Beregner sandsynlighedsvægtet covarians
#'
#' @param x Vektor
#' @param y Vektor, hvis denne udelades, sættes y <- x, og man får derved varians
#' @param p Vektor af sandsynlighedsvægte
#'
#' @return vægtene for tangentporteføljen
#' @export
#'
#' @examples A1 <- c(56,52,51,50,48)
#' probs <- c(.1,.2,.4,.2,.1)
#' A1_0 <- 50
#' weighted.covar(A1/A1_0,p=probs)
#'
weighted.covar <- function(x,y,p){
  if(missing(y)){
    y <- x
  }
  sum(x*y*p) - sum(x*p)*sum(y*p)
}

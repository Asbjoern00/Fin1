#' Beregner q-sandsynligheder
#'
#' @param S0 Kurs til t=0
#' @param Sup Kurs i up tilstand t=1
#' @param Sdown Kurs i down tilstand t=1
#' @param r risikofrirente
#'
#' @return tibble med u,d,q
#' @import tibble
#' @export
#'
#' @examples qprop(100,120,80,0)


qprop <- function(S0,Sup,Sdown,r=0){
  u <- Sup/S0
  d <- Sdown/S0
  R <- 1+r
  q <- (R-d)/(u-d)
  return(tibble(u,d,q))
}



#' Beregner binomialtræ for aktiepriser
#'
#' @param S_0 Kurs til t=0
#' @param alpha alphaparameter til st.binomialmodel
#' @param sigma volparameter i st.binomialmodel
#' @param deltat tidsskridt i modellen
#' @param N antal tidsskridt i modellen
#'
#' @return matrix med binomialtræet
#' @export
#'
#' @examples sigma <- 0.2
#' alpha <- 0.07
#' deltat <- 0.25
#' S_0 <- 100
#' N <- 4
#' genbinomtree(S_0,alpha,sigma,deltat,N)
genbinomtree <- function(S_0,alpha,sigma,deltat,N){
  u <- exp(alpha*deltat+sigma*sqrt(deltat))
  d <- exp(alpha*deltat-sigma*sqrt(deltat))
  tree <- matrix(0,N+1,N+1)
  for(i in 1:(N+1)){
    for(j in (N+1):(N+2-i)){
      tree[j,i] <- S_0*u^((N+1)-j)*d^(i-1-((N+1)-j))
    }
  }
  return(tree)
}

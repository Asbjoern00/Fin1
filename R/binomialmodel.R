#' En funktion til beregning af forventet afkast givet en binomialmodel
#'
#' @param prisslut En vektor med priser ved sidste tidspunkt
#' @param sandsynlighed er for indeværende sat til 0.5 (fremtidig udvikling)
#'
#' @return En tekststreng med spredning og forventning
#' @export
#'
#' @examples binomialmodel(c(160,131,107.25,87.81,71.89))
binomialmodel <- function(prisslut, sandsynlighed=0.5){
sandsynligheder <- c()
p <- sandsynlighed

for (i in 0:length(prisslut)-1){
  sandsynligheder[i+1] <- stats::dbinom(i, length(prisslut)-1,p)
}
forventning <- t(sandsynligheder) %*% prisslut
varians <- t(sandsynligheder) %*% (forventning[1,1]-prisslut)^2
spredning <- sqrt(varians)

return(paste("Forventningen til prisen ved sluttidspunktet er", forventning[1,1], ". Spredningen er", spredning[1,1]))
}

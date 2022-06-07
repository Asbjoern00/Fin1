#' Beregner prisen på en option givet dens payoffstruktur
#'
#' @param terminal payoff til sidste tidspunkt, oppefra og ned
#' @param q vektor af q-sandsynligheder - angives seperat for hver delmodel, oppefra og ned, startende fra terminal-tidspunkt og gaaende mod venstre i gitteret
#' @param r vektor af korte risikofrie renter i hver delmodel - angives seperat for hver delmodel, oppefra og ned, startende fra terminal-tidspunkt og gaaende mod venstre i gitteret
#' @param pathdependent logisk. skal gitteret udfoldes? Bruges hvis optionens payoff er stiafhaengigt. I dette tilfælde skal der angives terminal-vaerdi for optionen i alle omega'er
#' @param periods Antal perioder i modellen. skal kun angives hvis pathdependent = T
#' @return matrix med priser på optionen
#' @export
#'
#' @examples
#'
#'r <- c(0.075,0.024976,0.05)
#'q <- c(0.5,0.3408437,0.5)
#'periods <- 3
#'terminal <- c(69,14,14,0)
#'optiontree(terminal,r,q,TRUE,3)
#' ##Det er instruktivt at se at dette er det samme som
#'terminal <- c(69,14,0)
#'optiontree(terminal,r,q)
optiontree <- function(terminal,r,q,pathdependent=F,periods=NULL){
  if(pathdependent==F){
    opttree <- matrix(0,ncol = length(terminal),nrow=length(terminal))
    opttree[,length(terminal)] <- terminal
    dummy <- 1
    runsum <- 0
    for(i in ((length(terminal)-1):1)){
      runsum <- runsum+i
      interval <- c((runsum-i+1):runsum)
      qtemp <- q[interval]
      rtemp <- r[interval]
      for(j in 1:i){
        opttree[j+dummy,i] <- (opttree[j+dummy,i+1]*(1-qtemp[j])+opttree[j+dummy-1,i+1]*qtemp[j])/(rtemp[j]+1)
      }
      dummy <- dummy+1
    }
  }
  else{
    opttree <- matrix(0,nrow=length(terminal),ncol = periods)
    opttree[,periods] <- terminal
    for(i in ((periods-1):1)){
      qtemp <- rep(0,periods^2)
      rtemp <- rep(0,periods^2)
      jint <- (length(terminal)-2^(i-1)+1):length(terminal)
      if(i == periods-1){
        index <- 1:length(jint)
      }else{
        index <- (max(index)+1):(max(index)+length(jint))
      }
      qtemp[jint] <- q[index]
      rtemp[jint] <- r[index]
      dummy <- length(terminal)-2^(i)+1
      for(j in jint){
        margin <- dummy:(dummy+1)
        opttree[j,i] <- (opttree[margin[1],i+1]*(qtemp[j])+opttree[margin[2],i+1]*(1-qtemp[j]))/(rtemp[j]+1)
        dummy <- dummy+2
      }
    }
  }
  return(opttree)
}

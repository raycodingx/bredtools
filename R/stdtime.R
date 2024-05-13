## Given time points covariate and order of fit for Legendre polynomials, return matrix 'M' containing the polynomials of standardized time. 'M' is order t (number of time points) by k (order of Legendre polynomials).

## Arguments
##	t: a vector of time, age or days
## 	n: order of polynomials
##      tmax: max time (optional)
##      tmin: min time (optional)

## Literature: Mrode, R.A. 2005. Linear Models for the Prediction of Animal Breeding Values. CAB International, Oxon, UK.

## Author: Gota Morota <morota at wisc dot edu>
## Created: 31-Mar-2010
## Last-Modified: 2-Apr-2010
## License: GPLv3 or later

#' Given time points covariate and order of fit for Legendre polynomials, return matrix 'M' containing the polynomials of standardized time. 'M' is order t (number of time points) by k (order of Legendre polynomials).
#'
#' @param t a vector of time, age or days
#' @param n order of polynomials
#' @param tmax max time (optional)
#' @param tmin min time (optional)
#'
#' @return matrix 'M' containing the polynomials of standardized time
#' @export
stdtime <-
  function(t, n, tmax, tmin){
    if(missing(tmax)) {
      tmax <- t[which.max(t)]
    }
    if(missing(tmin)) {
      tmin <- t[which.min(t)]
    }

    N <- n+1
    M <- matrix(0, nrow=length(t), ncol=N)
    a <- -1 + 2*(t-tmin)/(tmax - tmin)
    M[,1] <- 1

    for (i in 2:N){
      M[,i] <- a^(i-1)
    }

    return(M)
  }

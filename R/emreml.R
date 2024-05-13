## Given the components of MME, estimate variance components by EM Algorithm.

## Arguments
## Ainv: an inverse of additive relationship matrix
## y: a vector of response
## X: a incidence matrix for fixed effects
## Z: a incidence matrix for random effects
## initE: initial value for the residual variance
## initU: initial value for the additive genetic variance
## disp: a logical value. If true, display estimates of variance components at each iteration.

## Note
## Unknown parents should be coded as zero.

## Literature 1: Suzuki, M. 2007. Applied Animal Breeding & Genetics. Course Notes. Obihiro University of Agriculture and Veterinary Medicine.
## Literature 2: Mrode, R.A. 2005. Linear Models for the Prediction of Animal Breeding Values. CAB International, Oxon, UK.

## Author: Gota Morota <morota at wisc dot edu>
## Create: 6-Apr-2010
## Last-Modified: 8-Apr-2010
## License: GPLv3 or later

#' Given the components of MME, estimate variance components by EM Algorithm.
#'
#' @param Ainv an inverse of additive relationship matrix
#' @param y a vector of response
#' @param X an incidence matrix for fixed effects
#' @param Z an incidence matrix for random effects
#' @param initE initial value for the residual variance
#' @param initU initial value for the additive genetic variance
#' @param disp a logical value. If true, display estimates of variance components at each iteration.
#'
#' @return a vector of the residual variance and the additive genetic variance
#' @export
#'
emreml <-
  function(Ainv, y, X, Z, initE, initU, disp){

    # MME
    n <- length(y)
    Xpy <- t(X)%*%y
    Zpy <- t(Z)%*%y
    XpX <- t(X)%*% X
    XpZ <- t(X)%*%Z
    ZpX <- t(Z)%*%X
    ZpZ <- t(Z)%*%Z
    RHS <- c(Xpy, Zpy)
    oldE <- initE
    oldU <- initU
    rankX <- qr(X,LAPACK=TRUE)$rank
    rankA <- qr(Ainv,LAPACK=TRUE)$rank

    lhsRow <- length(RHS)
    z <- length(RHS) - dim(ZpZ)[1] + 1

    diff1 <- 1
    diff2 <- 1
    i <- 0
    while (diff1 > 10E-6 & diff2 > 10E-6){
      i <- i+1
      alpha <- as.vector((oldE/oldU))
      LHS <- rbind( cbind(XpX, XpZ), cbind(ZpX, ZpZ+Ainv*alpha ) )
      B <- solve(LHS)%*%RHS
      e <-  y - cbind(X,Z)%*%B
      sig2E <- (t(e)%*%y)/(n-rankX)
      c22 <- solve(LHS)[z:lhsRow, z:lhsRow]
      u <- B[z:length(B)]
      # sum(Ainv*c22) is same as sum(diag(Ainv%*%c22))
      sig2U <- (t(u)%*%Ainv%*%u + sum(Ainv*c22)*oldE )/(rankA)
      diff1 <- abs(sig2E-oldE)
      diff2 <- abs(sig2U-oldU)
      if (disp == TRUE){
        cat('\n')
        cat("iteration ", i, '\n')
        #cat("oldE", oldE, '\n')
        cat("sig2E", sig2E, '\n')
        #cat("oldU", oldU, '\n')
        cat("sig2U", sig2U, '\n')
      }
      oldE <- sig2E
      oldU <- sig2U
    }

    cat('\n')
    return(list( "sigma2E"=sig2E, "sigma2U"=sig2U   ))

  }


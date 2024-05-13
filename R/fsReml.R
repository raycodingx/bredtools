## Given the components of MME, estimate variance components by REML in the Fisher's Scoring framework.

## Arguments
## A: an additive relationship matrix
## y: a vector of response
## X: an incidence matrix for fixed effects
## Z: an incidence matrix for random effects
## initE: initial value for the residual variance
## initU: initial value for the additive genetic variance
## disp: a logical value. If true, display estimates of variance components at each iteration.

## Note

## Literature 1: Tsuruta, S. 2006. Estimation of Variance Components in Animal Breeding. The University of Georgia.
## Literature 2: Mrode, R.A. 2005. Linear Models for the Prediction of Animal Breeding Values. CAB International, Oxon, UK.

## Author: Gota Morota <morota at wisc dot edu>
## Create: 7-Apr-2010
## Last-Modified: 8-Apr-2010
## License: GPLv3 or later

#' Given the components of MME, estimate variance components by REML in the Fisher's Scoring framework.
#'
#' @param A an additive relationship matrix
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
fsReml <-
  function(A, y, X, Z, initE, initU, disp){

    N <- length(y)
    Ze <- diag(N)
    Var <- c(initU, initE)
    I <- matrix(0, ncol=2, nrow=2)
    s <- matrix(0, ncol=1, nrow=2)
    diff1 <- 10
    diff2 <- 10

    i <- 0
    while (diff1 > 10e-7 & diff2 > 10e-7 ){

      i <- i + 1

      G <- A*Var[1]
      R <- Ze%*%t(Ze)*Var[2]
      V <- Z%*%G%*%t(Z) + R
      Vinv <- solve(V)
      P <- Vinv - Vinv%*%X%*%solve(t(X)%*%Vinv%*%X)%*%t(X)%*%Vinv

      I[1,1] <- sum(diag((P%*%Z%*%t(Z)%*%P%*%Z%*%t(Z) )))
      I[1,2] <- sum(diag((P%*%Z%*%t(Z)%*%P%*%Ze%*%t(Ze) )))
      I[2,1] <- I[1,2]
      I[2,2] <- sum(diag((P%*%Ze%*%t(Ze)%*%P%*%Ze%*%t(Ze) )))
      s[1,1] <- sum(diag((P%*%Z%*%t(Z) ))) - (t(y)%*%P%*%Z%*%t(Z)%*%P%*%y )
      s[2,1] <- sum(diag((P%*%Ze%*%t(Ze) ))) - (t(y)%*%P%*%Ze%*%t(Ze)%*%P%*%y )

      newVar <- Var - solve(I)%*%s
      diff1 <- abs(Var[1] - newVar[1])
      diff2 <- abs(Var[2] - newVar[2])
      Var <- newVar
      if (disp == TRUE){
        cat('\n')
        cat("iteration ", i, '\n')
        cat("sig2U", Var[1], '\n')
        cat("sig2E", Var[2], '\n')
      }

    }

    cat('\n')
    return(list( "sigma2U"=Var[1], "sigma2E"=Var[2]   ))

  }

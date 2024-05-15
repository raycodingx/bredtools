## Given the components of MME, perform Gibbs sampler.

## Arguments
## Ainv: an inverse of additive relationship matrix
## y: a vector of response
## X: a incidence matrix for systematic effects
## Z: a incidence matrix for individual effects
## N: a number of gibbs samples
## burnin: a number of burnin
## inits: a vecotor of initial values for systematic and individual effects
## varE: initial value for residual variance
## varA: initial value for additive genetic variance
## ve: a degree of belief for residual variance
## va: a degree of belief for additive genetic variance
## s2e: a prior value for the residual variance
## s2a: a prior value for the additive genetic variance
## disp: a logical value. If true, display estimates of variance components at each iteration.

## Note
## Unknown parents should be coded as zero.
## If you don't have a prior knowlodge, you may set ve or va to -2, and s2e or s2a to 0.

## Literature 1: Mrode, R.A. 2005. Linear Models for the Prediction of Animal Breeding Values. CAB International, Oxon, UK.
## Literature 2: Misztal, I. 2008. Computational Techniques in Animal Breeding. Course Notes. University of Georgia.

## Author: Gota Morota <morota at wisc dot edu>
## Create: 4-Apr-2010
## Last-Modified: 5-Apr-2010
## License: GPLv3 or later

#' Given the components of MME, perform Gibbs sampler.
#'
#' @param Ainv an inverse of additive relationship matrix
#' @param y Given the components of MME, perform Gibbs sampler.
#' @param X incidence matrix for systematic effects
#' @param Z a incidence matrix for individual effects
#' @param N a number of gibbs samples
#' @param burnin a number of burnin
#' @param inits a vecotor of initial values for systematic and individual effects
#' @param varE initial value for residual variance
#' @param varA initial value for additive genetic variance
#' @param ve a degree of belief for residual variance
#' @param va a degree of belief for additive genetic variance
#' @param s2e a prior value for the residual variance
#' @param s2a a prior value for the additive genetic variance
#' @param disp a logical value. If true, display estimates of variance components at each iteration.
#'
#' @return a list of result
#' @export
#'
gibbs <-
  function(Ainv, y, X, Z, N, burnin, inits, varE, varA, ve, va, s2e, s2a, disp){

    for (i in 1:length(inits)){
      param <- paste("a",  i, sep="" )
      assign(param, inits[i])
    }

    XpX <- t(X)%*%X
    XpZ <- t(X)%*%Z
    ZpX <- t(Z)%*%X
    ZpZ <- t(Z)%*%Z
    Xpy <- t(X)%*%y
    Zpy <- t(Z)%*%y
    LHS <- rbind(cbind(XpX, XpZ), cbind(ZpX, ZpZ+Ainv*(varE/varA) ) )
    RHS <- rbind(Xpy, Zpy)
    nr <- length(y)
    nfix <- dim(X)[2]
    nrandom <- dim(Z)[2]
    nanim <- nrandom
    ndim <- nfix+nrandom
    Total <- burnin + N

    if (length(inits) != dim(LHS)[2]){
      stop("not enough initials")
    }

    for (i in 1:Total){

      for (j in 1:ndim){
        tmp <- 0
        for(k in 1:ndim){
          if (k !=j){
            tmp <- tmp + LHS[j,k]* (get((paste("a", k, sep=""))) )[length(get((paste("a", k, sep=""))))]
          }
        }
        mean <- (RHS[j]-tmp)/LHS[j,j]
        var <- varE[length(varE)]/LHS[j,j]
        new <- rnorm(1,mean, sqrt(var))
        z <- 0
        assign( paste("a", j, sep=""), { z <- get(paste("a", j, sep="")); z[i] <- new; z } )

      }

      B <- 0
      for(k in 1:ndim){
        #print(get((paste("a", k, sep="")))[length(get((paste("a", k, sep=""))))])
        B <-  c(B, get((paste("a", k, sep="")))[length(get((paste("a", k, sep=""))))]  )
      }
      B <- B[2:length(B)]
      e <- y - cbind(X,Z)%*%B
      epe <- t(e)%*%e
      varE[i] <- (epe + ve*s2e)/rchisq(1,(nr+ve))
      upu <- t(matrix(  B[(nfix+1):ndim])) %*% Ainv %*% matrix(  B[(nfix+1):ndim])
      varA[i] <- (upu + va*s2a)/rchisq(1,(nanim+va ))

      if (disp == TRUE){
        cat('\n')
        cat("iteration ", i, '\n')
        cat("varA ", varA[i], '\n')
        cat("varE ", varE[i], '\n')
      }
    }
    sol <- 0
    for(k in 1:ndim){
      #print(get((paste("a", k, sep="")))[length(get((paste("a", k, sep=""))))])
      sol <-  c(sol, mean(get((paste("a", k, sep=""))) [(burnin+1):length(get((paste("a", k, sep=""))))]) )
    }
    sol <- sol[-1]
    vcs <- c(mean(varA[(burnin+1):length(varA)]), mean(varE[(burnin+1):length(varE)])  )

    cat('\n')
    return(list( "Solutions"=sol, "Variance Components"=vcs   ))
  }

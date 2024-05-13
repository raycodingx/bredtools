## Given the MME, iteratively solve for the solutions by Jocobi Over Relaxation.

## Arguments
## LHS: left hand side of MME
## RHS: right hand side of MME
## inits: a vecotr of initial values for the solutions
## w: a vector of relaxation factor
## disp: a logical value. If true, show the solutions at each iteration

## Note: When 'w' is all one, usual Jacobi method will be performed.

## Literature1 : Mrode, R.A. 2005. Linear Models for the Prediction of Animal Breeding Values. CAB International, Oxon, UK.
## Literature 2: Misztal, I. 2008. Computational Techniques in Animal Breeding. Course Notes. University of Georgia.

## Author: Gota Morota <morota at wisc dot edu>
## Create: 9-Apr-2010
## Last-Modified: 11-Apr-2010
## License: GPLv3 or later

#' Given the MME, iteratively solve for the solutions by Jocobi Over Relaxation.
#'
#' @param LHS left hand side of MME
#' @param RHS right hand side of MME
#' @param inits a vecotr of initial values for the solutions
#' @param w a vector of relaxation factor
#' @param disp a logical value. If true, show the solutions at each iteration
#'
#' @return a vector of iteratively solve for the solutions
#' @export
#'
jor <-
  function(LHS, RHS, inits, w, disp){

    D <- diag(diag(LHS))
    R <- LHS
    diag(R) <- 0
    w <- diag(w)
    Dinv <- solve(D)%*%w
    x <- matrix(inits)
    I <- diag(1,dim(w)[1])

    diff <- 1
    i = 0
    while (diff > 10E-9){
      i = i + 1
      newx <- (I-w)%*%x + Dinv%*%(RHS - R%*%x)
      if (disp == TRUE){
        cat('\n')
        cat("iteration ", i, '\n')
        print(x)
      }
      diff <-  (  sum((newx - x)^2) ) /sum(newx^2)
      x <- newx
    }

    cat('\n')
    cat("Final solutions after",  i, "th iteration"  )
    return(x)

  }

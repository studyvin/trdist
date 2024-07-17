
#'
#' @name truncatedDistribution
#' @aliases dtrunc
#' @aliases ptrunc
#' @aliases qtrunc
#' @aliases rtrunc
#'
#' @title Truncated Distributions
#'
#' @description Truncated probability density function, truncated cumulative density function, inverse truncated cumulative density function, and random variates from a truncated distribution.
#'
#'
#' @param x Vector of quantiles.
#' @param q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n A positive integer specifying the desired number of random variates.
#' @param distr Character value specifying the desired probability distribution.
#' @param ... Additional arguments passed to the non-truncated distribution functions.
#' @param low Numeric value specifying the lower truncation bound.
#' @param high Numeric value specifying the upper truncation bound. 
#' @param log Logical; if TRUE, log densities are returned.
#' @param lower.tail Logical; if TRUE (default), probabilities are P(X <= x) otherwise, P(X > x).
#' @param log.p Currently ignored.
#'
#'
#' @details The non truncated distribution functions are assumed to be available. For example if the normal distribution is desired then used \code{distr='norm'}, the functions then look for 'qnorm', 'pnorm', etc.
#'
#' The truncation interval is (low, high], which only matters for discrete distribution. 
#' 
#'
#' @return \code{dtrunc} returns a vector of densities.
#'
#' @export dtrunc
#'
#' @examples
#'
#' ## dtrunc
#' # not truncted
#' dnorm(5,mean=5)
#' dtrunc(x=5,distr='norm',mean=5)
#' # truncated
#' dtrunc(x=5,distr='norm',mean=5,low=4, high=5.5)
#'
#'


dtrunc <- function(x, distr, ..., low=-Inf, high=Inf,log=FALSE){
##print('dtrunc:');print(as.list(match.call()))

##############################################
### argument checking
    if(!is.character(distr)|length(distr)!=1){
        stop('argument distribution must be a single character string')
    }

    if(!is.numeric(high)){
        stop('argument high needs to be numeric')
    } #end if

    if(!is.numeric(low)){
        stop('argument low needs to be numeric')
    } #end if

    
    if(!is.logical(log)|length(log)!=1){
        stop('Argument log must be a single logical value.')
    }#


    if(!is.numeric(x)){
        stop('Argument x must be numeric.')
    } #end if


###############################################

    tbound <- c(as.vector(low),as.vector(high))


    ## get truncation bounds
    low <- min(tbound,na.rm=TRUE)
    high <- max(tbound,na.rm=TRUE)


    if (low == high){
        stop("arguments low and high must not be the same value")
    }# end if

    pNonTrunc <- getDistributionFunction(type='p',distr=distr)##get(paste("p", distr, sep = ""), mode = "function")
    dNonTrunc <- getDistributionFunction(type='d',distr=distr)##get(paste("d", distr, sep = ""), mode = "function")

    ## for testing
    ##pLow <- pNonTrunc(low,shape=3,rate=2,lower.tail=FALSE)
    ##pHigh <- pNonTrunc(high,shape=3,rate=2,lower.tail=FALSE)

    pLow <- pNonTrunc(low,...)
    pHigh <- pNonTrunc(high,...)

    (pCheck <- c(pLow,pHigh))
    if(any(!is.finite(pCheck))| any(is.na(pCheck))){
        ## if pNonTrunc return NA, then return NA
        return(rep(NA,length(x)))
    }# end if


    ## calculate truncated density
    out <- dNonTrunc(x,...)/(pHigh-pLow)

    ## make value zero when outside the truncation bounds
    out[x<=low | x>high] <- 0


    if(log){
        out <- log(out)
    }# end if


    return(out)

} #end function

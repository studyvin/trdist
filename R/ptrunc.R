

#'
#' @rdname truncatedDistribution
#'
#' @return \code{ptrunc} returns a vector of probabilities.
#'
#' @export ptrunc
#'
#'
#' @examples
#'
#' ## ptrunc
#' #not truncated
#' pgamma(2, shape=3, rate=2)
#' ptrunc(2, distr = 'gamma', shape=3, rate=2)
#' # truncated
#' ptrunc(2, distr = 'gamma', shape=3, rate=2, low=1, high=5)
#'
#' ## upper tail
#' # not truncated
#' pgamma(2, shape=3, rate=2,lower.tail=FALSE)
#' ptrunc(2, distr='gamma', shape=3, rate=2, lower.tail=FALSE)
#' # truncated
#' ptrunc(2, distr='gamma', shape=3, rate=2, low=1, high=5, lower.tail=FALSE)
#'
#' 

ptrunc <- function(q, distr,...,low=-Inf, high=Inf,lower.tail=TRUE,log.p=FALSE){

    ## for testing
    ##q <- c(2);distribution <- 'gamma';tbound <- c(1,5)
    ##q <- 100;distribution <- 'norm';tbound <- c(0,100);xbar <- -355;stdev <- 42


##############################################################
    ## argument checking

    if(!is.logical(lower.tail)||length(lower.tail)!=1){
        stop('Argument lower.tail must be a single logical value.')
    }#

    if(!is.numeric(q)){
        stop('Argument q must be numeric.')
    } #end if


    if(!is.character(distr)||length(distr)!=1){
        stop('Argument distr must be a single character string.')
    }

    if(!is.numeric(high)){
        stop('Argument high needs to be numeric')
    } #end if

    if(!is.numeric(low)){
        stop('Argument low needs to be numeric')
    } #end if


############################################################


    tbound <- c(as.vector(low),as.vector(high))


    ## get truncation bounds
    low <- min(tbound,na.rm=TRUE)
    high <- max(tbound,na.rm=TRUE)

    if (low == high){
        stop("Arguments low and high must not be the same value")
    }# end if


    
    pNonTrunc <- getDistributionFunction(type='p',distr=distr)##get(paste("p", distr, sep = ""), mode = "function")
    qNonTrunc <- getDistributionFunction(type='q',distr=distr)##get(paste("q", distr, sep = ""), mode = "function")

    ## for testing
    ##(pLow <- pnorm(low,mean=xbar,sd=stdev,lower.tail=FALSE))
    ## (pHigh <- pnorm(high,mean=xbar,sd=stdev,lower.tail=FALSE))
    ##pHigh <- pNonTrunc(high,mean=mean,sd=sd,lower.tail=FALSE)

    (pLow <- pNonTrunc(low,...))
    (pHigh <- pNonTrunc(high,...))

    (pCheck <- c(pLow,pHigh))
    if(any(!is.finite(pCheck))|| any(is.na(pCheck))){
        ## if pNonTrunc return NA, then return NA
        return(rep(NA,length(q)))
    }# end if


    ## if q > high then q = high
    ## if q < low then q= low
    ## for each element of q
    (qAdj <- pmax(pmin(q,high),low))


    ## if density is flat across truncation bounds
    ## return one or zero
    if(pHigh-pLow==0){
        warning('The probability density function within that truncation interval is practically flat. Returning either zeros or ones.')
        out <- rep(pLow,length=length(q))

    }else{
    ##
        out <- (pNonTrunc(qAdj,...) - pLow)/(pHigh-pLow)
    }# end if else


    if(!lower.tail){
        out <- 1-out
    }# end if


    return(out)

} # end function

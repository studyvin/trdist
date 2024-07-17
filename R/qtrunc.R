


#'
#' @rdname truncatedDistribution
#'
#' @return \code{qtrunc} returns a vector of quantiles.
#'
#' @export qtrunc
#'
#'
#' @examples
#'
#' ## qtrunc
#' #not truncated
#' qnorm(p=.975)
#' qtrunc(p=.975,distr='norm')
#' # truncted
#' qtrunc(p=.975,distr='norm', low=0, high=1)
#'
#' ## upper tail
#' # not truncted
#' qnorm(p=.975,lower.tail=FALSE)
#' qtrunc(p=.975, distr='norm', lower.tail=FALSE)
#' # truncated
#' qtrunc(p=.975, distr='norm', low=0, high=1, lower.tail=FALSE)


qtrunc <- function(p, distr, ..., low=-Inf, high=Inf, lower.tail=TRUE,log.p=FALSE){

###################################################
## argument checking
    if(!is.character(distr)|length(distr)!=1){
        stop('Argument distr must be a single character string')
    }#end if

    if(!is.numeric(high)){
        stop('Argument high needs to be numeric')
    } #end if

    if(!is.numeric(low)){
        stop('Argument low needs to be numeric')
    } #end if

    if(!is.logical(lower.tail)|length(lower.tail)!=1){
        stop('Argument lower.tail must be a single logical value.')
    }#end if

    if(!is.numeric(p)){
        stop('Argument p must be numeric.')
    } #end if



###################################################


    tbound <- c(as.vector(low),as.vector(high))


    ## get truncation bounds
    low <- min(tbound,na.rm=TRUE)
    high <- max(tbound,na.rm=TRUE)


    if (low == high){
        stop("Arguments low and high must not be the same value")
    }# end if

    

    ## non truncated functions
    pNonTrunc <- getDistributionFunction(type='p',distr=distr) ##get(paste("p", distr, sep = ""), mode = "function")
    qNonTrunc <- getDistributionFunction(type='q',distr=distr) ##get(paste("q", distr, sep = ""), mode = "function")

    (pLow <- pNonTrunc(low,...))
    (pHigh <- pNonTrunc(high,...))

    (pCheck <- c(pLow,pHigh))
    if(any(!is.finite(pCheck))| any(is.na(pCheck))){
        ## if pNonTrunc return NA, then return NA
        return(rep(NA,length(p)))
    }# end if

    ## reverse p for opposite tail
    if(!lower.tail){
        p <- 1-p
    }# end if


    ## 0<= p <=1, if not make NA
    p[p<0 | p>1] <- NA


    ## adjust p for the truncation
    (adjP <- pLow + p *(pHigh - pLow))

    (adjQ <- qNonTrunc(adjP,...))

    ## ensure quantile is inside the truncation bounds
    out <- pmin(high,pmax(low,adjQ))


    return(out)

} #end function

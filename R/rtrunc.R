

#'
#' @rdname truncatedDistribution
#'
#'
#' @details The random variates are produced using the direct method (see Casella and Berger 2002).
#'
#'
#' @return \code{rtrunc} returns a vector of random variates.
#'
#' @export rtrunc
#'
#' @references
#' G. Casella and R. L. Berger. Statistical inference. Vol. 2. Duxbury Pacific Grove, CA, 2002.
#'
#' @examples
#'
#' ## rtrunc
#' rtrunc(n=5, distr = 'gamma', shape=3, rate=2, low=2, high=5)


rtrunc <- function (n, distr, ..., low=-Inf, high=Inf){

################################################
### argument checking
    if(!is.character(distr)|length(distr)!=1){
        stop('argument distr must be a single character')
    }

    if(!is.numeric(high)){
        stop('argument high needs to be numeric')
    } #end if

    if(!is.numeric(low)){
        stop('argument low needs to be numeric')
    } #end if


    if(!is.numeric(n)){
        stop('Argument n must be numeric.')
    } #end if


################################################

    tbound <- c(as.vector(low),as.vector(high))


    ## get truncation bounds
    low <- min(tbound,na.rm=TRUE)
    high <- max(tbound,na.rm=TRUE)


    if (low == high){
        stop("Arguments low and high must not be the same value")
    }# end if

    randUnif <- stats::runif(n, min = 0, max = 1)
    out <- qtrunc(p=randUnif, distr=distr, ..., low=low, high=high)


    return(out)
} #end function

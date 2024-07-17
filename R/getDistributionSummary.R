
## #'
## #' @name getDistributionSummary
## #'
## #' @title Summary statistics from the fitted distribution
## #'
## #' @description Summary statistics are calculated for the distribution with parameter estimates. Right now only the median is produced.
## #'
## #'
## #'
## #' @param distr  String indicating which distribution to use.
## #' @param paramVec Numeric vector for the parameters associated with distribution. Assumed to be in the same order as the function indicated by \code{distr}.
## #' @param low Numeric, indicating the lower bound of the truncation limits.
## #' @param high Numeric, indicating the high (or upper) bound of the truncation limits.
## #' @param ... Additional arguments to \code{\link[stats]{integrate}}.
## #'
## #'
## #' @details
## #'   The \code{paramVec} argument can only handle one or two parameter distributions. 
## #'
## #'
## #' @return Data frame with the summary statistics.
## #'
## #' @export
## #'
## #' @examples
## #'
## #' getDistributionSummary(distr='norm', paramVec=c(40,25), low=-Inf, high=Inf)
## #'
## #' getDistributionSummary(distr='norm', paramVec=c(40,25), low=0, high=Inf)
## #'
## #' getDistributionSummary(distr='norm', paramVec=c(40,25), low=0, high=30)
## #'
## #' 
## #'

## ## a <- 0
## ## b <- 30
## ## mu <- 40
## ## s <- 25

## ## qnorm((pnorm((a-mu)/s)+pnorm((b-mu)/s))/2)*s + mu


## ## distribution <- 'norm'
## ## paramVec <- c(100,10)
## ## truncBounds <- c(10,190)



## getDistributionSummary <- function(distr, paramVec, low=-Inf, high=Inf, ...){

##     ## Check distribution.
##     if(missing(distr) || length(distr)!=1){
##         stop('Argument distr must be a single character string')
##     }#end if

##     ## for internal use
##     distn <- tolower(distr)


##     ## Check parameters
##     if(missing(paramVec) || !is.numeric(paramVec) || any(is.na(paramVec))){
##         stop('Argument paramVec needs to be numeric')
##     } #end if

##     ## for internal use
##     param1 <- paramVec[1]
##     param2 <- paramVec[2]


##     if(!is.numeric(high)){
##         stop('Argument high needs to be numeric')
##     } #end if

##     if(!is.numeric(low)){
##         stop('Argument low needs to be numeric')
##     } #end if



##     tbound <- c(as.vector(low),as.vector(high))

##     ## get truncation bounds
##     low <- min(tbound,na.rm=TRUE)
##     high <- max(tbound,na.rm=TRUE)


##     if (low == high){
##         stop("Arguments low and high must not be the same value")
##     }# end if

##     tUp <- high
##     tLow <- low

##     ## get the distribution functions
##     pDist <- getDistributionFunction(type = "p", distr = distn)
##     qDist <- getDistributionFunction(type = "q", distr = distn)
##     dDist <- getDistributionFunction(type = "d", distr = distn)


##     ## get the median
##     ## https://en.wikipedia.org/wiki/Truncated_distribution
##     if(is.na(param2)){
##         (cdfLow <- pDist(tLow,param1))
##         (cdfUp <- pDist(tUp,param1))
##         thisMedian <- (qDist((cdfLow+cdfUp)/2,param1))
##     }else{
##         (cdfLow <- pDist(tLow,param1,param2))
##         (cdfUp <- pDist(tUp,param1,param2))
##         thisMedian <- (qDist((cdfLow+cdfUp)/2,param1,param2))

##     }# end if else



##     ## E[X] function
##     expFun <- function(x,distr,param1,param2,tLow,tUp,...){
##         if(is.na(param2)){
##             return(x*dtrunc(x,distr=distr,param1,low=tLow, high=tUp))
##         }#end if

##         return(x*dtrunc(x,distr=distr,param1, param2,low=tLow, high=tUp))
##     }#end expFun


##     ## E[X^2] function
##     exp2Fun <- function(x,distr,param1,param2,tLow,tUp,...){
##         if(is.na(param2)){
##             return(x^2*dtrunc(x,distr=distr,param1,low=tLow, high=tUp))
##         }#end if

##         return(x^2*dtrunc(x,distr=distr,param1, param2,low=tLow, high=tUp))
##     }#end expFun

    

##     thisMean <- stats::integrate(f=expFun,lower=tLow,upper=tUp,dDist=dDist,param1=param1,param2=param2,...)$value/(cdfUp - cdfLow)
##     thisEX2 <- stats::integrate(f=exp2Fun,lower=tLow,upper=tUp,dDist=dDist,param1=param1,param2=param2,...)$value/(cdfUp - cdfLow)
##     thisVar <- thisEX2 - thisMean^2

##     out <- data.frame(distribution=distn,param1=param1,param2=param2,mean=thisMean,median=thisMedian, variance=thisVar)

##     return(out)


## }#end getDistributionSummary function



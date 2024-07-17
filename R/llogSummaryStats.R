#'
#' @rdname LogLogistic
#'
#' @export
#' @return \code{llogSummaryStats} returns a data frame of summary statistics.




llogSummaryStats <- function(shape, scale, ...){

    ################################################
### argument checking


    if(!is.numeric(shape)){
        stop('Argument shape must be numeric.')
    } #end if

    if(!is.numeric(scale)){
        stop('Argument scale must be numeric.')
    } #end if


#################################################
    shape <- as.vector(shape)
    scale <- as.vector(scale)

    if(length(shape) != length(scale)){

        grid <- expand.grid(shape=shape, scale=scale)
        shape <- grid$shape
        scale <- grid$scale
        
    }#end if different lengths



#################################################
    

    a <- shape
    s <- scale

    out <- data.frame(shape=a, scale=s, median=exp(s))

    out$mean <- ifelse(1/a > 1, (exp(s)*pi*a)/(sin(pi*a)), NA)

    out$mode <- ifelse(1/a > 1, exp(s)*((1/a - 1)/(1/a + 1))^a,0)

    out$variance <- ifelse(1/a > 2, (exp(s)^2)*(2*pi*a/sin(2*pi*a) - (a*pi/sin(a*pi))^2),NA)

    return(out)

}#end llogSummaryStats

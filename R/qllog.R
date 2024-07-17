#########################################
## documentation is in dllog
#########################################


#'
#' @rdname LogLogistic
#'
#' @return \code{qllog} returns a vector of quantiles.
#' @export qllog
#'



qllog <- function(p, shape = 1, scale = 1, lower.tail = TRUE, log.p = FALSE,...){

################################################
### argument checking

    if(!is.logical(lower.tail)||length(lower.tail)!=1){
        stop('Argument lower.tail must be a single logical value.')
    }#end if


    if(!is.logical(log.p)||length(log.p)!=1){
        stop('Argument log.p must be a single logical value.')
    }#end if


    if(!is.numeric(p)){
        stop('Argument p must be numeric.')
    } #end if

    if(!is.numeric(shape)){
        stop('Argument shape must be numeric.')
    } #end if

    if(!is.numeric(scale)){
        stop('Argument scale must be numeric.')
    } #end if


#################################################


    out <- exp(stats::qlogis(p, location = scale, scale = shape, log.p=log.p, lower.tail=lower.tail))

    return(out)

} ## end qllog function



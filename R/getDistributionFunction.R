###########


#' @name getDistributionFunction
#'
#' @title Get Distribution Functions
#'
#' @description Determines if the distribution functions are available. This is intended for internal use only.
#'
#' @param type Character, typically either 'r', 'q', 'p', or  'd'.
#' @param distr Character, typically something like 'norm', 'gamma', etc.
#' @param ... Currently ignored.
#'
#' @details It is determined that \code{paste0(type, dist)} is a function and returns that function.  The nature of the returned function is not verified.

#'
#' @return Function, the first function in the search path that matches the name \code{paste0(type, dist)}.
#'
#' @export getDistributionFunction
#'
#' @examples
#'
#' fun <- getDistributionFunction(type="q",distr="norm")
#'

getDistributionFunction <- function(type,distr,...){

    if(!is.character(type)){
        stop('Argument type must be a character')
    }#end if

    if(!is.character(distr)){
        stop('Argument distr must be a character')
    }#end if


    funName <- paste0(type, distr)
    fun <- tryCatch(
    {
        get(funName, mode = "function")
    },
    error=function(cond){
        return(NA)
    },finally={}) #end tryCatch

    if(!is.function(fun)){
        stop('The function ', funName,' is not available.\n',
        'Ensure the function ', funName,' is available before continuing.')
    }# end if

    return(fun)

} # end getDistributionFunction function

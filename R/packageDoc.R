

#'
#' @title Univariate Proability Distributions with Truncation
#'
#' @description Truncation of univariate probability distributions. The probability distribution can come from other packages so long as the function names follow the standard d, p, q, r naming format. Also other univariate probability distributions are included. 
#' 
#' @name trdist
#'
#' @keywords package
#'
"_PACKAGE"


.onAttach <- 
    function(libname, pkgname) {
        pkName <- 'trdist'
        packageStartupMessage("\nThank you for using the ",pkName," package!")
        packageStartupMessage("To acknowledge our work, please cite the package.")
        packageStartupMessage("Use citation('", pkName,
                              "') to obtain the citation information. \n")
        
}

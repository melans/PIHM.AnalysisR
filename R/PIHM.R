#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' 
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' 
#' =============================================
#' @param  Love PIHM? Defaults to TRUE.
#' @keywords PIHM
#' @export
#' @examples
#' PIHM()
 
PIHM <-function(param=TRUE){
    print ("########  #### ##     ## ##     ##");
    print ("##     ##  ##  ##     ## ###   ###"); 
    print ("##     ##  ##  ##     ## #### ####");
    print ("########   ##  ######### ## ### ##");
    print ("##         ##  ##     ## ##     ##");
    print ("##         ##  ##     ## ##     ##"); 
    print ("##        #### ##     ## ##     ##");
    print ("    The Penn State Integrated Hydrologic Model");
    print ("    Current version is PIHM-MF and PIHM v2.4");
     
    if (!require(xts))
    {
        cat('\n\n Trying to install xts package\n');
        install.packages("xts",dep=TRUE,repos='http://cran.us.r-project.org')
        if(!require(xts)) stop("Package not found")
    }
    if (!require(geometry))
    {
        cat('\n\n Trying to install geometry package\n');
        install.packages("geometry",dep=TRUE,repos='http://cran.us.r-project.org')
        if(!require(rgl)) 
            stop("Package not found")
    }

    
}

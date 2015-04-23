#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  inpath Path of output folder.
#' @param  prpjectname.
#' @keywords read input. att file.
#' @return A list. att$att, att$points, att$size etc
#' @export  List of att data.
#' @examples
#' readatt(inpath="./",projectname)


readatt <-function(inpath="./",projectname){
    nargin <- nargs();
    if (nargin <1){
        cat("\nUsage:\n\t readatt <-function(inpath=\"./\",projectname)\n");
        cat("\n\n");
        return(0);
    }
    
    if (substring(inpath,nchar(inpath))=="/"){
    }else{
        inpath <- paste(inpath,"/",sep='');    
    }
    if (nargin <2){ # default: projenctname can be access from projectName.txt;
        projectname=scan(paste(inpatth,"projectName.txt",sep=''));
    }

attfile <- paste(inpath, projectname,".att",sep='');
atthead=scan(attfile,what=character(),nlines=1,blank.lines.skip = TRUE);
matatt <-t( matrix (scan(attfile,what=integer(),skip=1,blank.lines.skip = TRUE), nrow=16))
colnames(matatt)=atthead;

#att <-list("att"=matatt, "headeratt"=atthead);

return(matatt);
}

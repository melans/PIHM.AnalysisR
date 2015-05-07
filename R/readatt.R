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
#' readatt()


readatt <-function(fn){
  if (missing(fn)){
        attfile <- file.path(inpath, paste(projectname,".att",sep=''));
  }else{
        attfile <- fn
  }

if (!file.exists(attfile)){
    stop ("\n\n\nAtt file \'", attfile , "\' is missing\n\n");
}
if (pihmver >2.3){
    atthead=scan(attfile,what=character(),nlines=1,blank.lines.skip = TRUE);
}else{
    atthead=c( "IND",  "SOIL", "GEOL", "LC",   "CMC",  "SNOWH","HSFC", "UNSAT","GW",  "METEO","LAI",  "SS",   "BC0",  "BC1",  "BC2",  "MACP")
}
cat(pihmver)
matatt <-t( matrix (scan(attfile,what=integer(),skip=1,blank.lines.skip = TRUE), nrow=16))
colnames(matatt)=atthead;
return(matatt);
}

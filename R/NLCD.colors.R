#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Fri Sep  2 15:32:54 EDT 2016
#' =============================================
#' @param  x, which is the integer number defined in NLCD 2001,2006, and 2011.  Any value out of (0,100) will be assigned as black.
#' @keywords NLCD, Landuse, colormap
#' @return HEX color strings
#' @examples
#' lc=c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95)
#' nx=length(x)
#' x = numeric(nx)+1
#' names(x)= lc
#' barplot(lc, x, col=NLCD.colors(lc) )
#' @source See the link of NLCD: \url{http://www.mrlc.gov/nlcd11_leg.php} 
NLCD.colors <- function( x){
   # 2001, 2006, 2011 version 
   #reference http://www.mrlc.gov/nlcd11_leg.php
    lccol = matrix('#000000', nrow=100)
    lccol[1]=	"#00fa00"
    lccol[11]=	"#476ba1"
    lccol[12]=	"#d1defa"
    lccol[21]=	"#decaca"
    lccol[22]=	"#d99482"
    lccol[23]=	"#ee0000"
    lccol[24]=	"#ab0000"
    lccol[31]=	"#b3aea3"
    lccol[32]=	"#fafafa"
    lccol[41]=	"#68ab63"
    lccol[42]=	"#1c6330"
    lccol[43]=	"#b5ca8f"
    lccol[51]=	"#a68c30"
    lccol[52]=	"#ccba7d"
    lccol[71]=	"#e3e3c2"
    lccol[72]=	"#caca78"
    lccol[73]=	"#99c247"
    lccol[74]=	"#78ae94"
    lccol[81]=	"#dcd93d"
    lccol[82]=	"#ab7028"
    lccol[90]=	"#bad9eb"
    lccol[91]=	"#b5d4e6"
    lccol[92]=	"#b5d4e6"
    lccol[93]=	"#b5d4e6"
    lccol[94]=	"#b5d4e6"
    lccol[95]=	"#70a3ba"
    x=round(x);
    x[x > 100 | x<1] = 100;
    ret = return( lccol[sort(unique(x) ) ])
}

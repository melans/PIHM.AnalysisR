#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  
#' @param  
#' @keywords 
#' @return 
#' @export  
#' @examples

readtrial <- function(){
    theFile <- file.path(inpath, paste(projectname,".",'trial',sep=''));

    if (!file.exists(theFile)){
        stop ("\n\n\n .trial file \'", theFile , "\' is missing\n\n");
    }

    trials <- list();
    namelist <- character()
    comments <- character();
    
    lines <- readLines(theFile);   
    i=0; 
    for(k in 1:length(lines) ){
        if (grepl("^#",lines[k]) || nchar(lines[k])<=0){
            next ;
        }
        str <- scan(text=lines[k],what=character(),quiet = TRUE);
        
        i = i + 1;
        keyword <- str[1];
        
        n <- length(str);
        range <- numeric();
        nj=1;
        for (j in 2:n){
            if ( !grepl("[[:digit:]]",str[j]) || grepl("^#",str[j]) ) {
               # message(str[j],'\n');
                break;
            }else{
                value <- eval(parse(text=str[j]));
                nv <- length(value);
                range[nj:(nj+nv-1)] <- value ; 
                
                nj = nj+nv;
                #message(str[j],'\t',value,'\n');
            }
        }
        trials[[keyword]]= unique(range) ;
    }
    return(trials);
}

calibSets <- function(trials, calib) {
   # if (missing(calib)){
        calib <- readcalib();
   # }
    onid <- which(calib$offon) ;
    names <- names(calib)
    calib <- list(calib$value[onid],calib$offon[onid],calib$comments[onid]);
    names(calib) <- names
   
    
    trials <- readtrial();
    
    triallen <- do.call(rbind, lapply(trials, function(x) length(x))) 
    mattrial <- expand.grid(trials)
    aname <- names(calib$value)
    bname <- names(trials);
    cname <- union(names(calib$value),names(trials))
    
    nlen <- length(cname);
    offon <- as.logical(1:nlen);
    comments <- character(nlen);
    calibSets <- list();
    for (i in 1:prod(triallen)){
        triline <- as.numeric(mattrial[i,]);
        names(triline) <- bname;
        value <- numeric(nlen);
        names(value) <- cname;
        value[aname] <- calib$value[aname] ;
        value[bname] <- triline[bname] ;
        calibSets[[i]] <- list('value' = value, 
                               'offon' = offon,
                               'comments' = comments)
    }
    return(calibSets) 
}

#unfinished
settrial <- function(name, values){
    trialitems <- c("KSATH",	"KSATV",	"KINF",	 "KMACSATH",  "KMACSATV",  "DINF",	
                "DROOT",	"DMAC",	 "POROSITY",  "ALPHA",	"BETA",	 "MACVF"    ,
                "MACHF",	"VEGFRAC" ,  "ALBEDO" ,   "ROUGH",	"PRCP",	 "SFCTMP"  , 
                "EC",	   "ETT",	  "EDIR",	 "ROUGH_RIV", "KRIVH",	"KRIVV" ,   
                "BEDTHCK" ,  "RIV_DPTH",  "RIV_WDTH",  "DRIP",	 "CMCMAX",    "RS",	  
                "CZIL",	 "FXEXP",	"CFACTR" ,   "RGL",	  "HS",	   "REFSMC"   ,
                "WLTSMC"  );
}


randtrial  <- function(){
    trials <- readtrial();
    calib <- readcalib();
    sets <- calibSets(trials, calib);
    
    nsets <- length(sets);
    ncores <- min(nsets,MAX_THREADS);
    
    tmp <- character(nsets)
    for(i in 1:nsets){
        tmp[i]<- -1;
    }
     if (!require(doParallel))
    {
        cat('\n\n Trying to install doParallel package\n');
        install.packages("doParallel",dep=TRUE,repos='http://cran.us.r-project.org')
        if(!require(doParallel)) 
            stop("Package not found")
    } 

   library(doParallel)
    registerDoParallel(cores=ncores)
   message('\n\n',nsets,' trials are waiting for resources. There are ', ncores,' workers available.\n\n');
    message('Go on ? (yes/no) ');
   line <- readline()
   if ( !grepl('^y', tolower(line)) ){
       stop(' Abort. \n');
   }
   message('\n\nStart parrellel processes\t',Sys.time(),'\n');
    foreach(i=1:nsets)         %dopar%  
    {   
        lag = round(rnorm(1,ncores*2)*((i-1) %% ncores ) );   #lag time before start PIHM.
        message('lag = ',lag,'\n');
        null <- system(paste('sleep', as.character(lag) ) ,wait=TRUE );
        writecalib(sets[[i]]);
        message('Trial #', i, ' starts. \t@',Sys.time(),'\n');
        null <- system(paste('./PIHM-MF ',projectname) ,ignore.stdout = FALSE, intern=TRUE, ignore.stderr = FALSE);
        
        message('\t\tTrial #', i, ' is finished. \t@',Sys.time(),'\n' );
    }

}
testdopar  <- function(){
    nsets=10;
    ncores=5;
      library(doParallel)
    registerDoParallel(cores=ncores)
   message('\n\n',nsets,' trials are waiting for resources. There are ', ncores,' workers available.\n\n');
   null <- system('echo > log.txt');
   message('Go on ? (yes/no) ');
   line <- readline()
   if ( !grepl('^y', tolower(line)) ){
       stop(' Abort. \n');
   }
   
   message('\n\nStart parrellel processes\t',Sys.time(),'\n');
    foreach(i=1:nsets)         %dopar%  
    {   
        lag = round(rnorm(1,ncores*2)*((i-1) %% ncores ) );   #lag time before start PIHM.
        message('lag = ',lag,'\n');
        null <- system(paste('sleep', as.character(lag) ) ,wait=TRUE );
        message('Trial #', i, ' starts. \t@',Sys.time(),'\n');
        null <- system(paste('echo ',i,'  ',Sys.time(),'   ',lag,'>>log.txt') ,ignore.stdout = FALSE, intern=TRUE, ignore.stderr = FALSE);
        
        message('\t\tTrial #', i, ' is finished. \t@',Sys.time(),'\n' );
    }

}

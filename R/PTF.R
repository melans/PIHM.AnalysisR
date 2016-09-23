#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Mon Aug 29 16:42:43 EDT 2016
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
#' 
#' 
#'  <- ============================================

PTF <- function (x, topsoil=TRUE){
    y = matrix(x, ncol = 4)
    nsoil  = nrow(y)
    ly = topsoil * matrix(1, nrow= nsoil)
    for (i in  1:nsoil){
        id= i;
        S = y[i,1] #Silt
        if (S <=0) { #must be positive. minimum value 0.1%.
            warning('Non-positive SILT percentage, type ', i)
            S = 1/10
        }
        C = y[i,2] # Clay
        if (C <=0){ #must be positive. minimum value 0.1%.
            warning('Non-positive CLAY percentage, type ', i)
            C = 1/10    
        }
        OM = y[i,3] #Organic matter
        if(OM <0) { #must be positive or zero
            warning('Non-positive OM percentage, type ', i)
            OM = 0 
        }
        D = y[i,4]  # Bulk Density
        if (D <=0) { # cannot be negative. default = 1.3 g/cm3
            warning('Non-positive bulk density, type ', i)
            D = 1.3
        }
        TS = ly[i];
        #KsatV
        #outData[i][1]= (7.755+0.03252*S+0.93*TS-0.967*D*D-0.000484*C*C-0.000322*S*S+0.001/S-0.0748/OM-0.643*log(S)-0.01398*D*C-0.1673*D*OM+0.02986*TS*C-0.03305*TS*S);
        KsatV=exp(7.755+0.03252*S+0.93*TS-0.967*D*D-0.000484*C*C-0.000322*S*S+0.001/S-0.0748/OM-0.643*log(S)-0.01398*D*C-0.1673*D*OM+0.02986*TS*C-0.03305*TS*S);
        #outData[i][1]=outData[i][1]/100;
        KsatV = KsatV / 100; #cm/day -> m/day
        #ThetaS
        #outData[i][2]=(0.7919+0.001691*C-0.29619*D-0.000001491*S*S+0.0000821*OM*OM+0.02427/C+0.01113/S+0.01472*log(S)-0.0000733*OM*C-0.000619*D*C-0.001183*D*OM-0.0001664*TS*S);
        ThetaS=  (0.7919+0.001691*C-0.29619*D-0.000001491*S*S+0.0000821*OM*OM+0.02427/C+0.01113/S+0.01472*log(S)-0.0000733*OM*C-0.000619*D*C-0.001183*D*OM-0.0001664*TS*S);
        #ThetaR
        #ThetaR=0.01;
        #InfD
        #InfD=0.10;
        #Alpha
        #outData[i][5]=log(-14.96+0.03135*C+0.0351*S+0.646*OM+15.29*D-0.192*TS-4.671*D*D-0.000781*C*C-0.00687*OM*OM+0.0449/OM+0.0663*log(S)+0.1482*log(OM)-0.04546*D*S-0.4852*D*OM+0.00673*TS*C);
        Alpha=  exp(-14.96+0.03135*C+0.0351*S+0.646*OM+15.29*D-0.192*TS-4.671*D*D-0.000781*C*C-0.00687*OM*OM+0.0449/OM+0.0663*log(S)+0.1482*log(OM)-0.04546*D*S-0.4852*D*OM+0.00673*TS*C);
        #Beta
        Beta=1+exp(-25.23-0.02195*C+0.0074*S-0.1940*OM+45.5*D-7.24*D*D+0.0003658*C*C+0.002885*OM*OM-12.81/D-0.1524/S-0.01958/OM-0.2876*log(S)-0.0709*log(OM)-44.6*log(D)-0.02264*D*C+0.0896*D*OM+0.00718*TS*C);
        #hAreaF
        #hAreaF=0.01;
        #macKsatV
        #macKsatV=100*outData[i][1];
        #macKsatV = KsatV * 100
        #val = c(id, KsatV, ThetaS,ThetaR,InfD, Alpha, Beta, hAreaF, macKsatV)
        val = c(id, KsatV, ThetaS, Alpha, Beta)
        if( i==1 ){
            mat =matrix(val, ncol = 5);
        }else{
            mat= rbind(mat, val);
        }
    }
    #colnames(mat) = c('INDEX', 'KsatV(m/d)', 'ThetaS(m3/m3)', 'ThetaR(m3/m3)', 'InfD(m)', 'Alpha(1/m)', 'Beta', 'hAreaF(m2/m2)', 'macKsatV(m/d)')
    colnames(mat) = c('INDEX', 'KsatV(m/d)', 'ThetaS(m3/m3)',  'Alpha(1/m)', 'Beta')
    rownames(mat) = paste(1:nsoil)
    return(mat)
}

PTF.soil <- function(x, topsoil=TRUE){
    y = matrix(x, ncol = 4)
    nsoil  = nrow(y)
    ly = topsoil * matrix(1, nrow= nsoil)
    ptf = PTF(x, topsoil=topsoil)
    
    ret = matrix(0, ncol=9, nrow=nsoil)
    colnames(ret) = c('INDEX', 'KsatV(m/d)', 'ThetaS(m3/m3)', 'ThetaR(m3/m3)', 'InfD(m)', 'Alpha(1/m)', 'Beta', 'hAreaF(m2/m2)', 'macKsatV(m/d)') 
    ret[,1:3]= ptf[,1:3]
    ret[,2]= ptf[,2]    # 
    ret[,4]=0.01        #ThetaR - residual
    ret[,5]= 0.1    #infiltration depth 10cm
    ret[,6:7]= ptf[,4:5] #alpha, beta in van genuchten.
    ret[,8] = 0.01; #macropore fraction, 1%
    ret[,9] = ptf[,2] * 100 # Kmp = kmx * 100;
    
    return(ret)
}



PTF.geol <- function(x, topsoil=FALSE){
    y = matrix(x, ncol = 4)
    nsoil  = nrow(y)
    ly = topsoil * matrix(1, nrow= nsoil)
    ptf = PTF(x, topsoil=topsoil)
    
    ret = matrix(0, ncol=10, nrow=nsoil)
    colnames(ret) = c('INDEX', 'KsatH(m/d)','KsatV(m/d)',
                      'ThetaS(m3/m3)', 'ThetaR(m3/m3)',  'Alpha(1/m)', 
                      'Beta', 'vAreaF(m2/m2)', 'macKsatH(m/d)', 'Dmac(m)') 
    ret[,1] = ptf[,1]    #INDEX
    ret[,2] = ptf[,2] * 10    #Horizontal K Kh = Kv * 10
    ret[,3] = ptf[,2]      #Vertical K    /100 -- cm/day -> m/day
    ret[,4] = ptf[,3]       #porosity
    ret[,5] = 0.01        #ThetaR - residual
    ret[,6:7] = ptf[,4:5] #alpha, beta in van genuchten.
    ret[,8] = 0.01;         #vertical macropore fraction, 1%
    ret[,9] = ptf[,2] * 1e5 # Kmp = kmx * 100,000;
    ret[,10] = 1            # Depth of Macropore;
    
    return(ret)
}


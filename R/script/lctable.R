

tb=list()
att0=readatt(attfile='lc.att_Pre')
tb[[1]]=landcovertable(att=att0)[,2]

att1=readatt(attfile='lc.att$')
tb[[2]]=landcovertable(att=att1)[,2]


att2=readatt(attfile='lc.att_Urban1')
tb[[3]]=landcovertable(att=att2)[,2]

att3=readatt(attfile='lc.att_Urban2')
tb[[4]]=landcovertable(att=att3)[,2]


att4=readatt(attfile='lc.att_Urban3')
tb[[5]]=landcovertable(att=att4)[,2]


tb.names=sort(c(names(tb[[2]]),'43'))
m=length(tb.names)
n=5;
mat=matrix(0,m,n)
rownames(mat)=tb.names;
for( i in 1:n){
    tmp=tb[[i]]; 
    tmp.names=names(tmp)
    for(j in 1:m){
        key=tb.names[j]
        if( key %in% tmp.names){
            mat[key,i]=tmp[key]
        }
    }
}

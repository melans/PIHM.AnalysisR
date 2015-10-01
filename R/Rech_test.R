
kv=1    #m/day;
km=kv*1000  #m/day

alpha=3.5
beta=1.2
af=0.05

sd=20
gw=5;

thetaR=0.05
thetaS=0.5


theta=seq(thetaR+0.001,thetaS,by=0.05)
de=sd-gw;
se=(theta-thetaR)/(thetaS-thetaR);

pm=-(-1+se^(beta/(1-beta)))^(1/beta)
#ke=EffKV(af=af,alpha=alpha,beta=beta,km=km,ks=kv,se=se)


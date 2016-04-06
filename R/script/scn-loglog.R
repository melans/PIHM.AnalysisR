Q0=readRDS('q0.RDS')
Q1=readRDS('q1.RDS')

Q2=readRDS('q2.RDS')
Q2=Q2[-1,]
Q3=readRDS('q3.RDS')
Q4=readRDS('q4.RDS')
Q20=readRDS('q20.RDS')
Q30=readRDS('q30.RDS')
Q40=readRDS('q40.RDS')
att2=readatt(attfile='lc.att_Urban3')

scnlog(att2=att2,Q0=Q0,Q1=Q1,Q2=Q2,file='scncompare_log2.png')
scnlog(att2=att2,Q0=Q0,Q1=Q1,Q2=Q3,file='scncompare_log3.png')
scnlog(att2=att2,Q0=Q0,Q1=Q1,Q2=Q4,file='scncompare_log4.png')
scnlog(att2=att2,Q0=Q0,Q1=Q1,Q2=Q20,file='scncompare_log20.png')
scnlog(att2=att2,Q0=Q0,Q1=Q1,Q2=Q30,file='scncompare_log30.png')
scnlog(att2=att2,Q0=Q0,Q1=Q1,Q2=Q40,file='scncompare_log40.png')




#1:??????
#2???ɽ?��
#3:?ɼ?
#4????ӯ??
#5???о???
#6????ͨ??ֵ
library(lubridate)
library(progress)

a1=read.csv('??????.csv',header = T)
a2=read.csv('?ɽ?��.csv',header = T)
a3=read.csv('?ɼ?.csv',header = T)
a4=read.csv('??ӯ??.csv',header = T)
a5=read.csv('?о???.csv',header = T)
w=read.csv('??ͨ??ֵ.csv',header=T)
t=read.csv('ͣ??.csv',header = T)
ud=read.csv('?ǵ???.csv',header=T)
d=read.csv('????.csv',header = T)
date=as.Date(d$date)
n=length(d$date)   #??????ʱ???ڵ?????986????Ҳ????985??
k=seq(1:(n-1))
pb <- progress_bar$new(total = n-1)
for (i in 1:(n-1)) {
  k[i]=round(0.05*length(a1[which(a1$ipo<year(date[i])),]$code))
  pb$tick()
  Sys.sleep(1 / (n-1))
}             
#????ÿ?????ڵ?????????

b1=b2=b3=b4=b5=b6=list()
pb <- progress_bar$new(total = n-1)
for (i in 1:(n-1)) {
  code_t=t[which(t[,i+1]>=1),]$code          #?????޽??׹?Ʊ?޳?
  a1_ipo=a1[which(a1$ipo<year(date[i]) & a1$code%in%code_t),]    #?޳??¹ɼ?δ???й?Ʊ
  a2_ipo=a2[which(a2$ipo<year(date[i]) & a2$code%in%code_t),] 
  a3_ipo=a3[which(a3$ipo<year(date[i]) & a3$code%in%code_t),] 
  a4_ipo=a4[which(a4$ipo<year(date[i]) & a4$code%in%code_t),]
  a5_ipo=a5[which(a5$ipo<year(date[i]) & a5$code%in%code_t),]
  a6_ipo=w[which(w$ipo<year(date[i]) & w$code%in%code_t),]
  b1[[i]]=a1_ipo[order(a1_ipo[,i+1],decreasing = T),][seq(1,k[i]),1]
  b2[[i]]=a2_ipo[order(a2_ipo[,i+1],decreasing = T),][seq(1,k[i]),1]
  b3[[i]]=a3_ipo[order(a3_ipo[,i+1],decreasing = T),][seq(1,k[i]),1]
  b4[[i]]=a4_ipo[order(a4_ipo[,i+1],decreasing = T),][seq(1,k[i]),1]
  b5[[i]]=a5_ipo[order(a5_ipo[,i+1],decreasing = T),][seq(1,k[i]),1]
  b6[[i]]=a6_ipo[order(a6_ipo[,i+1],decreasing = T),][seq(1,k[i]),1]
  pb$tick()
  Sys.sleep(1 / (n-1))
}             
#ѡȡ??#??????

hchg1=hchg2=hchg3=hchg4=hchg5=hchg6=seq(1:(n-1))
hchg1[1]=hchg2[1]=hchg3[1]=hchg4[1]=hchg5[1]=hchg6[1]=0
pb <- progress_bar$new(total = n-2)
for (i in 1:(n-2)) {
  ud_b1=ud[which(ud$code%in%b1[[i]]),]
  ud_b2=ud[which(ud$code%in%b2[[i]]),]
  ud_b3=ud[which(ud$code%in%b3[[i]]),]
  ud_b4=ud[which(ud$code%in%b4[[i]]),]
  ud_b5=ud[which(ud$code%in%b5[[i]]),]
  ud_b6=ud[which(ud$code%in%b6[[i]]),]
  w_b1=w[which(w$code%in%b1[[i]]),]
  w_b2=w[which(w$code%in%b2[[i]]),]
  w_b3=w[which(w$code%in%b3[[i]]),]
  w_b4=w[which(w$code%in%b4[[i]]),]
  w_b5=w[which(w$code%in%b5[[i]]),]
  w_b6=w[which(w$code%in%b6[[i]]),]
  hchg1[i+1]=weighted.mean(ud_b1[,i+1],w_b1[,i+1])
  hchg2[i+1]=weighted.mean(ud_b2[,i+1],w_b2[,i+1])
  hchg3[i+1]=weighted.mean(ud_b3[,i+1],w_b3[,i+1])
  hchg4[i+1]=weighted.mean(ud_b4[,i+1],w_b4[,i+1])
  hchg5[i+1]=weighted.mean(ud_b5[,i+1],w_b5[,i+1])
  hchg6[i+1]=weighted.mean(ud_b6[,i+1],w_b6[,i+1])
  pb$tick()
  Sys.sleep(1 / (n-2))
}             
#??????#?ǵ???

h1=h2=h3=h4=h5=h6=seq(1:n)
h1[1]=h2[1]=h3[1]=h4[1]=h5[1]=h6[1]=100
for(i in 1:(n-1)){
  h1[i+1]=h1[i]*(1+hchg1[i]/100)
  h2[i+1]=h2[i]*(1+hchg2[i]/100)
  h3[i+1]=h3[i]*(1+hchg3[i]/100)
  h4[i+1]=h4[i]*(1+hchg4[i]/100)
  h5[i+1]=h5[i]*(1+hchg5[i]/100)
  h6[i+1]=h6[i]*(1+hchg6[i]/100)
}
#??????#ָ??

c1=c2=c3=c4=c5=c6=list()
pb <- progress_bar$new(total = n-1)
for (i in 1:(n-1)) {
  code_t=t[which(t[,i+1]>=1),]$code     #?????޽??׹?Ʊ?޳?
  a1_ipo_t=a1[which(a1$ipo<year(date[i]) & a1$code%in%code_t & a1[,i+1]>0),]
  a2_ipo_t=a2[which(a2$ipo<year(date[i]) & a2$code%in%code_t & a2[,i+1]>0),]
  a3_ipo_t=a3[which(a3$ipo<year(date[i]) & a3$code%in%code_t & a3[,i+1]>0),]     #?޳??¹ɡ?δ???й?Ʊ??ͣ?ƹɡ??޽??׹?Ʊ(??????)
  a4_ipo_t=a4[which(a4$ipo<year(date[i]) & a4$code%in%code_t & a4[,i+1]>0),]
  a5_ipo_t=a5[which(a5$ipo<year(date[i]) & a5$code%in%code_t & a5[,i+1]>0),]
  a6_ipo_t=w[which(w$ipo<year(date[i]) & w$code%in%code_t & w[,i+1]>0),]
  c1[[i]]=a1_ipo_t[order(a1_ipo_t[,i+1]),][seq(1,k[i]),1]
  c2[[i]]=a2_ipo_t[order(a2_ipo_t[,i+1]),][seq(1,k[i]),1]
  c3[[i]]=a3_ipo_t[order(a3_ipo_t[,i+1]),][seq(1,k[i]),1]
  c4[[i]]=a4_ipo_t[order(a4_ipo_t[,i+1]),][seq(1,k[i]),1]
  c5[[i]]=a5_ipo_t[order(a5_ipo_t[,i+1]),][seq(1,k[i]),1]
  c6[[i]]=a6_ipo_t[order(a6_ipo_t[,i+1]),][seq(1,k[i]),1]
  pb$tick()
  Sys.sleep(1 / (n-1))
}             
#ѡȡ??#??????

pb <- progress_bar$new(total = n-1)
lchg1=lchg2=lchg3=lchg4=lchg5=lchg6=seq(1:(n-1))
lchg1[1]=lchg2[1]=lchg3[1]=lchg4[1]=lchg5[1]=lchg6[1]=0
for (i in 1:(n-2)) {
  ud_c1=ud[which(ud$code%in%c1[[i]]),]
  ud_c2=ud[which(ud$code%in%c2[[i]]),]
  ud_c3=ud[which(ud$code%in%c3[[i]]),]
  ud_c4=ud[which(ud$code%in%c4[[i]]),]
  ud_c5=ud[which(ud$code%in%c5[[i]]),]
  ud_c6=ud[which(ud$code%in%c6[[i]]),]
  w_c1=w[which(w$code%in%c1[[i]]),]
  w_c2=w[which(w$code%in%c2[[i]]),]
  w_c3=w[which(w$code%in%c3[[i]]),]
  w_c4=w[which(w$code%in%c4[[i]]),]
  w_c5=w[which(w$code%in%c5[[i]]),]
  w_c6=w[which(w$code%in%c6[[i]]),]
  lchg1[i+1]=weighted.mean(ud_c1[,i+1],w_c1[,i+1])
  lchg2[i+1]=weighted.mean(ud_c2[,i+1],w_c2[,i+1])
  lchg3[i+1]=weighted.mean(ud_c3[,i+1],w_c3[,i+1])
  lchg4[i+1]=weighted.mean(ud_c4[,i+1],w_c4[,i+1])
  lchg5[i+1]=weighted.mean(ud_c5[,i+1],w_c5[,i+1])
  lchg6[i+1]=weighted.mean(ud_c6[,i+1],w_c6[,i+1])
  pb$tick()
  Sys.sleep(1 / (n-2))
}             
#??????#?ǵ???

l1=l2=l3=l4=l5=l6=seq(1:n)
l1[1]=l2[1]=l3[1]=l4[1]=l5[1]=l6[1]=100
for(i in 1:(n-1)){
  l1[i+1]=l1[i]*(1+lchg1[i]/100)
  l2[i+1]=l2[i]*(1+lchg2[i]/100)
  l3[i+1]=l3[i]*(1+lchg3[i]/100)
  l4[i+1]=l4[i]*(1+lchg4[i]/100)
  l5[i+1]=l5[i]*(1+lchg5[i]/100)
  l6[i+1]=l6[i]*(1+lchg6[i]/100)
}
#??????#ָ??


#r1=(h[n]-100)/100
#r2=(l[n]-100)/100
#r11=(h[n]-h[959])/h[959]
#r22=(l[n]-l[959])/l[959]
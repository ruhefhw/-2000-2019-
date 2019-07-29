#7：流动比率
#8：资产负债率
#9：股息率
#10：ROE
library(lubridate)
library(progress)

a7=read.csv("流动比率.csv",header = T)
a8=read.csv("资产负债率.csv",header = T)
a9=read.csv("股息率.csv",header = T)
a10=read.csv("ROE.csv",header = T)
w=read.csv('流通市值.csv',header=T)
udd=read.csv('涨跌幅（年指标）.csv',header=T)
tt=read.csv('交易天数年.csv',header=T)
nn=length(a7[1,])-2
kk=seq(1:nn)
for (i in 1:nn) {
  kk[i]=round(0.05*length(a7[which(a7$ipo<(1998+i)),]$code))
}

b7=b8=b9=b10=list()
pb <- progress_bar$new(total = nn)
for (i in 1:nn) {
  code_tt=tt[which(tt[,i+1]>=230),]$code    #剔除交易时间小于230天的个股
  a7_ipo=a7[which(a7$code%in%code_tt & a7$ipo<(1998+i)),]   #剔除当年发行的新股
  a8_ipo=a8[which(a8$code%in%code_tt & a8$ipo<(1998+i)),]
  a9_ipo=a9[which(a9$code%in%code_tt & a9$ipo<(1998+i)),]
  a10_ipo=a10[which(a10$code%in%code_tt & a10$ipo<(1998+i)),]
  b7[[i]]=a7_ipo[order(a7_ipo[,i+1],decreasing = T),][seq(1,kk[i]),1]
  b8[[i]]=a8_ipo[order(a8_ipo[,i+1],decreasing = T),][seq(1,kk[i]),1]
  b9[[i]]=a9_ipo[order(a9_ipo[,i+1],decreasing = T),][seq(1,kk[i]),1]
  b10[[i]]=a10_ipo[order(a10_ipo[,i+1],decreasing = T),][seq(1,kk[i]),1]
  pb$tick()
  Sys.sleep(1 / nn)
}
#选取高#样本股

hchg7=hchg8=hchg9=hchg10=seq(1:(n-1))
pb <- progress_bar$new(total = n-1)
for (i in 1:(n-2)) {
  j=year(date[i+1])
  ud_b7=udd[which(udd$code%in%b7[[j-1999]]),]
  ud_b8=udd[which(udd$code%in%b8[[j-1999]]),]
  ud_b9=udd[which(udd$code%in%b9[[j-1999]]),]
  ud_b10=udd[which(udd$code%in%b10[[j-1999]]),]
  w_b7=w[which(w$code%in%b7[[j-1999]]),]
  w_b8=w[which(w$code%in%b8[[j-1999]]),]
  w_b9=w[which(w$code%in%b9[[j-1999]]),]
  w_b10=w[which(w$code%in%b10[[j-1999]]),]
  hchg7[i+1]=weighted.mean(ud_b7[,i+1],w_b7[,i+1])
  hchg8[i+1]=weighted.mean(ud_b8[,i+1],w_b8[,i+1])
  hchg9[i+1]=weighted.mean(ud_b9[,i+1],w_b9[,i+1])
  hchg10[i+1]=weighted.mean(ud_b10[,i+1],w_b10[,i+1])
  pb$tick()
  Sys.sleep(1 / (n-1))
}
#计算高#涨跌幅

h7=h8=h9=h10=seq(1:n)
h7[1]=h8[1]=h9[1]=h10[1]=100
for (i in 1:(n-1)) {
  h7[i+1]=h7[i]*(1+hchg7[i]/100)
  h8[i+1]=h8[i]*(1+hchg8[i]/100)
  h9[i+1]=h9[i]*(1+hchg9[i]/100)
  h10[i+1]=h10[i]*(1+hchg10[i]/100)
}
#计算高#指数


c7=c8=c9=c10=list()
pb <- progress_bar$new(total = nn)
for (i in 1:nn) {
  code_tt=tt[which(tt[,i+1]>=230),]$code    #剔除交易时间小于230天的个股
  a7_ipo_tt=a7[which(a7$code%in%code_tt & a7$ipo<(1998+i) & a7[,i+1]>0),] #剔除新股，停牌股，指标小于0的个股
  a8_ipo_tt=a8[which(a8$code%in%code_tt & a8$ipo<(1998+i) & a8[,i+1]>0),]
  a9_ipo_tt=a9[which(a9$code%in%code_tt & a9$ipo<(1998+i)),]
  a10_ipo_tt=a10[which(a10$code%in%code_tt & a10$ipo<(1998+i) & a10[,i+1]>0),]
  c7[[i]]=a7_ipo_tt[order(a7_ipo_tt[,i+1]),][seq(1,kk[i]),1]
  c8[[i]]=a8_ipo_tt[order(a8_ipo_tt[,i+1]),][seq(1,kk[i]),1]
  c9[[i]]=a9_ipo_tt[which(a9_ipo_tt[,i+1]==0),][,1]     #选取0股息个股
  c10[[i]]=a10_ipo_tt[order(a10_ipo_tt[,i+1]),][seq(1,kk[i]),1]
  pb$tick()
  Sys.sleep(1 / nn)
}
#选取低#样本股

lchg7=lchg8=lchg9=lchg10=seq(1:(n-1))
pb <- progress_bar$new(total = n-1)
for (i in 1:(n-1)) {
  j=year(date[i+1])
  udd_c7=udd[which(udd$code%in%c7[[j-1999]]),]
  udd_c8=udd[which(udd$code%in%c8[[j-1999]]),]
  udd_c9=udd[which(udd$code%in%c9[[j-1999]]),]
  udd_c10=udd[which(udd$code%in%c10[[j-1999]]),]
  w_c7=w[which(w$code%in%c7[[j-1999]]),]
  w_c8=w[which(w$code%in%c8[[j-1999]]),]
  w_c9=w[which(w$code%in%c9[[j-1999]]),]
  w_c10=w[which(w$code%in%c10[[j-1999]]),]
  lchg7[i]=weighted.mean(udd_c7[,i+1],w_c7[,i+1])
  lchg8[i]=weighted.mean(udd_c8[,i+1],w_c8[,i+1])
  lchg9[i]=weighted.mean(udd_c9[,i+1],w_c9[,i+1])
  lchg10[i]=weighted.mean(udd_c10[,i+1],w_c10[,i+1])
  pb$tick()
  Sys.sleep(1 / (n-1))
}
#计算低#涨跌幅

l7=l8=l9=l10=seq(1:n)
l7[1]=l8[1]=l9[1]=l10[1]=100
for (i in 1:(n-1)) {
  l7[i+1]=l7[i]*(1+lchg7[i]/100)
  l8[i+1]=l8[i]*(1+lchg8[i]/100)
  l9[i+1]=l9[i]*(1+lchg9[i]/100)
  l10[i+1]=l10[i]*(1+lchg10[i]/100)
}
#计算低#指数
library(base)
RW2D<-function(N)
{
  i<-0
  xdir<-0
  ydir<-0
  xpos<-vector()
  xpos[1]<-xdir
  ypos<-vector()
  ypos[1]<-ydir
  for (i in 1:N-1)
  {
    r<-runif(1)
    if(r<=0.25) {xdir<-xdir+1}
    if(r>0.25 && r<=0.5) {xdir<-xdir-1}
    if(r>0.5 && r<=0.75) {ydir<-ydir +1}
    if(r>0.75) {ydir<-ydir-1}
    xpos[i+1]<-xdir
    ypos[i+1]<-ydir
  }
  return(cbind(xpos,ypos))
}
###sprawdzenie czy wylosoway polinorm jest sawem
check<-function(r)
{
  a<-0
  rwt<-r
  i<-dim(rwt)[2]
  for(j in 1:i-1)
  {
    
    for(k in (j+1):i)
    {
      if(identical(rwt[,j],rwt[,k]))
      {
        a<-1;
        a
      }
    }
  }
  
  a
}
check1<-function(r)
{
  a<-0
  rwt<-r
  rw<-as.data.frame(t(rwt))
  i<-dim(rwt)[2]
  for(p in 1:i)
  {
    if(rw[p,1]==0 & rw[p,2]==0)
    {
      a<-1
      a
    }
    a
  }
  a
  for(j in 1:i-1)
  {
    
    for(k in (j+1):i)
    {
      if(identical(rwt[,j],rwt[,k]))
      {
        a<-1;
        a
      }
    }
  }
  
  a
}
##sprawdzanie czy dany mniejszy polinorm jest sawem 
fun<-function(r,l){
  rwt<-r
  i<-l
  a<-0
  for(j in 1:i-1)
  {
    for(k in (j+1):i)
    {
      if(identical(rwt[,j],rwt[,k]))
      {
        a<-1;
        a
      }
    }
  }
  a
}
#liczenie wagi dlanego polinormu
fun1<-function(r){
  rw<-r
for(i in 2:ile)
{
  if(dim(rw)[1]==1)
  {
    wyn<-wyn
    break;
  }
  jjjj<-numeric(1)
  xpos<-rw[i-1,1] 
  ypos<-rw[i-1,2]
  jjj<-numeric(4)
  palt1<-numeric(1)
  palt2<-numeric(1)
  palt3<-numeric(1)
  palt4<-numeric(1)
 for(k in 1:4)
 {
  if(k==1)
  {
    xpos<-xpos+1
  }
   if(k==2)
   {
     xpos<-xpos-1
   }
   if(k==3)
   {
     ypos<-ypos+1
   }
   if(k==4)
   {
     ypos<-ypos-1
   }
   s<-cbind(xpos,ypos)
    if(k==1)
   {
     assign(paste0("pal",k),rbind(rw[1:i-1,],s))
    palt1<-t(pal1)
    jjj[1]<-max(check(palt1),ifelse(xpos==0 & ypos==0,1,0))
   }
   if(k==2)
   {
     assign(paste0("pal",k),rbind(rw[1:i-1,],s))
     palt2<-t(pal2)
     jjj[2]<-max(check(palt2),ifelse(xpos==0 & ypos==0,1,0))
   }
   if(k==3)
   {
     assign(paste0("pal",k),rbind(rw[1:i-1,],s))
     palt3<-t(pal3)
     jjj[3]<-max(check(palt3),ifelse(xpos==0 & ypos==0,1,0))
   }
   if(k==4)
   {
     assign(paste0("pal",k),rbind(rw[1:i-1,],s))
     palt4<-t(pal4)
     jjj[4]<- max(check(palt4),ifelse(xpos==0 & ypos==0,1,0))
   }
   xpos<-rw[i-1,1] 
   ypos<-rw[i-1,2]
 }
  jjjj<-jjj[1]+jjj[2]+jjj[3]+jjj[4]
  licz[i]<-4-jjjj
  wyn<-wyn*licz[i]
}
  wyn
  }
###symulacje prostą metodą MC
results<-numeric(10)

ile<-1
w<-10000
res<-numeric(w)
for(y in 1:w)
{
rw<-as.data.frame(RW2D(ile))
rwt<-as.data.frame(t(rw))
res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[1]<-(sum(res)/w)*4^{ile}
results[1]

ile<-2
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[2]<-(sum(res)/w)*4^{ile}
results[2]

ile<-3
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[3]<-(sum(res)/w)*4^{ile}
results[3]

ile<-4
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[4]<-(sum(res)/w)*4^{ile}
results[4]

ile<-5
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[5]<-(sum(res)/w)*4^{ile}
results[5]

ile<-10
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[6]<-(sum(res)/w)*4^{ile}
results[6]

ile<-20
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[7]<-(sum(res)/w)*4^{ile}
results[7]

ile<-30
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[8]<-(sum(res)/w)*4^{ile}
results[8]

ile<-40
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[9]<-(sum(res)/w)*4^{ile}
results[9]

ile<-50
w<-10000
res<-numeric(w)
for(y in 1:w)
{
  rw<-as.data.frame(RW2D(ile))
  rwt<-as.data.frame(t(rw))
  res[y]<-ifelse(check1(rwt)==1,0,1)
}
results[10]<-(sum(res)/w)*4^{ile}
results[10]
###symulacje metodą Rosenbluthów
wynik<-numeric(10)
ile<-1
w<-10000
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[1]<-sum(mno2)/w
  wynik[1]
  
  ile<-2
  w<-10000
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[2]<-sum(mno2)/w
  wynik[2]
  
  ile<-3
  w<-10000
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[3]<-sum(mno2)/w
  wynik[3]
  
  ile<-4
  w<-10000
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[4]<-sum(mno2)/w
  wynik[4]
  
  ile<-5
  w<-10000
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[5]<-sum(mno2)/w
  wynik[5]
  
  ile<-10
  w<-10000
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[6]<-sum(mno2)/w
  wynik[6]
  
  ile<-20
  w<-10
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[7]<-sum(mno2)/w
  wynik[7]
  
  ile<-30
  w<-2
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[8]<-sum(mno2)/w
  wynik[8]
  
  ile<-40
  w<-1
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[9]<-sum(mno2)/w
  wynik[9]
  
  ile<-50
  w<-1
  licz<-numeric(ile)
  licz[1]<-4
  wyn<-licz[1]
  n=1
  mno2<-numeric(w)
  while (n<w+1){
    rw<-as.data.frame(RW2D(ile))
    rwt<-as.data.frame(t(rw))
    mno2[n]<-fun1(rw)
    n<-ifelse(check(rwt)==1,n,n+1)
  }
  wynik[10]<-sum(mno2)/w
  wynik[10]


YiTing=function(time,censor)  #KMtable、存活函數、風險函數、KMcurve 
{ 	###Survival funtion
  	d1=data.frame(time,censor)
   	d2=d1[order(d1$censor,decreasing=T),]
  	d3=d2[order(d2$time),]
 	time=d3$time
 	censor=d3$censor
 	n=length(time)
 	death=numeric(n)
 	number=numeric(n)
 	for(i in 1:n)
  	{
 		c=censor[i]
 		if (c==1)
 		{
 			death[i]=length(which(time==time[i]))
 		}else
 		{
 			death[i]=0
 		}
 		number[i]=n-i+1
 		 		if(length(which(time==time[i])) > 1)
 		{
 			death[i]=death[i]-length(which((time==time[i]) & censor==0))
 		}
 	}
 	test=data.frame(time,censor,death,number)
 	test2=test[!duplicated(test$time) & censor==1,]
 	tj=test2$time
 	nj=test2$number
 	dj=test2$death
  	n2=length(tj)
 	st=numeric(n2)
 	for(j in 1:n2)
 	{
 		if(j==1)
 		{
 			st[j]=1*((nj[j]-dj[j])/nj[j])
 		}else
 		{
 			st[j]=st[j-1]*((nj[j]-dj[j])/nj[j])
 		}
 	}
 	KMtable=data.frame(tj,nj,dj,st)

  	###Hazard funtion
   	ht=numeric(n2)
 	for(k in 1:n2)
 	{
 		ht[k]=dj[k]/nj[k]

	 }  	

	###KMcurve
  	KMX=numeric(n2+2)
 	KMX[1]=0
 	KMX[2]=tj[1]
 	KMX[n2+2]=time[n]
 	KMY=numeric(n2+2)
 	KMY[1]=1 
	KMY[2]=st[1]
 	KMY[n2+2]=st[n2]
 	for(KMi in 2:n2+1)
 	{
 		KMX[KMi]=tj[KMi-1]
 		KMY[KMi]=st[KMi-1]
 	}
 	photo=plot(KMX,KMY,xlim=c(1,time[n]),ylim=c(0,1),xlab="survival time",ylab="survival probability",type="s")

  	### 	
	out=list(KMtable,st,ht,photo)
 	return(out)
} 
########################################### 
#test 
x1=c(2,3,6,6,7,10,15,15,16,27,30,32)
y1=c(1,0,1,1,1,0,1,1,1,1,1,1)
x2=c(6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,32,34,35,6)
y2=c(0,1,1,1,0,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,1)
YiTing(x1,y1)
YiTing(x2,y2) 
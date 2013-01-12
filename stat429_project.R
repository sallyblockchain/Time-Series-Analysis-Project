library(rdatamarket)
library(astsa)
paper <- dmseries("http://data.is/Ur24KI")
#-------Data Section begins-------- 
plot(paper, type="o", ylab="Sales",main="Industry Sales for Printing and Writing Paper")
#plot(paper,type="o", ylim=c(0,1200), xlab=" ",ylab=" ")
#par(new=TRUE)
#plot(SMA(paper), col=2,ylim=c(0,1200), xlab="Index", 
#     ylab="Car Sales", main="Moving Average Plot for Car Sales (in red)")
plot(diff(paper,12), type="o")
plot(diff(diff(paper,12)), type="o", ylab="Paper Sales",main="Monthly Industry Sales for Printing and Writing Paper")
#-------Model Section begins-------- 
n = length(paper)
n # 120
train.size = round(n*0.95)
train.size # 114
train.data = paper[1:114]
test.data = paper[115:120]
paper.transformed = diff(diff(train.data, 12)) # on train.data
# acf(paper.transformed, 48)
# pacf(paper.transformed, 48)
acf2(paper.transformed, 48)
#---
P=4
Q=4
crit<-matrix(0,P+1,Q+1)
for (j in 0:P)
{
  for (k in 0:Q)
  {
    dataML<-arima(train.data,order=c(j,1,k),seasonal=list(order=c(0,1,1),period=12),method="ML")
    #AICC
    crit[j+1,k+1]<-n*log(dataML$sigma)+2*(j+k+1)*n/(n-j-k-2)
    #BIC
    #crit[j+1,k+1]<-n*log(dataML$sigma)+(j+k+1)*log(n)
    #AIC
    #crit[j+1,k+1]<-n*log(dataML$sigma)+2*(j+k+1)
  }
}
#locate the minimum information criteria
crit
min(crit)
# BIC&AICc 
sarima(train.data, 0,1,1,0,1,1,12)
bic.pr=sarima.for(train.data,6,0,1,1,0,1,1,12)
bic.pr
lines(paper[114:120],type="o")
# AIC 
sarima(train.data, 2,1,3,0,1,1,12)
aic.pr=sarima.for(train.data,6,2,1,3,0,1,1,12)
aic.pr
lines(paper[114:120],type="o")
# forcast errors
#pred=bic.pr$pred
pred = aic.pr$pred
fe=rep(0,6)
for(i in 1:6)
{
  fe[i]<-(pred[i]-test.data[i])^2
}
fe
sum(fe)
#-----spectral density analysis begins--------
data=diff(diff(paper,12))
T=108
freq<-(1:(T/2))/T
per<-(abs(fft(data)))^2/T
per<-per[2:(T/2+1)]
truespec<-spec.pgram(data, taper=0, log="no")
Umax<-max(max(truespec$spec),max(per))
plot(freq,per,type="l",ylab="spectral density",ylim=c(0,Umax))
lines(freq,truespec$spec,ylim=c(0,Umax),col=2,lwd=3)
#par(mfrow=c(2,2))
k1 = kernel("modified.daniell", c(1,1))
truespec1<-spec.pgram(data, kernel =k1,taper=0, log="no")
per.ave1 = spec.pgram(data, kernel =k1, taper=0, log ="no",plot=F)$spec
plot(freq,per,type="o",ylab="spectral density",ylim=c(0,Umax))
lines(freq,truespec1$spec,ylim=c(0,Umax),col=2,lwd=3)

k2 = kernel("modified.daniell", c(2,2))
truespec2<-spec.pgram(data, kernel =k2,taper=0, log="no")
per.ave2 = spec.pgram(data, kernel =k2, taper=0, log ="no",plot=F)$spec
plot(freq,per.ave2,type="l",ylab="spectral density",ylim=c(0,Umax))
lines(freq,truespec2$spec,ylim=c(0,Umax),col=2,lwd=3)

k2 = kernel("modified.daniell", c(2,2))
truespec2<-spec.pgram(data, kernel =k2,taper=0, log="no")
k3 = kernel("modified.daniell", c(3,3))
truespec3<-spec.pgram(data, kernel =k3,taper=0, log="no")
k4 = kernel("modified.daniell", c(4,4))
truespec4<-spec.pgram(data, kernel =k4,taper=0, log="no")
Umax<-max(max(truespec$spec),max(per))

#abline(v=1/12, lty="dotted")
k2 = kernel("modified.daniell", c(2,2))
per.ave2 = spec.pgram(data, kernel =k2, taper=0, log ="no",plot=F)$spec
plot(freq,truespec$spec,type="l",col=1,lwd=3)
lines(freq,per.ave2,col=2,lwd=3,type="l")



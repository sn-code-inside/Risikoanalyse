#-------------------------------------------------#
#             Beispiel 5.1.1.1                    #
#-------------------------------------------------#
#	    Parameter der Normalverteilung
mu <- c(0,0)
var.1<-1
var.2<-1
#     Grafikparameter
par(mar=c(4,4,2,2)+0.1)
par(mfrow=c(2,4))
#     Simulationen
n<-1000
#	    Erzeuge Plots
rho<-1
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(a)")

rho<-0.9
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(b)")

rho<-0.5
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(c)")

rho<-0
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(d)")

rho<--0.5
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(e)")

rho<--0.9
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(f)")

rho<--1
cov<-rho*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y",main="(g)")

#     rho=0, jedoch Abhängigkeiten
set.seed(4711)
n<-1000
x<-runif(n,-1,1)
y<-x^2+rnorm(n,0,0.05)
plot(x,y,main="(h)")
data<-cbind(x,y)  
#-------------------------------------------------#
#             Beispiel 5.1.1.2                    #
#-------------------------------------------------#
BMW<-c(0.0030,0.0224,-0.0059,0.0206,-0.0058,-0.0118,0.0064,0.0039,-0.0015,-0.0238)
Siemens<-c(-0.0051,0.0072,-0.0055,0.0017,0.0160,-0.0013,0.0219,0.0016,-0.0156,-0.0222)
data<-as.data.frame(cbind(BMW,Siemens))
plot(data)
cbind(mean(BMW),mean(Siemens))
var.BMW<-(length(BMW)-1)/length(BMW)*var(BMW)
var.Siemens<-(length(Siemens)-1)/length(Siemens)*var(Siemens)
cbind(var.BMW,var.Siemens)
cor(data)

#-------------------------------------------------#
#             Beispiel in 5.1.2                   #
#-------------------------------------------------#
#     Verwende Daten aus Beispiel 5.1.1.2
#     Berechne Regressionskoeffizienten 
#     mit Formeln von S. 258
b.dach<-sd(Siemens)/sd(BMW)*cor(BMW,Siemens)
a.dach<-mean(Siemens)-b.dach*mean(BMW)
c(a.dach,b.dach)
#     Erzeugt Abb. 5.5
plot(data,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
abline(a.dach,b.dach,lwd=2,lty=1)

#     Alternative: Mit lm-Befehl
mod.1<-lm(Siemens~BMW,data=as.data.frame(data))
mod.1
summary(mod.1)
par(mar=c(4,4,2,2)+0.1)
plot(data,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
abline(mod.1,lwd=2)

#-------------------------------------------------#
#             Beispiel in 5.3.3                   #
#-------------------------------------------------#
library(copula)
#     Definiere Copulas
gauss.cop<-mvdc(normalCopula(0.66), c("norm", "norm"),
          list(list(mean=0,sd=0.0157), list(mean=0,sd=0.0121)))
t.cop<-mvdc(tCopula(0.67,df=5.5), c("norm", "norm"),
          list(list(mean=0,sd=0.0157), list(mean=0,sd=0.0121)))
gumbel.cop<-mvdc(gumbelCopula(1.77), c("norm", "norm"),
          list(list(mean=0,sd=0.0157), list(mean=0,sd=0.0121)))
clayton.cop<-mvdc(claytonCopula(1.27), c("norm", "norm"),
          list(list(mean=0,sd=0.0157), list(mean=0,sd=0.0121)))
          
          
          
par(mfrow=c(4,2))
par(mar=c(2,2,3,1)+0.1)
x.lim<-c(-0.05,0.05)
y.lim<-x.lim
theta<-20
phi<-20          
#     Erzeuge Abb. 5.9
par(mfrow=c(2,4))
par(mar=c(2,2,3,1)+0.1)
x.lim<-c(-0.05,0.05)
y.lim<-c(-0.05,0.05)
persp(gauss.cop,dmvdc,theta , phi ,shade=.5, expand = .5,xlim=x.lim, ylim=y.lim,xlab="BMW",ylab="Siemens",zlab="")
persp(t.cop,dmvdc,theta , phi,shade=.5, expand = .5,xlim=x.lim, ylim=y.lim,xlab="BMW",ylab="Siemens",zlab="")
persp(gumbel.cop,dmvdc,theta , phi ,shade=.5, expand = .5,xlim=x.lim, ylim=y.lim,xlab="BMW",ylab="Siemens",zlab="")
persp(clayton.cop,dmvdc,theta , phi ,shade=.5, expand = .5,xlim=x.lim, ylim=y.lim,xlab="BMW",ylab="Siemens",zlab="")

contour(gauss.cop,dmvdc,xlim=x.lim, ylim=y.lim)
contour(t.cop,dmvdc,xlim=x.lim, ylim=y.lim)
contour(gumbel.cop,dmvdc,xlim=x.lim, ylim=y.lim)
contour(clayton.cop,dmvdc,xlim=x.lim, ylim=y.lim)
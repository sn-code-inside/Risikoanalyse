#-------------------------------------------------#
#             Aufgabe 6.1                         #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
#     Daten laden
#     Pfad des Verzeichnisses, z.B.
pfad.name<-"C:\\Dokumente und Einstellungen\\...\\Risikoanalyse\\Datensätze\\"
#      Hänge Dateinamen an Pfadnamen an, z.B.
file.name.in<-paste(pfad.name,sep="","Aufgabe 6.1.xls")
#      Einlesen als Excel-Datei
data<-read.table(file.name.in,header=TRUE,sep="\t",dec=",")
#     Alternativ: Daten erzeugen 
set.seed(4711)
data<-rlnorm(100)

#-------------Analyse-----------------------------#
summary(data)
hist(data) 
par(mfcol=c(2,2))
hist(data,breaks=5)
hist(data,breaks=10)
hist(data,breaks=20)
hist(data,breaks=50)

#-------------------------------------------------#
#             Aufgabe 6.4                         #
#-------------------------------------------------#
#   Erzeugung der Daten
set.seed(4711)
schadenzahlen<-rpois(10,5)
schadenzahlen
mu<-mean(schadenzahlen)
sigma2<-var(schadenzahlen)


#-------------Teil (a)----------------------------#
mu
sigma2


#-------------Teil (b)----------------------------#
#     Plot der Häufigkeiten
x<-0:15
z<-cut(schadenzahlen,breaks=c(-Inf,x))
y<-table(z)/length(schadenzahlen)
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))
#     Anpassung Poisson-Verteilung
#     Plot angepasste Poisson-Verteilung
y<-dpois(x,mu)
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))

#     Schätzer Binomverteilung
p.dach.binom<-function(mu,sigma2){1-sigma2/mu}
n.dach.binom<-function(mu,sigma2){mu/(1-sigma2/mu)}

#     Anpassung Binverteilung
p.dach.bin<-p.dach.binom(mu,sigma2)
n.dach.bin<-n.dach.binom(mu,sigma2)
p.dach.bin
n.dach.bin

#     Nicht ausgeführt
#     Plot angepasste Bin-Verteilung
#   y<-dbinom(x,n.dach.bin,p.dach.bin)
#   plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))

#     Schätzer NegBinomverteilung
p.dach.neg.binom<-function(mu,sigma2){mu/sigma2}
r.dach.neg.binom<-function(mu,sigma2){mu^2/(sigma2-mu)}

#     Anpassung Binverteilung
p.dach.negb<-p.dach.neg.binom(mu,sigma2)
r.dach.negb<-r.dach.neg.binom(mu,sigma2)
p.dach.negb
r.dach.negb

#     Plot angepasste NegBin-Verteilung
y<-dnbinom(x,r.dach.negb,p.dach.negb)
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))


x<-0:10
lambda<-0.6
y<-dpois(x,lambda)
y
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))

#-------------------------------------------------#
#             Aufgabe 6.6                         #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
#     Erzeugung der Daten
library(actuar)
#     Simuliere n Pareto(1,2)-verteilte
x0<-1
n<-100
set.seed(4711)
data<-rpareto(n,2,x0)+x0

#     Alternativ: Daten von der Website benutzen und einlesen 

#-------------Teile (a-d)-------------------------#
#     Explorative Datenanalyse
par(mfrow=c(2,2))
f<-ecdf(data)
plot(f,lwd=2,xlab="Verluste",main="Empirische Verteilungsfunktion",ylab=expression(hat(F)(x)))
#lines(sort(data),pnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
curve(pexp(x,1/mean(data)),col="red",lwd=2,add=TRUE)
#qqnorm(data,xlab="Theoretische Quantile",ylab="Stichprobenquantile",main="Q-Q-Plot")
exp.data<-rexp(length(data),1/mean(data))
qqplot(exp.data,data)
hist(data,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Verluste",col="grey")
curve(dexp(x,1/mean(data)),col="red",lwd=2,add=TRUE)
#lines(sort(data),dnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
boxplot(data,horizontal=TRUE)

#       Alternativ Anpassung mit fitdistr
library(MASS)
fit.exp<-fitdistr(data,"exponential")
fit.exp$estimate

#-------------Teil (e)----------------------------#
ks.test(data,"pexp",1/mean(data))
#     Eigentlich sinnvoller
ks.test(data-x0,"pexp",1/mean(data-x0))

#-------------Teil (f)----------------------------#
#     Anpassung Lognorm-Verteilung
fit.lognorm<-fitdistr(data,"lognormal")
meanlog<-fit.lognorm[[1]][[1]]
sdlog<-fit.lognorm[[1]][[2]]
par(mfrow=c(2,2))
f<-ecdf(data)
plot(f,lwd=2,xlab="Verluste",main="Empirische Verteilungsfunktion",ylab=expression(hat(F)(x)))
#lines(sort(data),pnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
curve(plnorm(x,meanlog,sdlog),col="red",lwd=2,add=TRUE)
#qqnorm(data,xlab="Theoretische Quantile",ylab="Stichprobenquantile",main="Q-Q-Plot")
lnorm.data<-rlnorm(length(data),meanlog,sdlog)
qqplot(lnorm.data,data)
hist(data,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Verluste",col="grey")
curve(dlnorm(x,meanlog,sdlog),col="red",lwd=2,add=TRUE)
#lines(sort(data),dnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
boxplot(data,horizontal=TRUE)

#     KS-Test
ks.test(data,"plnorm",meanlog,sdlog)

#-------------------------------------------------#
#             Aufgabe 6.7                         #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.6

#-------------Teil (a)----------------------------#
#     Berechne Schätzer
alpha.moment<-1+1/(mean(data)/x0-1)
alpha.moment
alpha.ML<-length(data)/sum(log(data)-log(x0))
alpha.ML

#-------------Teil (b)----------------------------#
#     Darstellung Anpassung Pareto
par(mfrow=c(2,2))
f<-ecdf(data)
plot(f,lwd=2,xlab="Verluste",main="Empirische Verteilungsfunktion",ylab=expression(hat(F)(x)))
#lines(sort(data),pnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
curve(ppareto(x-1,alpha.ML,x0),col="red",lwd=2,add=TRUE)
#qqnorm(data,xlab="Theoretische Quantile",ylab="Stichprobenquantile",main="Q-Q-Plot")
pareto.data<-rpareto(length(data),alpha.ML,x0)+1
qqplot(pareto.data,data)
hist(data,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Verluste",col="grey")
curve(dpareto(x-1,alpha.ML,x0),col="red",lwd=2,add=TRUE)
#lines(sort(data),dnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
boxplot(data,horizontal=TRUE)

#     KS-Test
ks.test(data-1,"ppareto",alpha.ML,1)





#-------------------------------------------------#
#             Aufgabe 6.8                         #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
#   DAX-daten aus R-Datensatz
data(EuStockMarkets)
log.returns<-diff(log(EuStockMarkets[,"DAX"]))
data<-log.returns


#     Alternativ: Daten von der Website benutzen und einlesen

#-------------Teile (a,b)-------------------------#
#   Anpassung Normalverteilung
mu.normal<-mean(data)
sigma.normal<-sd(data)
#   Grafische Darstellung
par(mfrow=c(2,2))
f<-ecdf(data)
plot(f,lwd=2,xlab="Verluste",main="Empirische Verteilungsfunktion",ylab=expression(hat(F)(x)))
#lines(sort(data),pnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
curve(pnorm(x,mu.normal,sigma.normal),col="red",lwd=2,add=TRUE)
qqnorm(data,xlab="Theoretische Quantile",ylab="Stichprobenquantile",main="Q-Q-Plot")
hist(data,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Verluste",col="grey")
curve(dnorm(x,mu.normal,sigma.normal),col="red",lwd=2,add=TRUE)
#lines(sort(data),dnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
boxplot(data,horizontal=TRUE)

#     KS-Test Normalverteilung
ks.test(data,"pnorm",mu.normal,sigma.normal)


#-------------Teil (c)----------------------------#
#   Anpassung t-Verteilung
Anpa.t<-function(data,t.df=3){
fit.t<-fitdistr(data,"t",df=t.df)
mean.t<-fit.t[[1]][[1]]
sd.t<-fit.t[[1]][[2]]

#   Standardisierte Daten
data.stand<-(data-mean(data))*sqrt(t.df/(t.df-2))/sd(data)

#   Grafische Darstellung
par(mfrow=c(2,2))
f<-ecdf(data.stand)
plot(f,lwd=2,xlab="Verluste",main="Empirische Verteilungsfunktion",ylab=expression(hat(F)(x)))
#lines(sort(data),pnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
curve(pt(x,df=t.df),col="red",lwd=2,add=TRUE)
t.data<-rt(length(data),df=t.df)
qqplot(t.data,data)
hist(data.stand,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Verluste",col="grey",breaks=50)
curve(dt(x,df=t.df),col="red",lwd=2,add=TRUE)
#lines(sort(data),dnorm(sort(data),mean(data),sd(data)),col="red",lwd=2)
boxplot(data.stand,horizontal=TRUE)

#     KS-Test t-verteilung
d<-ks.test(data.stand,"pt",df=t.df)
return(d$p.value)
}

#     Anpassung verschiedene t- Verteilungen
Anpa.t(data,3)
Anpa.t(data,4)
Anpa.t(data,5)
Anpa.t(data,6)

#-------------------------------------------------#
#             Aufgabe 6.9                         #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.8

#-------------Teil (a)----------------------------#
#     Parametrischer VaR-Schätzer
VaR.est.P<-function(alpha=.95,l){
qnorm(alpha)*sd(l)
}
#     Nichtparametrischer VaR-Schätzer
VaR.est.NP<-function(alpha=.95,l){
quantile(l,alpha,type=1,names=FALSE)
}
#-------------Teil (b)----------------------------#
#     Mit data aus 6.8
VaR.est.P(.99,data)
VaR.est.NP(.99,data)
#-------------Teil (c)----------------------------#
#     Für den parametrischen VaR
VaR.KI.P<-function(alpha.VaR=.99,alpha.KI=.95,l){
#   Berechnet zweiseitiges KI gemäß S. 316
n<-length(l)
s<-sd(l)
breite.KI<-qnorm(alpha.VaR)*qnorm(alpha.KI+(1-alpha.KI)/2)*sd(l)*sqrt(2)/sqrt(n)
lower<-VaR.est.P(alpha.VaR,l)-breite.KI/2
upper<-VaR.est.P(alpha.VaR,l)+breite.KI/2
data.frame(lower, upper)
}
#    Aufruf
VaR.KI.P(,,data)

#-------------------------------------------------#
#             Aufgabe 6.10                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.8

#-------------Teil (a)----------------------------#
#     Parametrischer TVaR-Schätzer (Formel (6.9))
TVaR.est.P<-function(alpha=.95,l){
dnorm(qnorm(alpha))*sd(l)/(1-alpha)
}
#     Nichtparametrischer TVaR-Schätzer
TVaR.est.NP<-function(alpha=.95,l){
mean(l[l>VaR.est.P(alpha,l)])
}
#-------------Teil (b)----------------------------#
#     Mit data aus 6.8
TVaR.est.P(.99,data)
TVaR.est.NP(.99,data)
#-------------Teil (c)----------------------------#
#     Für den parametrischen VaR
TVaR.KI.P<-function(alpha.VaR=.99,alpha.KI=.95,l){
#   Berechnet zweiseitiges KI gemäß S. 321
n<-length(l)
s<-sd(l)
breite.KI<-dnorm(qnorm(alpha.VaR))/(1-alpha.VaR)*qnorm(alpha.KI+(1-alpha.KI)/2)*sd(l)*sqrt(2)/sqrt(n)
lower<-TVaR.est.P(alpha.VaR,l)-breite.KI/2
upper<-TVaR.est.P(alpha.VaR,l)+breite.KI/2
data.frame(lower, upper)
}
#    Aufruf
TVaR.KI.P(,,data)

#-------------------------------------------------#
#             Aufgabe 6.11                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.6

#-------------Teil (b)----------------------------#
#     Quantilfunktion der Paretoverteilung
quantil.Pareto<-function(p,x.0,alpha){
x.0*(1-p)^(-1/alpha)
}
Pareto.alpha.ML.est<-function(l,x.0){
#     x.0 bekannt
#     Aufgabe 6.5
n<-length(l)
ML.alpha.est<-n/(sum(log(l))-n*log(x.0))
return(ML.alpha.est)
}
#     Parametrischer VaR
VaR.est.Pareto.P<-function(l,VaR.alpha,x.0){
quantil.Pareto(VaR.alpha,x.0,Pareto.alpha.ML.est(l,x.0))
}
#     Nichtüarametrischer VaR
VaR.est.NP<-function(l,VaR.alpha){quantile(data,0.99,type=1,names=FALSE)}
#     Aufrufe
VaR.est.Pareto.P(data,0.99,1)
VaR.est.NP(data,0.99)

#-------------------------------------------------#
#             Aufgabe 6.12                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.6

#-------------Teil (b)----------------------------#
#     Quantilfunktion der Lognormalverteilung
quantil.lognorm<-function(p,mu,sigma){
exp(mu+sigma*qnorm(p))
}
#     Parametrischer VaR
VaR.est.lognorm.P<-function(l,VaR.alpha){
mu<-mean(log(l))
sigma<-sd(log(l))
quantil.lognorm(VaR.alpha,mu,sigma)
}
#     Aufrufe
VaR.est.lognorm.P(data,0.99)
VaR.est.NP(data,0.99)

#-------------------------------------------------#
#             Aufgabe 6.13                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.8

#     Pakete laden                
library(copula)            
library(QRMlib)
library(evir)

plot(ecdf(data[,1]))
pairs(data)
#-------------Teil (a)----------------------------#
rho.S<-Spearman(data)
rho.S
rho.K<-Kendall(data,noforce=FALSE)
rho.K
#     Schätzer der Copulaparameter 
rho.gauss<-rho.S
rho.t<-sin(pi*rho.K/2)
rho.Gumbel<-1/(1-rho.K)
rho.Clayton<-2*rho.K/(1-rho.K)
list(rho.gauss,rho.t,rho.Gumbel,rho.Clayton)

#-------------Teil (b)----------------------------#
z<-data
#     Gemeinsame Nullwerte entfernt (s. Carmona, Zivot)
z.ohne.0<-z[z[,1]!=0 & z[,2]!=0 & z[,3]!=0 & z[,4]!=0,]
emp.DAX<-ecdf(z.ohne.0[,1])
emp.SMI<-ecdf(z.ohne.0[,2])
emp.CAC<-ecdf(z.ohne.0[,3])
emp.FTSE<-ecdf(z.ohne.0[,4])
vert<-function(z){
c(emp.DAX(z[1]),emp.SMI(z[2]),emp.CAC(z[3]),emp.FTSE(z[4]))
}
#   Renormierung der empirischen Verteilungsfunktion 
#   mit n+1 statt n um Konvergenzprobleme bei Maximierung der 
#   Likelihoodfunktion zu vermeiden
z.neu<-t(apply(z.ohne.0,1,FUN=vert))
daten<-z.neu*length(z.neu)/(length(z.neu)+1)
pairs(daten,labels=c("DAX","SMI","CAC","FTSE"))

#   Anpassung  gausscopula (QRMlib)
fit.gauss <- fit.gausscopula(daten)
fit.gauss

#   Anpassung  tcopula (QRMlib)
fit.t <- fit.tcopula(daten)
fit.t

#   Anpassung  Clayton (QRMlib)
fit.clayton <- fit.Archcopula2d(daten[,c(1,4)],"clayton")
fit.clayton

#   Anpassung  Gumbel (QRMlib)
fit.gumbel <- fit.Archcopula2d(daten[,c(1,4)],"gumbel")
fit.gumbel

#-------------------------------------------------#
#             Aufgabe 6.14                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.8

#-------------Teil (0)----------------------------#
#     Nützliche Funktionen
#	    VaR bei Normalverteilung mit EW und Var bekannt
VaR.normal<-function(mu=0,sigma=1,alpha=.99){
mu+sigma*qnorm(alpha)
}
#	    Parametrischer VaR-Schätzer
VaR.est.P<-function(alpha=.99,l){
sd(l)*qnorm(alpha)
}
VaR.KI.P<-function(alpha.VaR=.99,alpha.KI=.95,l){
#   Berechnet zweiseitiges KI von....
#
n<-length(l)
s<-sd(l)
breite.KI<-qnorm(alpha.VaR)*qnorm(alpha.KI+(1-alpha.KI)/2)*sd(l)*sqrt(2)/sqrt(n)
lower<-VaR.est.P(alpha.VaR,l)-breite.KI/2
upper<-VaR.est.P(alpha.VaR,l)+breite.KI/2
data.frame(lower, upper)
}

#	    Nichtparametrischer VaR-Schätzer
VaR.est.NP<-function(alpha=.99,l){
quantile(l,alpha,type=1)
}
#-------------Teil (a)----------------------------#
#     Selektiere Backtesting Daten
Select.Data<-function(data,T=252){
s<-1:(length(data)-(T+1))
rows<-T
columns<-length(s)
btd<-as.array(seq(1:(rows*columns)))
dim(btd)<-c(rows,columns)
for (i in 1:T){
    btd[i,]<-data[s+(i-1)]
}
return(btd)
}

#     DAX-Verluste
data<--z.ohne.0[,1]
btd<-Select.Data(data,T=252) 
T<-252
True.Losses<-data[(length(data)-(T-1)):length(data)]

#     Parametric VaR
Estimated.P.VaR<-apply(btd,1,FUN=VaR.est.P,alpha=.99)
plot(Estimated.P.VaR,type="l",ylab="P & L",ylim=c(min(min(Estimated.P.VaR),min(True.Losses)),max(max(Estimated.P.VaR),max(True.Losses))))
lines(True.Losses,type="h")
Differ.P<-True.Losses-Estimated.P.VaR
index.P<-subset(1:T,Differ.P>0)
Ausreisser.P<- Estimated.P.VaR[index.P]
points(index.P,Ausreisser.P,pch=16)
#-------------Teil (b)----------------------------#
#      Approx. Binomialtest (von Hand)
aus<-length(index.P)
z<-(aus-T*0.01)/sqrt(T*0.99*0.01)
p.value<-2*(1-pnorm(z))

#       Mit prop.test
prop.test(aus,T,0.01,"two.sided")
#       Konsistenz mit Approx. Binomialtest oben (ohne Korrektur)
prop.test(aus,T,0.01,"two.sided",correct=FALSE)

#   Kupiec-Test
LR<-function(p.0,p.dach,n.Ausr,T){
L.0<-p.0^n.Ausr*(1-p.0)^(T-n.Ausr)
L.A<-p.dach^n.Ausr*(1-p.dach)^(T-n.Ausr)
-2*(log(L.0)-log(L.A))
}

#   Backtest über 504 Tage
T<-252
p.0<-0.01
#   Parametrisch
p.dach<-aus/T
LR(p.0,p.dach,aus,T)  
#    Vergleich mit Chi-Quadrat
qchisq(0.95,1)

#-------------------------------------------------#
#             Aufgabe 6.15                        #
#-------------------------------------------------#
#     Analog Aufgabe 6.14 mit VaR.est.NP statt VaR.est.P
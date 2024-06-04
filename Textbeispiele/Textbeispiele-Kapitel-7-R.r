#-------------------------------------------------#
#   Beispiel in 7.2.4                             #
#-------------------------------------------------#
library(copula)
#     Definiere Copulas
gauss.cop<-mvdc(normalCopula(0.66), c("unif", "unif"),
          list(list(min=0,max=1), list(min=0,max=1)))

t.cop<-mvdc(tCopula(0.67,df=5.5), c("unif", "unif"),
          list(list(min=0,max=1), list(min=0,max=1)))
gumbel.cop<-mvdc(gumbelCopula(1.77), c("unif", "unif"),
          list(list(min=0,max=1), list(min=0,max=1)))
clayton.cop<-mvdc(claytonCopula(1.27), c("unif", "unif"),
          list(list(min=0,max=1), list(min=0,max=1)))

#     Erzeuge Simulationen
set.seed(4711)
gauss.cop.sim <- rmvdc(gauss.cop, 5000)
set.seed(4711)
t.cop.sim <- rmvdc(t.cop, 5000)
set.seed(4711)
gumbel.cop.sim <- rmvdc(gumbel.cop, 5000)
set.seed(4711)
clayton.cop.sim <- rmvdc(clayton.cop, 5000)

#   Plots (entspricht Abbildung 7.3)
par(mfrow=c(2,4))
par(mar=c(2,2,3,1)+0.1)


persp(gauss.cop, dmvdc,xlim=c(0,1),ylim=c(0,1),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )
persp(t.cop, dmvdc,xlim=c(0,1),ylim=c(0,1),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )
persp(gumbel.cop, dmvdc,xlim=c(0,1),ylim=c(0,1),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )
persp(clayton.cop, dmvdc,xlim=c(0,1),ylim=c(0,1),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )

plot(gauss.cop.sim,xlab="x",ylab="y")  
plot(t.cop.sim,xlab="x",ylab="y")  
plot(gumbel.cop.sim,xlab="x",ylab="y")   
plot(clayton.cop.sim,xlab="x",ylab="y")

#-------------------------------------------------#
#   Beispiel in 7.2.5                             #
#-------------------------------------------------#
library(copula)
#     Definiere Copulas
gauss.cop<-mvdc(normalCopula(0.66), c("norm", "norm"),
          list(list(mean=0,sd=1), list(mean=0,sd=1)))

t.cop<-mvdc(tCopula(0.67,df=5.5), c("norm", "norm"),
          list(list(mean=0,sd=1), list(mean=0,sd=1)))
gumbel.cop<-mvdc(gumbelCopula(1.77), c("norm", "norm"),
          list(list(mean=0,sd=1), list(mean=0,sd=1)))
clayton.cop<-mvdc(claytonCopula(1.27), c("norm", "norm"),
          list(list(mean=0,sd=1), list(mean=0,sd=1)))

#     Erzeuge Simulationen
set.seed(4711)
gauss.cop.sim <- rmvdc(gauss.cop, 5000)
set.seed(4711)
t.cop.sim <- rmvdc(t.cop, 5000)
set.seed(4711)
gumbel.cop.sim <- rmvdc(gumbel.cop, 5000)
set.seed(4711)
clayton.cop.sim <- rmvdc(clayton.cop, 5000)

#   Plots (entspricht Abbildung 7.4)
par(mfrow=c(2,4))
par(mar=c(2,2,3,1)+0.1)


contour(gauss.cop, dmvdc,xlim=c(-4,4),ylim=c(-4,4),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )
contour(t.cop, dmvdc,xlim=c(-4,4),ylim=c(-4,4),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )
contour(gumbel.cop, dmvdc,xlim=c(-4,4),ylim=c(-4,4),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )
contour(clayton.cop, dmvdc,xlim=c(-4,4),ylim=c(-4,4),shade=1,expand=.5,xlab="x",ylab="y",zlab="",
ticktype = "detailed" )

plot(gauss.cop.sim,xlim=c(-4,4),ylim=c(-4,4),xlab="x",ylab="y")  
plot(t.cop.sim,xlim=c(-4,4),ylim=c(-4,4),xlab="x",ylab="y")  
plot(gumbel.cop.sim,xlim=c(-4,4),ylim=c(-4,4),xlab="x",ylab="y")   
plot(clayton.cop.sim,xlim=c(-4,4),ylim=c(-4,4),xlab="x",ylab="y")

#     Extreme Verluste (Tabelle 7.1)
#     Analog mit qnorm(0.01)
x.cutoff<-qnorm(0.05)
y.cutoff<-qnorm(0.05)

gauss.cop.exceed<-gauss.cop.sim[gauss.cop.sim[,1]<=x.cutoff & gauss.cop.sim[,2]<=y.cutoff,]
t.cop.exceed<-t.cop.sim[t.cop.sim[,1]<=x.cutoff & t.cop.sim[,2]<=y.cutoff,]
gumbel.cop.exceed<-gumbel.cop.sim[gumbel.cop.sim[,1]<=x.cutoff & gumbel.cop.sim[,2]<=y.cutoff,]
clayton.cop.exceed<-clayton.cop.sim[clayton.cop.sim[,1]<=x.cutoff & clayton.cop.sim[,2]<=y.cutoff,]
x.cutoff
y.cutoff
extremes<-c(nrow(gauss.cop.exceed),nrow(t.cop.exceed),nrow(gumbel.cop.exceed),nrow(clayton.cop.exceed))
extremes
extremes/5000

#-------------------------------------------------#
#   Beispiel 7.3.2.1                              #
#-------------------------------------------------#
T<-12
p<-0.5
x<-rbinom(T,1,p)
y<-c(0,cumsum(x))
t<-0:T
plot(t,y,type="l")
points(t,y,pch=19)

#-------------------------------------------------#
#   Beispiel 7.3.2.2                              #
#-------------------------------------------------#
set.seed(4711)
lambda<-5
zeit.horizont<-3
t0<-rexp(1,lambda)
while (sum(t0)<=zeit.horizont){
    t1<-rexp(1,lambda)
    t0<-c(t0,t1)
}
t<-cumsum(t0)
t<-t[t<=zeit.horizont]
y<-0:length(t)
pfad<-stepfun(t,y)
#     Erzeugt Abb. 7.6
plot(pfad,verticals=FALSE,lwd=3,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,3),pch=20)
rug(t)






#-------------------------------------------------#
#   Beispiel in 7.3.3                             #
#-------------------------------------------------#
#   Definiere Intensitätsfunktion
Intensity.3<-function(t){
if (((t-floor(t))<=1/12) | ((t-floor(t))>=9/12))
  y<-8
else
  y<-2
return(y)
}

#     Ausdünnungsmethode                                   
Sim.I.PP.Thin.1<-function(Intensity,lambda.gross=60,zeit.horizont=10,n.Zeiten=100){
t1<-0
t.alt<-0
for (i in 1:n.Zeiten){
  t.neu<-t.alt+rexp(1,lambda.gross)
  u<-runif(1)
  if (u<=Intensity(t.neu)/lambda.gross){
    t1<-c(t1,t.neu)
    }
  t.alt<-t.neu
}
t<-t1[(t1<=zeit.horizont) & (t1>0)]
return(t)
}



#     Plot mittels Ausdünnung
Plot.Sim.I.PP.Thin.1<-function(Intensity,lambda.gross=60,zeit.horizont=10,n.Zeiten=100){
t<-Sim.I.PP.Thin.1(Intensity,lambda.gross,zeit.horizont,n.Zeiten)
y<-0:length(t)
pfad<-stepfun(t,y)
plot(pfad,verticals=FALSE,lwd=3,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,zeit.horizont),axes=FALSE,pch=20)
rug(t)
}

#     Aufrufe
t<-seq(0,3,0.0001)
set.seed(4711)
y<-sapply(t,Intensity.3)
#     Erzeugt Abbildung 7.8
par(mfcol=c(2,1))
par(mar=c(4,4,1,1)+0.1)
#     Plot der Intensitätsfunktion
plot(t,y,type="l",xlab="t",ylim=c(0,max(y)),ylab=expression(lambda),axes=FALSE)
axis(2,ylab=expression(lambda))
axis(1,c(0,1,2,3))
box() 
#     Plot des Pfades
Plot.Sim.I.PP.Thin.1(Intensity.3,10,3,1000)
axis(2)
axis(1,c(0,1,2,3))
box() 



#-------------------------------------------------#
#   Beispiel in 7.3.5                             #
#-------------------------------------------------#
#   Definiere Intensitätsfunktion (deterministisch!)
Intensity.4<-function(t){
u.1<--1.2
u.2<-1.3
u.3<--0.3
u.4<-0.1
y<-2
if (((t-floor(t))<=1/12) | ((t-floor(t))>=9/12)){
  if (t<=1/12){y<-8+u.1} else
  if ((t>=9/12) & (t<=13/10)){y<-8+u.2} else
  if ((t>=21/12) & (t<=23/10)){y<-8+u.3} else
  if (t>=33/12){y<-8+u.4} 
  }
return(y)
}
#     Cox-Prozess (Ausdünnung)                                   #
Sim.Cox.1<-function(Intensity,lambda.gross=60,zeit.horizont=3,n.Zeiten=100){
t1<-Sim.I.PP.Thin.1(Intensity,lambda.gross,zeit.horizont,n.Zeiten)
return(t1)
}
#     Erzeugt Abbildung 7.9
t<-seq(0,3,0.001)
#u<-runif(3,-1,1)
y<-sapply(t,FUN=Intensity.4)
par(mfcol=c(2,1))
par(mar=c(4,4,1,1)+0.1)
plot(t,unlist(y),type="l",xlab="t",ylim=c(0,max(y)),ylab=expression(lambda),axes=FALSE)
axis(1,c(0,1,2,3))
axis(2)
box() 
set.seed(4711)
Plot.Sim.I.PP.Thin.1(Intensity.4,10,3,1000)
axis(2)
axis(1,c(0,1,2,3))
box() 


#-------------------------------------------------#
#   Beispiel in 7.4                               #
#-------------------------------------------------#
set.seed(4711)
#     Simuliere Cox-Prozess
t<-Sim.I.PP.Thin.1(Intensity.4,10,3,1000)
y<-0:length(t)
pfad<-stepfun(t,y)
#     Erzeuge Schadenhöhen Exp(1)
y<-rexp(length(t)+1)
#     Erzeuge Abb. 7.10
pp.y<-cumsum(y)
pfad<-stepfun(t,pp.y)
plot(pfad,verticals=FALSE,lwd=3,ylab="S(t)",xlab="t",main="",axes=FALSE,pch=20,xlim=c(0,3))
rug(t)
axis(2)
axis(1,c(0,1,2,3))
box() 
rug(t)

#-------------------------------------------------#
#   Beispiel 7.5.2.1                              #
#-------------------------------------------------#
sigma<-1
delta.t<-0.001
t<-seq(0,1,delta.t)
set.seed(4711)
epsilon<-rnorm(length(t)-1)
zuwachs<-sigma*sqrt(delta.t)*epsilon
w<-c(0,cumsum(zuwachs))
plot(t,w,type="l")

#     Mit Drift und Volatilität
mu<-0.04
sigma<-0.2
w.mit.drift<-mu*t+sigma*w
plot(t,w.mit.drift,type="l")

#     geometrische BB
g<-exp(w)
plot(t,g,type="l")

#-------------------------------------------------#
#   Beispiel 7.5.3.1                              #
#-------------------------------------------------#
updown<-function(x,u,d){(u+d)*x-d}
set.seed(4711)
epsilon.1<-rbinom(100,1,0.5)
epsilon<-sapply(epsilon.1,FUN=updown,u=0.11797,d=0.11297)
epsilon
G<-c(0,cumsum(epsilon))
K.0<-1
K<-K.0*exp(G)
plot(K,type="l",lwd=2,xlab="t",ylab=expression(K[t]))

#-------------------------------------------------#
#   Beispiel in 7.6.1                             #
#-------------------------------------------------#
f<-function(x){sin(pi*x)}
plot.f.y.n<-function(x,f){
y<-as.numeric(lapply(x,FUN=f))
x.plot<-seq(0,1,0.01)
y.plot<-lapply(x.plot,FUN=f)
plot(x.plot,y.plot,type="l",lwd=2,xlab="u",ylab="g(u)")
rug(x,pch=3)
rug(y,side=2)
points(x,y,pch=15)
rug(mean(y),side=2,lwd=3)

x.0<-x
x.1<-x
y.0<-0
y.1<-f(x.0)
#segments(x.0,y.0,x.1,y.1,lty=3)
arrows(x.0,y.0,x.1,y.1,lty=1,length=0.2,angle=20)

a.0<-x
a.1<-0
b.0<-f(x.0)
b.1<-f(x.0)
#segments(a.0,b.0,a.1,b.1,lty=3)
arrows(a.0,b.0,a.1,b.1,lty=1,length=0.2,angle=20)

return(mean(y))
}

#     Erzeugt Abb. 7.15, weitere Funktionsaufrufe zur 
#     Berechnung der numerischen Werte
set.seed(4711)
x<-runif(10)
a<-plot.f.y.n(x,f)

set.seed(4711)
x<-runif(100)
b<-plot.f.y.n(x,f)


set.seed(4711)
x<-runif(1000)
c<-plot.f.y.n(x,f)

set.seed(4711)
x<-runif(10000)
d<-plot.f.y.n(x,f)

s<-c(a,b,c,d)
s-2/pi

#-------------------------------------------------#
#   Beispiel in 7.6.2                             #
#-------------------------------------------------#
#     Definiere Funktion, die nichparametrischen VaR 
#     für gegebenen Verlustvektor auswertet
VaR.est.NP<-function(alpha=.95,l){
quantile(l,alpha,type=1,names=FALSE)
}

#     Verwende Simulationen aus "Beispiel in 7.2.5"
#     Für Gauss-Copula
sum.gauss.cop<-apply(-gauss.cop.sim,1,FUN=sum)
VaR.gauss.cop<-VaR.est.NP(.95,sum.gauss.cop)
VaR.gauss.cop

#     Alternativ als Funktion
VaR.est.NP.multi<-function(alpha=.95,vektor){
VaR.est.NP(alpha,-rowSums(vektor))
}
#     Aufrufe
VaR.est.NP.multi(.95,gauss.cop.sim)
VaR.est.NP.multi(.95,t.cop.sim)
VaR.est.NP.multi(.95,gumbel.cop.sim)
VaR.est.NP.multi(.95,clayton.cop.sim)
VaR.est.NP.multi(.99,gauss.cop.sim)
VaR.est.NP.multi(.99,t.cop.sim)
VaR.est.NP.multi(.99,gumbel.cop.sim)
VaR.est.NP.multi(.99,clayton.cop.sim)

#-------------------------------------------------#
#   Beispiel in 7.6.3                             #
#-------------------------------------------------#   
library(copula)
#   Für Gauss-Copula (andere Copulas analog)
#  Gauss-Copula (beachte Parametrisierung der Lognormalverteilungen)
gauss.cop.bspl<- mvdc(normalCopula(0.2), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
set.seed(4711)
sim.pf.gauss<-rmvdc(gauss.cop.bspl,5000)
sim.pf<-sim.pf.gauss
gesamt.verluste<-sim.pf[,1]+sim.pf[,2]
q<-quantile(gesamt.verluste,.95,type=1,names=FALSE)
tce<-mean(gesamt.verluste[gesamt.verluste>q])
ac.1<-mean(sim.pf[sim.pf[,1]+sim.pf[,2]>q,1])
ac.2<-mean(sim.pf[sim.pf[,1]+sim.pf[,2]>q,2])
ac.1.rel<-ac.1/tce
ac.2.rel<-ac.2/tce
data.frame(tce,ac.1,ac.2,ac.1.rel,ac.2.rel)

#-------------------------------------------------#
#   Beispiel in 7.6.4                             #
#-------------------------------------------------# 
#     Library actuar für Pareto-Verteilung
library(actuar)  
#     Simuliert homogenen Poisson-Prozess
Sim.PP.2<-function(lambda=2,zeit.horizont=10){
t0<-rexp(1,lambda)
while (sum(t0)<=zeit.horizont){
    t1<-rexp(1,lambda)
    t0<-c(t0,t1)
}
t<-cumsum(t0)
t<-t[t<=zeit.horizont]
return(t)
}
#     Reserve ohne Schäden                        
reserve<-function(t,u.0,c){u.0+c*t}
#     Simulation Risikoreserveprozess
sim.pfad.rr.prozess<-function(u.0=100,c=5,lambda=5,x,zeit.horizont=100,anzahl.t.werte=1000){
#   Simuliert einen Risiko-reserveprozess über 
#   Zeithorizont [0,T]
#   Schadenanzahl:  Homogener Poisson-Prozess mit
#   Intensität lambda
#   Ausgabe: Zeitpunkt des Ruins bzw.
#   Eingabe:  u.0:    Anfangskapital
#             c:      Prämieneinnahme pro Zeiteinheit
#             x:      Schadenhöhenverteilung
#   Schadenzeiten
pp<-Sim.PP.2(lambda,zeit.horizont)
Anz.Schaeden<-length(pp)
schaeden<-eval(x)
kum.schaeden<-c(0,cumsum(schaeden))
pfad.risk<-stepfun(pp,kum.schaeden,right=FALSE)
t<-seq(0,zeit.horizont,length=anzahl.t.werte)
risk.y<-sapply(t,FUN=pfad.risk)
reserve.y<-sapply(t,FUN=reserve,u.0,c)
risk.reserve.y<-reserve.y-risk.y
return(risk.reserve.y)
}

#     Aufrufe
t<-seq(0,100,length=1000)
set.seed(4711)
x.1<-substitute(rpareto(Anz.Schaeden,1.5,0.5))
y.1<-replicate(10, sim.pfad.rr.prozess(,,,x.1,,))
x.2<-substitute(rexp(Anz.Schaeden))
y.2<-replicate(10, sim.pfad.rr.prozess(,,,x.2,,))

#     Erzeuge Abb 7.17
upper<-max(y.1)
lower<-min(0)
par(mfcol=c(1,2))
plot(t,y.1[,1],ylim=c(lower,upper),main="Pareto-Schadenhöhen",type="l",ylab="R(t)",lwd=1)
for (i in 2:10) lines(t,y.1[,i],lwd=1)
plot(t,y.2[,1],ylim=c(lower,upper),main="Exp-Schadenhöhen",type="l",ylab="R(t)",lwd=1)
for (i in 2:10) lines(t,y.2[,i],lwd=1)
dev.off()


#     Simuliere Ruinwahrscheinlichkeiten etc.
ruin<-function(u.0,c,lambda=2,x,zeit.horizont=10){
#   Simuliert einen Risiko-reserveprozess über 
#   Zeithorizont [0,T] und gibt Ruinzeitpunkt zurück
#   Schadenanzahl:  Homogener Poisson-Prozess mit
#   Intensität lambda
#   Ausgabe: Zeitpunkt des Ruins bzw.
#   Eingabe:  u.0:    Anfangskapital
#             c:      Prämieneinnahme pro Zeiteinheit
#             x:      Schadenhöhenverteilung
#   Schadenzeiten
t<-Sim.PP.2(lambda,zeit.horizont)
Anz.Schaeden<-length(t)
schaeden<-eval(x)
kum.schaeden<-cumsum(schaeden)
reserve.schadenzeiten<-sapply(t,FUN=reserve,u.0=u.0,c=c)
risk.reserve<-reserve.schadenzeiten-kum.schaeden
ruin.index<-min(which(risk.reserve<0))
ruin.zeitpunkt<-t[ruin.index]
return(ruin.zeitpunkt)
}
#     Aufrufe
#     Schadenhöhenverteilungen
x.1<-substitute(rpareto(Anz.Schaeden,1.5,0.5))
x.2<-substitute(rexp(Anz.Schaeden))
set.seed(4711)
z.1<-replicate(10000,ruin(100,5,5,x.1,100))
z.2<-replicate(10000,ruin(100,5,5,x.2,100))
#     Ausschluss der Realisierungen in denen kein Ruin stattgefunden hat
z.1.ruin<-z.1[is.na(z.1)=="FALSE"]
prob.1<-length(z.1.ruin)/length(z.1)
prob.1
mean(z.1.ruin)
z.2.ruin<-z.2[is.na(z.2)=="FALSE"]
prob.2<-length(z.2.ruin)/length(z.2)
prob.2
mean(z.2.ruin)


#-------------------------------------------------#
#   Beispiel 7.7.2                                #
#-------------------------------------------------# 

library(fBasics)	#	Lade Paket fBasics
data(nyse)
x<-nyse[3001:4000,2]
z1<-log(1/x[1]*x)
data<--diff(z1)

#	    VaR-Schätzer
#	    Parametrischer VaR-Schätzer
VaR.est.P<-function(alpha=.99,l){
sd(l)*qnorm(alpha)
}
#     Konfidenzintervall für VaR (parametrischer Schätzer)
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


#     Bootstrap KI 
#     Lade Paket bootstrap
library(bootstrap)

VaR.KI.boot<-function(alpha.VaR=.99,alpha.KI=.95,l,R=30){
#   Berechnet zweiseitiges nichtparametrisches Boostrap-KI 
#   R:    Anzahl Boostrapwiederholungen
set.seed(4711)
g<-bootstrap(data,R,VaR.est.NP,alpha=alpha.VaR)   
q<-quantile(g$thetastar,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
#   boxplot(g$thetastar)
hist(g$thetastar,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g$thetastar,xlab="Theoretische Quantile",ylab="Stichprobenquantile")
#qqline(g$thetastar)
return(q)
}
#     Aufrufe
#     Schätzer
VaR.est.P(.95,data)
VaR.est.NP(.95,data)
VaR.est.P(.99,data)
VaR.est.NP(.99,data)
#     Konfidenzintervalle
VaR.KI.boot(.95,,,10000)
VaR.KI.boot(.99,,,10000)

#-------------------------------------------------#
#   Beispiel 7.7.3                                #
#-------------------------------------------------# 
#   Lade Paket actuar für die Pareto-Verteilung
library(actuar)

#   Simulation Compound-Poisson-Verteilung
#   mit Schadenhöhe Pareto-verteilt
#   Eine Realisierung
sim.cp.1<-function(lambda.poisson=2,...){
N.sim<-rpois(1,lambda.poisson)
Schaeden<-rpareto(N.sim,...)
return(sum(Schaeden))
}

#   Erzeuge mehrere Realisierungen
sim.cp<-function(N.realis=100,lambda.poisson=0.15,pareto.shape=0.42,pareto.scale=2){
sim<-replicate(N.realis,sim.cp.1(lambda.poisson,pareto.shape,pareto.scale))
return(sim)
}
#   Quantilsimulation
VaR.cp<-function(alpha.VaR=.99,N.realis=1000,lambda.poisson=0.15,
pareto.shape=0.42,pareto.scale=2){
#   Berechnet zweiseitiges Boostrap-KI für den Value-at-Risk
#   N.realis:    Anzahl Boostrapwiederholungen
sim<-sim.cp(N.realis,lambda.poisson,pareto.shape,pareto.scale) 
q<-quantile(sim,alpha.VaR,type=1,names=FALSE)
return(q)
}

#     Berechnet Bootstrap-KI und stellt Grafiken dar
VaR.cp.KI.boot<-function(alpha.VaR=.99,alpha.KI=.95,N.Sim=1000,N.realis=100,lambda.poisson=0.15,
pareto.shape=0.42,pareto.scale=2){
#   Berechnet zweiseitiges Boostrap-KI von....
#   R:    Anzahl Boostrapwiederholungen
set.seed(4711)
g<-replicate(N.Sim,VaR.cp(alpha.VaR,N.realis,lambda.poisson,pareto.shape,pareto.scale))   
q<-quantile(g,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
#   boxplot(g$thetastar)
hist(g,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g,xlab="Theoretische Quantile",ylab="Stichprobenquantile")
#qqline(g)
return(q)
}

#       Aufrufe
#       Bootstrap-KI für VaR(95%)
VaR.cp.KI.boot(.95,.95,1000,10000,,,)
#        Bootstrap-KI für VaR(99%)
VaR.cp.KI.boot(.99,.95,1000,10000,,,)


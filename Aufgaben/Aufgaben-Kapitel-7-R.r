#-------------------------------------------------#
#             Aufgabe 7.1                         #
#-------------------------------------------------#

#-------------Teil (b)----------------------------#
N.1<-10
N.2<-1000
N.3<-10000
N.4<-100000
set.seed(1234)
x.1<-rbinom(N.1,5,0.5)
x.2<-rbinom(N.2,5,0.5)
x.3<-rbinom(N.3,5,0.5)
x.4<-rbinom(N.4,5,0.5)
mean(x.1)
var(x.1)
mean(x.2)
var(x.2)
mean(x.3)
var(x.3)
mean(x.4)
var(x.4)

#-------------Teil (c)----------------------------#
#             Poissonverteilung
set.seed(1234)
x.1<-rpois(N.1,1)
x.2<-rpois(N.2,1)
x.3<-rpois(N.3,1)
x.4<-rpois(N.4,1)
mean(x.1)
var(x.1)
mean(x.2)
var(x.2)
mean(x.3)
var(x.3)
mean(x.4)
var(x.4)

#             Normalverteilung
set.seed(1234)
x.1<-rnorm(N.1)
x.2<-rnorm(N.2)
x.3<-rnorm(N.3)
x.4<-rnorm(N.4)
mean(x.1)
var(x.1)
mean(x.2)
var(x.2)
mean(x.3)
var(x.3)
mean(x.4)
var(x.4)

#             Exponentialverteilung
set.seed(1234)
x.1<-rexp(N.1)
x.2<-rexp(N.2)
x.3<-rexp(N.3)
x.4<-rexp(N.4)
mean(x.1)
var(x.1)
mean(x.2)
var(x.2)
mean(x.3)
var(x.3)
mean(x.4)
var(x.4)

#-------------------------------------------------#
#             Aufgabe 7.2                         #
#-------------------------------------------------#

#   Definiere Funktion, die x und y Vektor erzeugt
Sim.XY<-function(N){
x<-runif(N,-1,1)
y<-x^2
return(cbind(x,y))
}
#   Rufe Funktion auf und berechne Korrelationsmatrix
set.seed(1234)
cor(Sim.XY(10))
cor(Sim.XY(100))
cor(Sim.XY(1000))
cor(Sim.XY(10000))


#-------------------------------------------------#
#             Aufgabe 7.3                         #
#-------------------------------------------------#

#-------------Teil (b)----------------------------#
quantil.Pareto<-function(p,x.0,alpha){
x.0*(1-p)^(-1/alpha)
}

#-------------Teil (c)----------------------------#
set.seed(4711)
u<-runif(10)  # Erzeugt die Zufallszahlen aus der Aufgabenstellung
x<-sapply(u,FUN=quantil.Pareto,x.0=1,alpha=2)
x

#-------------------------------------------------#
#             Aufgabe 7.5                         #
#-------------------------------------------------#
library(copula)
#-------------Teil (a)----------------------------#
#  Gauss-Copula (beachte Parametrisierung der Lognormalverteilungen)
gauss.cop.bspl<- mvdc(normalCopula(0.2), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
set.seed(4711)
sim.pf.gauss<-rmvdc(gauss.cop.bspl,5000)
plot(sim.pf.gauss)
#-------------Teil (b)----------------------------#
cor(sim.pf.gauss)
#-------------Teil (c)----------------------------#
#   Plots der alternativen Portfolios
#   t-Copula
t.cop.bspl<- mvdc(tCopula(0.18,df=2), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
set.seed(4711)     
sim.pf.t<-rmvdc(t.cop.bspl,5000)
#   Gumbel-Copula
gumbel.cop.bspl<- mvdc(gumbelCopula(1.13), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
set.seed(4711)    
sim.pf.gumbel<-rmvdc(gumbel.cop.bspl,5000)
#   Clayton-Copula
clayton.cop.bspl<- mvdc(claytonCopula(0.26), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
set.seed(4711)    
sim.pf.clayton<-rmvdc(clayton.cop.bspl,5000)
par(mfcol=c(2,2))
plot(sim.pf.gauss,xlim=c(0.6,1.5),ylim=c(0.6,1.3))
plot(sim.pf.t,xlim=c(0.6,1.5),ylim=c(0.6,1.3))
plot(sim.pf.gumbel,xlim=c(0.6,1.5),ylim=c(0.6,1.3))
plot(sim.pf.clayton,xlim=c(0.6,1.5),ylim=c(0.6,1.3))


#-------------------------------------------------#
#             Aufgabe 7.6(Fortsetzung Aufgabe 7.5)#
#-------------------------------------------------#
#-------------Teil (b)----------------------------#
sim.ev.gauss<-cbind(0.7*sim.pf.gauss[,1],0.6*sim.pf.gauss[,2])
sim.ev.t<-cbind(0.7*sim.pf.t[,1],0.6*sim.pf.t[,2])
sim.ev.gumbel<-cbind(0.7*sim.pf.gumbel[,1],0.6*sim.pf.gumbel[,2])
sim.ev.clayton<-cbind(0.7*sim.pf.clayton[,1],0.6*sim.pf.clayton[,2])
#-------------Teil (c)----------------------------#
sim.rv.gauss<-cbind(0.3*sim.pf.gauss[,1],0.4*sim.pf.gauss[,2])
sim.rv.t<-cbind(0.3*sim.pf.t[,1],0.4*sim.pf.t[,2])
sim.rv.gumbel<-cbind(0.3*sim.pf.gumbel[,1],0.4*sim.pf.gumbel[,2])
sim.rv.clayton<-cbind(0.3*sim.pf.clayton[,1],0.4*sim.pf.clayton[,2])


#-------------------------------------------------#
#             Aufgabe 7.7                         #
#-------------------------------------------------#
#	  Parameter der Normalverteilung
mu <- c(0,0)
var.1<-1
var.2<-1
#   Modifiziere Grafikparameter
par(mar=c(4,4,2,2)+0.1)
par(mfrow=c(2,4))
n<-1000
rho<-c(-1,-0.9,-0.5,0,0.5,0.9,1)
for (i in 1:length(rho)){
cov<-rho[i]*sqrt(var.1*var.2)
Sigma <- matrix(c(var.1,cov,cov,var.2), 2,2)
plot(rmvnorm(n,mu,Sigma),xlab="x",ylab="y")
}

#-------------------------------------------------#
#             Aufgabe 7.8                         #
#-------------------------------------------------#
#     TBD

#-------------------------------------------------#
#             Aufgabe 7.9                         #
#-------------------------------------------------#
#-------------Teil (a)----------------------------#
Sim.PP<-function(lambda=5,zeit.horizont=10){
#   Erzeugt Eintrittszeiten eines homogenen Poisson-Prozesses
#   mit Intesität lambda auf dem Zeithorizont [0,zeit.horizont]
t0<-rexp(1,lambda)
while (sum(t0)<=zeit.horizont){
    t1<-rexp(1,lambda)
    t0<-c(t0,t1)
}
t<-cumsum(t0)
t<-t[t<=zeit.horizont]
return(t)
}
#   Erzeugt 5 Pfade und stellt diese grafisch dar
set.seed(4712)
t<-Sim.PP(5,10)
y<-0:length(t)
pfad<-stepfun(t,y)
plot(pfad,verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10))
farben<-c("black","red","blue","green","cyan")
for (i in 2:5){
t<-Sim.PP(5,10)
y<-0:length(t)
pfad<-stepfun(t,y)
plot(pfad,verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10),add=TRUE,col.hor=farben[i],col.vert=farben[i])
}
#-------------Teil (a) Alternativlösung-----------#
set.seed(4712)
t<-replicate(5,Sim.PP(5,10))
y<-lapply(t,FUN=function(x)0:length(x))
plot(stepfun(t[[1]],y[[1]]),verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10))
farben<-c("black","red","blue","green","cyan")
for (i in 2:5)plot(stepfun(t[[i]],y[[i]]),verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10),add=TRUE,col.hor=farben[i],col.vert=farben[i])


#-------------Teil (b)----------------------------#
Sim.MPP<-function(para.1=2,para.2=8,zeit.horizont=10){
lambda<-runif(1,para.1,para.2)
t<-Sim.PP(lambda,zeit.horizont)
return(t)
}
#   Erzeugt 5 Pfade und stellt diese grafisch dar
set.seed(4712)
t<-Sim.MPP()
y<-0:length(t)
pfad<-stepfun(t,y)
plot(pfad,verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10))
farben<-c("black","red","blue","green","cyan")
for (i in 2:5){
t<-Sim.MPP()
y<-0:length(t)
pfad<-stepfun(t,y)
plot(pfad,verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10),add=TRUE,col.hor=farben[i],col.vert=farben[i])
}

#-------------------------------------------------#
#             Aufgabe 7.10                        #
#-------------------------------------------------#
#-------------Teil (a)----------------------------#
set.seed(4712)
t<-replicate(10,Sim.PP(5,10))
y<-lapply(t,FUN=function(x)c(0,cumsum(rlnorm(length(x),1,1))))
max.y<- max(as.numeric(lapply(y,FUN=max)))
plot(stepfun(t[[1]],y[[1]]),verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10),ylim=c(0,max.y))
#farben<-c("black","red","blue","green","cyan")
#for (i in 2:10)plot(stepfun(t[[i]],y[[i]]),verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10),add=TRUE,col.hor=farben[i],col.vert=farben[i])
farben<-c("black","red","blue","green","cyan")
for (i in 2:10)plot(stepfun(t[[i]],y[[i]]),verticals=TRUE,lwd=2,do.points=FALSE,main="",ylab="Anzahl Ereignisse",xlab="t",xlim=c(0,10),add=TRUE,col.hor=rainbow(9)[i],col.vert=rainbow(9)[i])

#-------------Teil (b)----------------------------#
#   Simulation Compound-Poisson-Verteilung
#   Eine Realisierung
sim.cp<-function(lambda.poisson,...){
N<-rpois(1,lambda.poisson)
Schaeden<-rlnorm(N,1,1)
return(sum(Schaeden))
}
#   Simuliere 100 Gesamtschäden
N.Sim<-100
set.seed(1234)
S.10.lnorm<-replicate(N.Sim,sim.cp(50))
#   Deskriptive Analysen 
hist(S.10.lnorm)
boxplot(S.10.lnorm)
emp.f<-ecdf(S.10.lnorm)
plot(emp.f)
plot(emp.f,log="x",xlim=c(0.001,max(sim)))
quantile(S.10.lnorm,0.95,type=1)
#-------------Teil (c)----------------------------#
#   Lade Aktuar Paket für Paretoverteilung
library(actuar)
#   Analog (b)
x.0<-exp(1.5)
sim.cp<-function(lambda.poisson,...){
N<-rpois(1,lambda.poisson)
Schaeden<-rpareto(N,2,x.0)
return(sum(Schaeden))
}
N.Sim<-100
set.seed(1234)
S.10.pareto<-replicate(N.Sim,sim.cp(50))
#   Deskriptive Analysen 
hist(S.10.pareto)
boxplot(S.10.pareto)
emp.f<-ecdf(S.10.pareto)
plot(emp.f)
plot(emp.f,log="x",xlim=c(0.001,max(sim)))
quantile(S.10.pareto,0.95,type=1)
qqplot(S.10.lnorm,S.10.pareto)

#-------------------------------------------------#
#             Aufgabe 7.11                        #
#-------------------------------------------------#
#   Lade Aktuar Paket für Paretoverteilung
library(actuar)

#   Definiere Reservefunktion
reserve<-function(t,u.0,c){u.0+c*t}

#--------Hom. PP als Schadenanzahlprozess---------#              
sim.pfad.rr.prozess<-function(u.0=20,c=7.5,lambda=5,x,zeit.horizont=10,anzahl.t.werte=1000){
#   Simuliert einen Risiko-reserveprozess über 
#   Zeithorizont [0,T]
#   Schadenanzahl:  Homogener Poisson-Prozess mit
#   Intensität lambda
#   Eingabe:  u.0:    Anfangskapital
#             c:      Prämieneinnahme pro Zeiteinheit
#             x:      Schadenhöhenverteilung
#   Verwendet Funktion Sim.PP
pp<-Sim.PP(lambda,zeit.horizont)
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

#     Aufruf
#     Mehrere RR-Pfade in einer Grafik
t<-seq(0,10,length=1000)
#     Definiere Schadenhöhenverteilungen
pareto.alpha<-2.39
pareto.x0<-3.03*10^8
x.1<-substitute(rpareto(Anz.Schaeden,2.39,3.03*10^8))
x.2<-substitute(rlnorm(Anz.Schaeden,18.44,1.13))
#     Parameter des Risikoreserveprozesses
u.0<-100*10^9
erw.N1<-30.97
erw.x.1<-pareto.x0/(pareto.alpha-1)
erw.x.2<-exp(18.44+(1.13)^2/2)
theta<-0.5
c.1<-(1+theta)*erw.N1*erw.x.1
c.2<-(1+theta)*erw.N1*erw.x.2
#     Erzeuge jeweils 10 Pfade
set.seed(4712)
y.1<-replicate(10, sim.pfad.rr.prozess(u.0,c.1,erw.N1,x.1,,))
y.2<-replicate(10, sim.pfad.rr.prozess(u.0,c.2,erw.N1,x.2,,))

par(mfrow=c(1,2))
plot(t,y.2[,1],ylim=range(c(y.1,y.2)),main="Lognorm-Schadenhöhen",type="l",ylab="u(t)",lwd=1)
for (i in 2:10) lines(t,y.2[,i],lwd=1)
plot(t,y.1[,1],ylim=range(c(y.1,y.2)),main="Pareto-Schadenhöhen",type="l",ylab="u(t)",lwd=1)
for (i in 2:10) lines(t,y.1[,i],lwd=1)

#--------Inhom. PP als Schadenanzahlprozess-------# 
#     Definiere Intensitätsfunktion
Intensity.1<-function(t){
return(35.32+2.32*2*pi*sin(2*pi*(t-0.2)))
}
#   Simulation der Eintrittszeiten eines inhomogenen PP
Sim.I.PP.Thin<-function(Intensity,lambda.gross=70,zeit.horizont=10){
#   Simuliert einen inhomogenen PP über die Verwerfungsmethode 
#   Zeithorizont [0,zeit.horizont]
#   Ausgabe: Eintrittszeiten eines inhomogenen PP
#   Eingabe:  Intensity:      Intensitätsfunktion
#             lambda.gross:   obere Schranke der Intensität 
t1<-0
t.alt<-0
while (max(t1)<=zeit.horizont){
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


#   Simulation RR-Prozess mit inhomogenem PP-Prozess
sim.pfad.rr.inhom.prozess<-function(u.0=20,c=7.5,Intensity,lambda.gross=70,x,zeit.horizont=10,anzahl.t.werte=1000){
#   Simuliert einen Risiko-reserveprozess über 
#   Zeithorizont [0,T]
#   Schadenanzahl:  Homogener Poisson-Prozess mit
#   Intensität lambda
#   Eingabe:  u.0:    Anfangskapital
#             c:      Prämieneinnahme pro Zeiteinheit
#             x:      Schadenhöhenverteilung
#   Verwendet Funktion Sim.PP
pp<-Sim.I.PP.Thin(Intensity,lambda.gross,zeit.horizont)
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
#     Aufruf analog homogener PP
#     Mehrere RR-Pfade in einer Grafik
t<-seq(0,10,length=1000)
#     Definiere Schadenhöhenverteilungen
pareto.alpha<-2.39
pareto.x0<-3.03*10^8
x.1<-substitute(rpareto(Anz.Schaeden,2.39,3.03*10^8))
x.2<-substitute(rlnorm(Anz.Schaeden,18.44,1.13))
#     Parameter des Risikoreserveprozesses
u.0<-100*10^9
erw.N2<-35.32
erw.x.1<-pareto.x0/(pareto.alpha-1)
erw.x.2<-exp(18.44+(1.13)^2/2)
theta<-0.5
c.1<-(1+theta)*erw.N2*erw.x.1
c.2<-(1+theta)*erw.N2*erw.x.2
#     Erzeuge jeweils 10 Pfade
set.seed(4712)
y.1<-replicate(10, sim.pfad.rr.inhom.prozess(u.0,c.1,Intensity.1,70,x.1,,))
y.2<-replicate(10, sim.pfad.rr.inhom.prozess(u.0,c.2,Intensity.1,70,x.2,,))

par(mfrow=c(1,2))
plot(t,y.2[,1],ylim=range(c(y.1,y.2)),main="Lognorm-Schadenhöhen",type="l",ylab="u(t)",lwd=1)
for (i in 2:10) lines(t,y.2[,i],lwd=1)
plot(t,y.1[,1],ylim=range(c(y.1,y.2)),main="Pareto-Schadenhöhen",type="l",ylab="u(t)",lwd=1)
for (i in 2:10) lines(t,y.1[,i],lwd=1)


#-------------------------------------------------#
#             Aufgabe 7.12                        #
#-------------------------------------------------#

sim.bin.process<-function(n=10,p=0.5){
x<-rbinom(n,1,p)
y<-c(0,cumsum(x))
return(y)
}
n<-10
t<-0:n
z.1<-sim.bin.process(n,0.1)
z.2<-sim.bin.process(n,0.5)
z.3<-sim.bin.process(n,0.7)
plot(0,0,lwd=2,type="l",xlim=c(0,n),ylim=c(0,n),ylab="Schadenanzahl",xlab="Zeit")
grid(NULL,NULL,col="black")
lines(t,z.1,lwd=2,lty=1,col="blue")
points(t,z.1,pch=19,col="blue")
lines(t,z.2,lwd=2,lty=1,col="red")
points(t,z.2,pch=19,col="red")
lines(t,z.3,lwd=2,lty=1)
points(t,z.3,pch=19)

#-------------------------------------------------#
#             Aufgabe 7.13                        #
#-------------------------------------------------#
updown<-function(x,u,d){(u+d)*x-d}

#-------------Teil (a)----------------------------#
Gen.geo.BG<-function(n.Sim=12){
epsilon.1<-rbinom(n.Sim,1,0.5)
epsilon<-sapply(epsilon.1,FUN=updown,u=0.11797,d=0.11297)
G<-c(0,cumsum(epsilon))
Q<-exp(G)
return(Q)
}
set.seed(4711)
G<-Gen.geo.BG(100)
plot(G,type="l",lwd=2,xlab="t",ylab=expression(G[t]))
#-------------Teil (b)----------------------------#
z<-replicate(1000,Gen.geo.BG(100))
#     Werte von K_100
K.100<-z[101,]
#     Schätzer für P(K_100>6)
length(K.100[K.100>6])/length(K.100)
#     Mit empirischer Verteilungsfunktion
emp.vert<-ecdf(K.100)
p<-1-emp.vert(6)

#-------------------------------------------------#
#             Aufgabe 7.14                        #
#-------------------------------------------------#
#     Erzeugt Simulation der Standard-BB auf [0;1]
Gen.BB<-function(n.Sim=1000){
sigma<-sqrt(0.001)
epsilon<-rnorm(n.Sim,0,sigma)
G<-c(0,cumsum(epsilon))
return(G)
}

#     Standard BB   
set.seed(4711)
G<-replicate(5,Gen.BB())
t<-seq(0,1,length.out=1001)
mu<-0.5
sigma<-0.1
#     BB mit Drift und Volatilität
W<-mu*t+sigma*G
#     Geom. BB
X<-exp(W)
#     Grafiken
par(mfcol=c(1,2))
plot(t,W[,1],type="l",lwd=2,xlab="t",ylab=expression(W[t]),lty=1,ylim=range(W))
for (i in 2:5) lines(t,W[,i],lwd=2)
plot(t,X[,1],type="l",lwd=2,xlab="t",ylab=expression(X[t]),lty=1,ylim=range(X))
for (i in 2:5) lines(t,X[,i],lwd=2)


#-------------------------------------------------#
#             Aufgabe 7.15                        #
#-------------------------------------------------#
#     Verwende Funktionen aus 7.11
#     Für N1&x.1-Modell (andere Modelle analog)

#     Definiere Schadenhöhenverteilungen
pareto.alpha<-2.39
pareto.x0<-3.03*10^8
x.1<-substitute(rpareto(Anz.Schaeden,2.39,3.03*10^8))
#     (Neue) Parameter des Risikoreserveprozesses
u.0<-1*10^9
erw.N1<-30.97
erw.x.1<-pareto.x0/(pareto.alpha-1)
theta<-0.1
c.1<-(1+theta)*erw.N1*erw.x.1
#     Erzeuge 2000 Pfade
set.seed(4712)
N.Sim<-2000
y.1<-replicate(N.Sim, sim.pfad.rr.prozess(u.0,c.1,erw.N1,x.1,,))
#     Ruinwahrscheinlichkeiten
min.y1<-apply(y.1,2,FUN=min)
prob.ruin.1<-length(min.y1[min.y1<0])/length(min.y1)
#     Alternativ über empirische Verteilungfunktion
emp.vert<-ecdf(min.y1)
p<-emp.vert(0)
#     Histogramm der minimalen Prozesswerte
hist(min.y1)

#-------------------------------------------------#
#             Aufgabe 7.16                        #
#-------------------------------------------------#
#     Verwende Funktionen aus 7.11
#     Für N1&x.1-Modell (andere Modelle analog)

#     Funktion, die die Quantillinien darstellt
plot.quantile.lines<-function(y,t,quants=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99)){
zeilen<-nrow(y)
spalten<-ncol(y)
values<-array(1,c(zeilen,length(quants)))
for (i in 1:length(quants))
values[,i]<-apply(y,1,FUN=quantile,quants[i])
upper<-max(values)
lower<-min(values)
plot(t,values[,1],ylim=c(lower,upper),main="Quantillinien",type="l",ylab="u(t)",lwd=1)
for (i in 2:length(quants)) lines(t,values[,i],lwd=1)
}

#      Aufruf
#      ... 
N.Sim<-2000
y.1<-replicate(N.Sim, sim.pfad.rr.prozess(u.0,c.1,erw.N1,x.1,,))
plot.quantile.lines(y.1,t)

#-------------------------------------------------#
#             Aufgabe 7.17                        #
#-------------------------------------------------#
schaetz.quantil.v<-function(N,alpha){
x<-rnorm(N)
v<-exp(x)+2*x^2
quantile(x,alpha)
}
#     Aufruf
schaetz.quantil.v(1000,0.9)

#-------------------------------------------------#
#             Aufgabe 7.18                        #
#-------------------------------------------------#
#     Verwende Simulationen aus Aufgabe 7.5
#     Im folgeden Darstellung für sim.pf.gauss (andere analog)
sim.pf<-sim.pf.gauss
gesamt.verluste<-sim.pf[,1]+sim.pf[,2]
#-------------Teil (a)----------------------------#
hist(gesamt.verluste)
boxplot(gesamt.verluste)
qqnorm(gesamt.verluste)
#-------------Teil (b)----------------------------#
q.95<-quantile(gesamt.verluste,.95,type=1,names=FALSE)
q.99<-quantile(gesamt.verluste,.99,type=1,names=FALSE)
#-------------Teil (c)----------------------------#
tce.95<-mean(gesamt.verluste[gesamt.verluste>q.95])
tce.99<-mean(gesamt.verluste[gesamt.verluste>q.99])
#-------------Teil (d)----------------------------#
grosse.verluste<-sim.pf[sim.pf[,1]>1.2 & sim.pf[,2]>1.2,]
p.grosse.verluste<-nrow(grosse.verluste)/nrow(sim.pf)
#-------------Teil (e)----------------------------#
praemie<-mean(gesamt.verluste)+0.1*var(gesamt.verluste)

#-------------------------------------------------#
#             Aufgabe 7.19                        #
#-------------------------------------------------#
#     Amalog zu 7.18 mit sim.ev.gauss etc statt sim.pf.gauss

#-------------------------------------------------#
#             Aufgabe 7.20                        #
#-------------------------------------------------#
#     Wahrer VaR-Wert
q.95<-qnorm(0.95)
#     Nichtparametrischer VaR-Schätzer
VaR.est.NP<-function(alpha=.95,l){
quantile(l,alpha,type=1,names=FALSE)
}

set.seed(1234)
l<-rnorm(10000)
#     Differenzen
q.95-VaR.est.NP(,l[1:10])
q.95-VaR.est.NP(,l[1:100])
q.95-VaR.est.NP(,l[1:1000])
q.95-VaR.est.NP(,l[1:10000])


#-------------------------------------------------#
#             Aufgabe 7.21                        #
#-------------------------------------------------#
library(copula)
#-------------Teil (a)----------------------------#
gauss.cop.bspl<- mvdc(normalCopula(0.7), c("norm", "norm"),
list(list(mean= 0, sd =1), list(mean = 0, sd =2)))
set.seed(4711)
sim.pf<-rmvdc(gauss.cop.bspl,1000)
gesamt.verluste<-sim.pf[,1]+sim.pf[,2]
#-------------Teil (b)----------------------------#
q<-quantile(gesamt.verluste,.95,type=1,names=FALSE)   # 95%-VaR
tce<-mean(gesamt.verluste[gesamt.verluste>q])
ac.1<-mean(sim.pf[sim.pf[,1]+sim.pf[,2]>q,1])
ac.2<-mean(sim.pf[sim.pf[,1]+sim.pf[,2]>q,2])
ac.1.rel<-ac.1/tce
ac.2.rel<-ac.2/tce
data.frame(tce,ac.1,ac.2,ac.1.rel,ac.2.rel)
#     Alternativ als Funktion
tce.alloc<-function(data){
#     returns absolute and relative estimates for allocations
#l1<--.3*data[,1]
#l2<--.7*data[,2]
l1<-data[,1]
l2<-data[,2]
l<-l1+l2
l.mat<-cbind(l1,l2,l)
q<-quantile(l,.95,type=1,names=FALSE)   # 95%-VaR
es<-mean(l[l>=q])
ac.1<-mean(l.mat[l.mat[,3]>q,1])
ac.2<-mean(l.mat[l.mat[,3]>q,2])
ac.1.rel<-ac.1/es
ac.2.rel<-ac.2/es
return(data.frame(es,ac.1,ac.2,ac.1.rel,ac.2.rel)) 
}
#   Aufruf
tce.alloc(sim.pf)

#-------------------------------------------------#
#             Aufgabe 7.22                        #
#-------------------------------------------------#
#-------------Teil (a)----------------------------#
Sim.Gesamtschaden<-function(N.Sim=1000,n.A=3,p.A=0.1,x.A=10000,n.B=3,p.B=0.05,x.B=20000,n.C=3,p.C=0.01,x.C=40000){
X.A<-x.A*rbinom(N.Sim,n.A,p.A)
X.B<-x.B*rbinom(N.Sim,n.B,p.B)
X.C<-x.C*rbinom(N.Sim,n.C,p.C)
S<-X.A+X.B+X.C
return(S)
}
#-------------Teil (b)----------------------------#
S<-Sim.Gesamtschaden()
mean(S)
var(S)
quantile(S,0.95,type=1)
quantile(S,0.99,type=1)
f.emp<-ecdf(S)
x<-knots(f.emp)
y<-diff(c(0,f.emp(x)))
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))
#-------------Teil (d)----------------------------#
mean(Sim.Gesamtschaden(100,))
mean(Sim.Gesamtschaden(1000,))
mean(Sim.Gesamtschaden(10000,))
mean(Sim.Gesamtschaden(100000,))
#-------------Teil (e)----------------------------#
S<-Sim.Gesamtschaden(,8,,,4,,,2,,)
mean(S)
var(S)
quantile(S,0.95,type=1)
quantile(S,0.99,type=1)
f.emp<-ecdf(S)
x<-knots(f.emp)
y<-diff(c(0,f.emp(x)))
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))
mean(Sim.Gesamtschaden(100,8,,,4,,,2,,))
mean(Sim.Gesamtschaden(1000,8,,,4,,,2,,))
mean(Sim.Gesamtschaden(10000,8,,,4,,,2,,))
mean(Sim.Gesamtschaden(100000,8,,,4,,,2,,))



#-------------------------------------------------#
#             Aufgabe 7.23                        #
#-------------------------------------------------#
#-------------Teil (b), (i)-----------------------#
Sim.Anz.Ausfaelle<-function(N.Sim,n.A=2,p.A=0.0005,n.B=8,p.B=0.005,n.C=90,p.C=0.05){
X.A<-rbinom(N.Sim,n.A,p.A)
X.B<-rbinom(N.Sim,n.B,p.B)
X.C<-rbinom(N.Sim,n.C,p.C)
S<-X.A+X.B+X.C
return(S)
}
#-------------Teil (b), (ii)----------------------#
set.seed(1234)
S<-Sim.Anz.Ausfaelle(100)
mean(S)
var(S)
quantile(S,0.95,type=1)
quantile(S,0.99,type=1)
f.emp<-ecdf(S)
x<-knots(f.emp)
y<-diff(c(0,f.emp(x)))
#   Schätzer für P(N=2) und P(N=7)
list(c(y[x==2],y[x==7]))
#   Zähldichte (geschätzt)
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))
#     Als Funktion
P.7<-function(N.Sim=100){
S<-Sim.Anz.Ausfaelle(N.Sim)
return(length(S[S==7])/N.Sim)
}
#     Aufruf
set.seed(1234)
P.7()

#-------------------------------------------------#
#             Aufgabe 7.24                        #
#-------------------------------------------------#
#     Nichtparametrischer VaR-Schätzer
VaR.est.NP<-function(alpha=.95,l){
quantile(l,alpha,type=1,names=FALSE)
}
#     Simuliert den NP-Schätzer für n Daten 
Sim.VaR.est.NP<-function(n=10,alpha=.95){
VaR.est.NP(alpha,rnorm(n))
}
B<-10000
alpha.KI<-0.95
g<-replicate(B,Sim.VaR.est.NP(10,))
#     bzw.
#     g<-replicate(B,Sim.VaR.est.NP(100,))  etc
q<-quantile(g,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
#   boxplot(g$thetastar)
hist(g,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g,xlab="Theoretische Quantile",ylab="Stichprobenquantile")
q



#-------------------------------------------------#
#             Aufgabe 7.25                        #
#-------------------------------------------------#
mu<-0.0764
sigma<-0.2668
N.Sim<-20
N.Boot<-1000
EW.Intervallrendite<-function(mu,sigma){exp(mu+0.5*sigma^2)-1}
Median.Intervallrendite<-function(mu,sigma){exp(mu)-1}
DichteMax.Intervallrendite<-function(mu,sigma){exp(mu-sigma^2)-1}

#-------------Teil (a)----------------------------#
sim.EW<-function(N.Sim=20,mu=0.0764,sigma=0.2668){
x<-rnorm(N.Sim,mean=mu,sd=sigma)
result<-EW.Intervallrendite(mean(x),sd(x))
return(result)
}
sim.Median<-function(N.Sim=20,mu=0.0764,sigma=0.2668){
x<-rnorm(N.Sim,mean=mu,sd=sigma)
result<-Median.Intervallrendite(mean(x),sd(x))
return(result)
}
sim.DichteMax<-function(N.Sim=20,mu=0.0764,sigma=0.2668){
x<-rnorm(N.Sim,mean=mu,sd=sigma)
result<-DichteMax.Intervallrendite(mean(x),sd(x))
return(result)
}
#-------------Teil (b)----------------------------#
set.seed(4711)
data.EW<-replicate(N.Boot,sim.EW())
data.Median<-replicate(N.Boot,sim.Median())
data.DichteMax<-replicate(N.Boot,sim.DichteMax())
par(mfcol=c(1,3))
hist(data.EW)
hist(data.Median)
hist(data.DichteMax)
#     95% Konfidenzintervalle
quantile(data.EW,c(0.025,0.975))
quantile(data.Median,c(0.025,0.975))
quantile(data.DichteMax,c(0.025,0.975))
#     Erwartungswerte etc
summary(data.EW)
summary(data.Median)
summary(data.DichteMax)

#-------------------------------------------------#
#             Aufgabe 7.26                        #
#-------------------------------------------------#
#       Ausführung für Gauss-Copula VaR
#       Andere Copulas und Risikomaße analog
VaR.KI.boot<-function(alpha.VaR=.99,alpha.KI=.95,l,B=30){
#   Berechnet zweiseitiges Boostrap-KI von....
#   B:    Anzahl Boostrapwiederholungen
set.seed(4711)
l<-replicate(B,rowSums(rmvdc(gauss.cop.bspl,5000)),simplify=FALSE)
#     Analog: g<-rmvdc(t.cop.bspl,5000)  etc
g<-sapply(l,FUN=VaR.est.NP,alpha=alpha.VaR)
q<-quantile(g,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
hist(g,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g,xlab="Theoretische Quantile",ylab="Stichprobenquantile")
return(q)
}
#     Aufruf
VaR.KI.boot()
#-------------------------------------------------#
#             Aufgabe 7.27                        #
#-------------------------------------------------#
#     Verwende Funktion tce.alloc aus Aufgabe 7.21
B<-10
set.seed(1234)
l<-replicate(B,rmvdc(gauss.cop.bspl,5000),simplify=FALSE)
g<-sapply(l,FUN=tce.alloc)
#     Histogramme
par(mfcol=c(1,5))
for (i in 1:5)hist(as.numeric(g[i,]))
boxplot(g)
h<-apply(g,2,as.numeric)
boxplot(as.numeric(g[1,]))
boxplot(as.data.frame(cbind(as.numeric(g[2,]),as.numeric(g[3,]))),names=c("ac.1","ac.2")) 
boxplot(as.data.frame(cbind(as.numeric(g[4,]),as.numeric(g[5,]))),names=c("ac.rel.1","ac.rel.2")) 

#-------------------------------------------------#
#             Aufgabe 7.28                        #
#-------------------------------------------------#
#     Verwende Daten aus Beispiel 6.1.6.4


#	     Lade Paket fBasics
library(fBasics)

#     Definiere Funktion, die den nichtparametrischen TVaR berechnet
TCE.est.NP<-function(data,alpha.VaR){ mean(data[data>quantile(data,alpha.VaR)])}
#     Lade Paket bootstrap
library(bootstrap)
TCE.KI.boot<-function(data,alpha.VaR=.99,alpha.KI=.95,R=30){
#   Berechnet zweiseitiges NP Boostrap-KI von
set.seed(4711)
g<-bootstrap(data,R,TCE.est.NP,alpha=alpha.VaR)   
q<-quantile(g$thetastar,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
#   boxplot(g$thetastar)
hist(g$thetastar,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g$thetastar)
qqline(g$thetastar)
return(q)
}
#   Aufruf
TCE.KI.boot(data,,,1000)

#-------------------------------------------------#
#             Aufgabe 7.29                        #
#-------------------------------------------------#
#     Verwende Funktion P.7 aus Aufgabe 7.23 
N.Boot<-10000
N.Sim<-1000
alpha.KI<-.95
set.seed(1234)
g<-replicate(N.Boot,P.7(N.Sim))
q<-quantile(g,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
hist(g,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g,xlab="Theoretische Quantile",ylab="Stichprobenquantile")
q

#-------------------------------------------------#
#             Aufgabe 7.30                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 6.6

#     Analog zu Aufgabe 7.28
#     Definiere Funktion, die die Prämie berechnet
Premium.est.NP<-function(data){mean(data)+0.1*var(data)}
#     Lade Paket bootstrap
library(bootstrap)
Premium.KI.boot<-function(data,alpha.KI=.95,B=1000){
#   Berechnet zweiseitiges NP Boostrap-KI von
set.seed(4711)
g<-bootstrap(data,B,Premium.est.NP)   
q<-quantile(g$thetastar,probs=c((1-alpha.KI)/2,1-(1-alpha.KI)/2))
par(mfcol=c(1,2))
#   boxplot(g$thetastar)
hist(g$thetastar,freq=FALSE,main="Histogramm",ylab="Dichte",col="grey",
xlab=expression(hat(theta)))
abline(v=q[1],lwd=2,lty=2)
abline(v=q[2],lwd=2,lty=2)
qqnorm(g$thetastar)
qqline(g$thetastar)
return(q)
}
#   Aufruf
set.seed(1234)
Premium.KI.boot(data,,)





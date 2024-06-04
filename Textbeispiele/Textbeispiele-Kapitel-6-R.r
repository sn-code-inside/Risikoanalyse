#-------------------------------------------------#
#             Beispiel in 6.1.1.1                 #
#-------------------------------------------------#
#   Erzeugung der Daten
set.seed(4711)
daten<-rlnorm(10)
daten<-round(daten,2)
daten<-sort(daten)

est.mu=mean(daten)
est.var=(length(daten)-1)/length(daten)*var(daten)
c(est.mu,est.var)

#-------------------------------------------------#
#             Beispiele in 6.1.1.2                #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1
#     Median
quantile(daten,0.5,type=1,names=FALSE)
#     Median-Befehl in R
median(daten)
#     Weitere Quantile
quantile(daten,0.8,type=1,names=FALSE)
quantile(daten,0.75,type=1,names=FALSE)

#-------------------------------------------------#
#             Beispiel in 6.1.2.2                 #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1
par(mfrow=c(1,2))
hist(daten,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Daten",col="grey")
rug(daten)
boxplot(daten,main="Box-Plot",horizontal=TRUE)

#-------------------------------------------------#
#             Beispiel in 6.1.3.2                 #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1
#     Verwende Paket MASS für die Anpassung
library(MASS)
#     Anpassung Normalverteilung
fit.norm<-fitdistr(daten, "normal")
fit.norm
#     Anpassung Gammaverteilung
fit.gamma<-fitdistr(daten, "gamma")
fit.gamma
#     Anpassung log-Normalverteilung
fit.lognormal<-fitdistr(daten, "log-normal")
fit.lognormal

#     Erzeuge Abb. 6.2
curve(dnorm(x,fit.norm$estimate[[1]],fit.norm$estimate[[2]]),from=-2,to=7,ylab="Dichte",lty=1,lwd=2,ylim=c(0,0.6))
curve(dgamma(x,fit.gamma$estimate[[1]],fit.gamma$estimate[[2]]),add=TRUE,from=0,to=7,lty=2,lwd=2,)
curve(dlnorm(x,fit.lognormal$estimate[[1]],fit.lognormal$estimate[[2]]),add=TRUE,from=0,to=7,lty=3,lwd=2,)

legend(2,0.6, 
c("Normalverteilung","Gamma-Verteilung","log-Normalverteilung"),
 lty=c(1,2,3),lwd=c(2,2,2))

#-------------------------------------------------#
#             Beispiel in 6.1.4.1                 #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1

#     Simultaner Verteilungsplot mit angepasster Normalverteilung
#     Erzeugt empirische Verteilungsfunktion
emp<-ecdf(daten)
#     Anpassung nach Momentenmethode
est.mu=mean(daten)
est.sd=sqrt(est.var)
#     Erzeugt Abb. 6.3
par(mar=c(5,5,4,2)+0.1)
plot(emp,pch=20,lwd=2,xlim=c(-2,7),main="Empirische Verteilungsfunktion und angepasste Normalverteilung",ylab=expression(hat(F)(x)))
rug(daten)
curve(pnorm(x,est.mu,est.sd),add=TRUE,lwd=2,from=-2,to=7,lty=2)

#     Erzeugt Abb. 6.4
hist(daten,freq=FALSE,main="Histogramm und angepasste Normalverteilung",ylab="Dichte",col="grey",xlab="x")
rug(daten)
curve(dnorm(x,est.mu,est.sd),add=TRUE,lwd=2,from=0,to=7)


#-------------------------------------------------#
#             Beispiel in 6.1.4.2                 #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1
qqnorm(daten,main="Q-Q-Plot",ylab="Stichprobenquantile",xlab="Theoretische Quantile")


#-------------------------------------------------#
#             Beispiele in 6.1.5                  #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1
#     Lade Paket nortest
library("nortest")
#     K-S-Test
ks.test(daten,"pnorm",est.mu,est.sd)
#     Lilliefors-Test
lillie.test(daten)
#     Anderson-Darling-Test
ad.test(daten)
#     Chi^2-Anpassungstest
pearson.test(daten)
pearson.test(daten,adjust=TRUE) # adjustiert

#-------------------------------------------------#
#             Beispiel in 6.1.6.1 und 6.1.6.2     #
#-------------------------------------------------#
#     Verwende daten aus 6.1.1.1
#     Lade Paket fBasics für den Jarque-Bera-Test
library(fBasics)
#     Jarque-Bera-Test
jarqueberaTest(daten)

#     Shapiro-Wilk-Test
shapiro.test(daten)


#-------------------------------------------------#
#             Beispiel 6.1.6.4                    #
#-------------------------------------------------#
#     Verwende nyse Daten aus Paket fBasics oder von Homepage
#	    Lade Paket fBasics
library(fBasics)	#	Lade Paket fBasics
data(nyse)
x<-nyse[3001:4000,2]
z1<-log(1/x[1]*x)
data<--diff(z1)

#     Funktion, die zu gegebenen Daten die Abbildung 6.7 erzeugt
plot.data<-function(x){
#	Stelle verschiedene Plots in einem großen Fenster dar
par(mfrow=c(2,2))
plot(x,main="Verlauf",type="l",xlab="Zeit",ylab="Verluste")
f<-ecdf(x)
plot(f,lwd=2,xlab="Verluste",main="Empirische Verteilungsfunktion",ylab=expression(hat(F)(x)))
#lines(sort(x),pnorm(sort(x),mean(x),sd(x)),col="red",lwd=2)
hist(x,freq=FALSE,main="Histogramm",ylab="Dichte",xlab="Verluste",col="grey")
est.mu=mean(x)
est.sd=sqrt((length(x)-1)/length(x))*sd(x)
curve(dnorm(x,est.mu,est.sd),add=TRUE,lwd=2)
qqnorm(x,xlab="Theoretische Quantile",ylab="Stichprobenquantile",main="Q-Q-Plot")
}
#     Aufruf
plot.data(data)

#	    Anpassungstests auf Normalität der log-returns
#     Lade Packet nortest
library("nortest")
#     K-S-Test
ks.test(data,"pnorm",mean(data),sd(data))
#     Lilliefors-Test
lillie.test(data)
#     Anderson-Darling-Test
ad.test(data)
#     Chi^2-Anpassungstest
pearson.test(data)
pearson.test(data,adjust=TRUE) # adjustiert
#     Shapiro-Wilk-Test
shapiro.test(data)
#     Shapiro-Francia-Test
sf.test(data)
#     Jarque-Bera-Test
library("fBasics")
jarqueberaTest(data)

#-------------------------------------------------#
#             Beispiel in 6.2.1.1                 #
#-------------------------------------------------#
#     Verwende data aus  Beispiel 6.1.6.4
#	    Parametrischer VaR-Schätzer
VaR.est.P<-function(alpha=.99,l){
sd(l)*qnorm(alpha)
}
#     Aufrufe
VaR.est.P(.95,data)
VaR.est.P(.99,data)

#-------------------------------------------------#
#             Beispiel in 6.2.1.2                 #
#-------------------------------------------------#
#     Verwende data aus  Beispiel 6.1.6.4
#	    Parametrischer VaR-Schätzer
VaR.est.P<-function(alpha=.99,l){
sd(l)*qnorm(alpha)
}
#     KI für VaR
VaR.KI.P<-function(alpha.VaR=.99,alpha.KI=.95,l){
#   Berechnet zweiseitiges KI für VaR (s. S. 316) 
#   auf Basis des parametrischen VaR-Schätzers
n<-length(l)
s<-sd(l)
breite.KI<-qnorm(alpha.VaR)*qnorm(alpha.KI+(1-alpha.KI)/2)*sd(l)*sqrt(2)/sqrt(n)
lower<-VaR.est.P(alpha.VaR,l)-breite.KI/2
upper<-VaR.est.P(alpha.VaR,l)+breite.KI/2
data.frame(lower, upper)
}

#     Aufrufe
VaR.KI.P(.95,.95,data)
VaR.KI.P(.99,.95,data)


#-------------------------------------------------#
#             Beispiel 6.3.3                      #
#-------------------------------------------------#
#      Pakete laden                
library(copula)            
library(QRMlib)
library(evir)

#      Daten aufbereiten
data(siemens)
data(bmw)
z<-cbind(bmw,siemens)
#     Gemeinsame Nullwerte entfernt (s. Carmona, Zivot)
z.neu<-z[z[,1]!=0 & z[,2]!=0,]
emp.bmw<-ecdf(z.neu[,1])
emp.siemens<-ecdf(z.neu[,2])
vert<-function(z){
c(emp.bmw(z[1]),emp.siemens(z[2]))
}
#-------------CML-Schätzung-----------------------#
#   Renormierung der empirischen Verteilungsfunktion 
#   mit n+1 statt n um Konvergenzprobleme bei Maximierung der 
#   Likelihoodfunktion zu vermeiden
z.renorm<-t(apply(z.neu,1,FUN=vert))
daten<-z.renorm*length(z.renorm)/(length(z.renorm)+1)

#     Erzeuge Abb. 6.9
par(mfcol=c(1,2))
plot(z,ylab="Siemens",xlab="BMW")
plot(z.renorm,ylab="Siemens",xlab="BMW")

#   Anpassung  Gausscopula (QRMlib)
fit.gauss <- fit.gausscopula(daten)
fit.gauss
#   Anpassung  tcopula (QRMlib)
fit.t <- fit.tcopula(daten)
fit.t
rho<-fit.t$P[1,2]
nu<- fit.t$nu
#   Anpassung  Clayton (QRMlib)
fit.clayton <- fit.Archcopula2d(daten,"clayton")
fit.clayton
theta<-fit.clayton$theta
#   Anpassung  Gumbel (QRMlib)
fit.gumbel <- fit.Archcopula2d(daten,"gumbel")
fit.gumbel
theta<-fit.gumbel$theta

#-------------Rangkorrelationskoeffizienten-------#
rho.S<-Spearman(z.renorm)
rho.K<-Kendall(z.renorm,noforce=FALSE)
rho.gauss<-rho.S
rho.t<-sin(pi*rho.K/2)
rho.Gumbel<-1/(1-rho.K)
rho.Clayton<-2*rho.K/(1-rho.K)
list(rho.gauss,rho.t,rho.Gumbel,rho.Clayton)

#-------------------------------------------------#
#             Beispiel in 6.4.2                   #
#-------------------------------------------------#
#     Verwende nyse Daten aus Paket fBasics oder von Homepage
#	    Lade Paket fBasics
library(fBasics)	
data(nyse)
x<-nyse[3001:4000,2]
z1<-log(1/x[1]*x)
data<--diff(z1)

#-----Nützliche Funktionen------------------------#
#	    Parametrischer VaR-Schätzer
VaR.est.P<-function(alpha=.99,l){
sd(l)*qnorm(alpha)
}
#	    Nichtparametrischer VaR-Schätzer
VaR.est.NP<-function(alpha=.99,l){
quantile(l,alpha,type=1)
}

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

#     Aufrufe für 504-Tages Backtesting 
#     Analog für T=252
btd<-Select.Data(data,T=504) 
T<-504
True.Losses<-data[(length(data)-(T-1)):length(data)]

#     Parametric VaR
Estimated.P.VaR<-apply(btd,1,FUN=VaR.est.P,alpha=.99)   
#     Nonparametric VaR
Estimated.NP.VaR<-apply(btd,1,FUN=VaR.est.NP,alpha=.99)
#     Darstellung in gemeinsamer Grafik (Erzeugt Abb. 6.11)
plot(Estimated.P.VaR,type="l",lwd=1,lty=2,ylim=c(min(min(Estimated.P.VaR),min(Estimated.NP.VaR),min(True.Losses)),
max(max(Estimated.P.VaR),max(Estimated.NP.VaR),max(True.Losses))))
lines(Estimated.NP.VaR,lwd=1,lty=1)
lines(True.Losses,type="h")
#     Ausreisser
Differ.P<-True.Losses-Estimated.P.VaR
Differ.NP<-True.Losses-Estimated.NP.VaR
index.P<-subset(1:T,Differ.P>0)
index.NP<-subset(1:T,Differ.NP>0)
Ausreisser.P<- Estimated.P.VaR[index.P]
Ausreisser.NP<- Estimated.NP.VaR[index.NP]
points(index.P,Ausreisser.P,pch=16)
points(index.NP,Ausreisser.NP,pch=16)
#     Ausgabe der Ausreisser
index.P
index.NP
#-------------------------------------------------#
#             Beispiele in 6.4.3                  #
#-------------------------------------------------#
#      Verwende Output von 6.4.2
#----- Approx. Binomialtest (von Hand)------------#
aus<-length(index.P)
z<-(aus-T*0.01)/sqrt(T*0.99*0.01)
z
p.value<-2*(1-pnorm(z))
p.value

#       Mit prop.test
prop.test(aus,T,0.01,"two.sided")
#       Konsistenz mit Approx. Binomialtest oben (ohne Korrektur)
prop.test(aus,T,0.01,"two.sided",correct=FALSE)

#-----Kupiec-Test--------------------------------#
LR<-function(p.0,p.dach,n.Ausr,T){
L.0<-p.0^n.Ausr*(1-p.0)^(T-n.Ausr)
L.A<-p.dach^n.Ausr*(1-p.dach)^(T-n.Ausr)
-2*(log(L.0)-log(L.A))
}
#   Backtest über 504 Tage
T<-504
p.0<-0.01
#   Parametrisch
p.dach<-aus/T
LR(p.0,p.dach,aus,T)  
#    Vergleich mit Chi-Quadrat
qchisq(0.95,1)


#-------------------------------------------------#
#             Beispiele in 6.4.3                  #
#-------------------------------------------------#
#   Von Hand: Christoffersen
T<-504
n.11<-0
n.10<-12
n.00<-480
n.01<-12
p.dach<-(n.11+n.10)/T
p.10<-n.10/(n.10+n.00)
p.11<-n.11/(n.11+n.01)
d<--2*log((1-p.dach)^(n.00+n.01)*p.dach^(n.10+n.11))+2*log((1-p.10)^(n.00)*p.10^(n.10)*(1-p.11)^(n.01)*p.11^(n.11))
d
#    Vergleich mit Chi-Quadrat
qchisq(0.95,1)
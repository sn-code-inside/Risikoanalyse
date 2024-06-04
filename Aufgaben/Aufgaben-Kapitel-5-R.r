#-------------------------------------------------#
#             Aufgabe 5.1                         #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
#     Daten laden
#     Pfad des Verzeichnisses, z.B.
pfad.name<-"C:\\Dokumente und Einstellungen\\...\\Risikoanalyse\\Datensätze\\"
#      Hänge Dateinamen an Pfadnamen an, z.B.
file.name.in<-paste(pfad.name,sep="","Aufgabe 5.1.xls")
#      Einlesen als Excel-Datei
data.roh<-read.table(file.name.in,header=TRUE,sep="\t",dec=",")


#     Alternativ: Daten erzeugen 
set.seed(4711)
flaeche<-round(runif(9,100,100000),0)
resi<-rnorm(9,0,0.15)
a<-0.4
b<-0.2
verlust<-round(exp(a*log(flaeche)+b + resi),0)
data.roh<-as.data.frame(cbind(flaeche,verlust))
#-------------Teil (a)----------------------------#
plot(data.roh)
cor(data.roh)
#-------------Teil (b)----------------------------#
log.flaeche<-log(flaeche)
log.verlust<-log(verlust)
data.log<-as.data.frame(cbind(log.flaeche,log.verlust))
plot(data.log)
cor(data.log)


#-------------------------------------------------#
#             Aufgabe 5.3                         #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
BMW<-c(0.0030,0.0224,-0.0059,0.0206,-0.0058,-0.0118,0.0064,0.0039,-0.0015,-0.0238)
Siemens<-c(-0.0051,0.0072,-0.0055,0.0017,0.0160,-0.0013,0.0219,0.0016,-0.0156,-0.0222)
#     Alternativ: Daten von der Website benutzen und einlesen     
#-------------Teil (a)----------------------------#
data<-as.data.frame(cbind(BMW,Siemens))
plot(data)
data.neu<-rbind(data,c(0.0089, 0.0730))
plot(data.neu)
cor(data.neu)


#-------------------------------------------------#
#             Aufgabe 5.5                         #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
#   Aufrufen von Anscombes Datensatz
library(stats)
d<-anscombe
#     Alternativ: Daten von der Website benutzen und einlesen 
#-------------Teil (a)----------------------------#
#   Pearson
cor.1.pearson<-cor(d$x1,d$y1)
cor.2.pearson<-cor(d$x2,d$y2)
cor.3.pearson<-cor(d$x3,d$y3)
cor.4.pearson<-cor(d$x4,d$y4)
#-------------Teil (b)----------------------------#
par(mfcol=c(2,2))
plot(d$x1,d$y1)
plot(d$x2,d$y2)
plot(d$x3,d$y3)
plot(d$x4,d$y4)

#-------------------------------------------------#
#             Aufgabe 5.8                         #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 5.3
#     Mit Formeln von S. 258
b.dach<-sd(BMW)/sd(Siemens)*cor(BMW,Siemens)
a.dach<-mean(BMW)-b.dach*mean(Siemens)
a.2<--a.dach/b.dach
b.2<-1/b.dach
plot(data,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
abline(a.2,b.2,lwd=2,lty=2)

#     Alternative: Mit lm-Befehl
mod.2<-lm(BMW~Siemens,data=as.data.frame(data))
mod.2
summary(mod.2)
par(mar=c(4,4,2,2)+0.1)
plot(data,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
abline(mod.2,lwd=2)
plot(data,xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
coef.2<-coefficients(mod.2)
a.2<--coef.2[[1]]/coef.2[[2]]
b.2<-1/coef.2[[2]]
abline(a.2,b.2,lwd=2,lty=2)


#-------------------------------------------------#
#             Aufgabe 5.9                         #
#-------------------------------------------------#
#     Analog 5.8
b.dach<-sd(data.neu$Siemens)/sd(data.neu$BMW)*cor(data.neu$BMW,data.neu$Siemens)
a.dach<-mean(data.neu$Siemens)-b.dach*mean(data.neu$BMW)
plot(data.neu,xlim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
abline(a.dach,b.dach,lwd=2,lty=2)
#     Regressionsgerade für "alte" Daten
abline(-0.0005,0.474,lwd=2,lty=1)

#     Alternative: Mit lm-Befehl
mod<-lm(Siemens~BMW,data=as.data.frame(data.neu))
plot(data.neu,xlim=c(-0.05,0.05),xlab="BMW",ylab="Siemens")
abline(mod,lwd=2)
#     Regressionsgerade für "alte" Daten
abline(-0.0005,0.474,lwd=2,lty=1)

#-------------------------------------------------#
#             Aufgabe 5.10                        #
#-------------------------------------------------#
#-------------Daten laden bzw. erzeugen-----------#
#     Aufrufen von Anscombes Datensatz
library(stats)
d<-anscombe
#     Alternativ: Daten von der Website benutzen und einlesen 
#     Regressionen durchführen
par(mfcol=c(2,2))
plot(d$x1,d$y1)
mod.1<-lm(y1~x1,data=d)
abline(mod.1)
plot(d$x2,d$y2)
mod.2<-lm(y2~x2,data=d)
abline(mod.2)
plot(d$x3,d$y3)
mod.3<-lm(y3~x3,data=d)
abline(mod.3)
plot(d$x4,d$y4)
mod.4<-lm(y4~x4,data=d)
abline(mod.4)


#-------------------------------------------------#
#             Aufgabe 5.11                        #
#-------------------------------------------------#
#     Verwende Daten aus Aufgabe 5.1
#-------------Teile (a,b)-------------------------#
mod.roh<-lm(verlust~flaeche,data=data.roh)
summary(mod.roh)
par(mfcol=c(1,2))
plot(data.roh,xlim=c(10000,100000))
abline(mod.roh)
mod.log<-lm(log.verlust~log.flaeche,data=data.log)
summary(mod.log)
plot(data.log)
abline(mod.log)
#-------------Teil (c)----------------------------#
f<-function(x,a,b){exp(a)*x^b}
plot(data.roh)
abline(mod.roh,lwd=2,lty=1)
x<-seq(10000,100000,1000)
y<-sapply(x,FUN=f,a=mod.log$coefficients[[1]],b=mod.log$coefficients[[2]])
lines(x,y,lwd=2,lty=2)


#-------------------------------------------------#
#             Aufgabe 5.12                        #
#-------------------------------------------------#
#     Paket laden
library(copula)
#-------------Teil (a)----------------------------#
gauss.cop<- mvdc(normalCopula(0.2), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
par(mfcol=c(1,2))
contour(gauss.cop,dmvdc,xlim=c(0.6,1.4),ylim=c(0.6,1.4))
persp(gauss.cop, dmvdc,xlim=c(0.6,1.4),ylim=c(0.6,1.4),shade=1,expand=.5,xlab="x",ylab="y",zlab="Copula-Dichte",
ticktype = "detailed" )
#-------------Teil (b)----------------------------#
pmvdc(gauss.cop,c(0.8,0.8))
#-------------Teil (c)----------------------------#
#  t-Copula 2
t.cop<- mvdc(tCopula(0.8,df=2), c("lnorm", "lnorm"),
list(list(meanlog = -0.0277, sdlog =0.1221), list(meanlog = -0.1099, sdlog =0.0942)))
par(mfcol=c(1,2))
contour(t.cop,dmvdc,xlim=c(0.6,1.4),ylim=c(0.6,1.4))
persp(t.cop, dmvdc,xlim=c(0.6,1.4),ylim=c(0.6,1.4),shade=1,expand=.5,xlab="x",ylab="y",zlab="Copula-Dichte",
ticktype = "detailed" )
pmvdc(t.cop,c(0.8,0.8))

#-------------------------------------------------#
#             Aufgabe 5.25                        #
#-------------------------------------------------#
#-------------Teil (a)----------------------------#
TDC.t<-function(nu,rho){
x<--sqrt((nu+1)*(1-rho)/(1+rho))
2*dt(x,nu+1)
}
rho<-seq(-0.9999,1,0.001)
lambda<-sapply(rho,FUN=TDC.t,nu=4)
plot(rho,lambda,type="l")

#-------------Teil (b)----------------------------#
TDC.t(4,0)


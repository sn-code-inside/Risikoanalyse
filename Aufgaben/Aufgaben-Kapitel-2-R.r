#-------------------------------------------------#
#             Aufgabe 2.8                         #
#-------------------------------------------------#


#     Erwartungswert logarithmische Verteilung (S. 55)
EW.log.Vert<-function(p){p/((p-1)*log(1-p))}
#     Varianz logarithmische Verteilung (S. 55)
Var.log.Vert<-function(p){p*(-log(1-p)-p)/((p-1)^2*(log(1-p))^2)}
#     Erwartungswert Klumpen PP
EW.KlumpenPP<-function(lambda,t,EW){lambda*t*EW}
#     Varianz Klumpen PP
Var.KlumpenPP<-function(lambda,t,EW,VAR){lambda*t*VAR+lambda*t*EW^2}
#

#     Dies Funktion findet zu vorgegebenem p.log
#     und p.NB das gesuchte r.NB, so dass die Varianzen 
#     der Prozesse gleich sind
r.loesung<-function(p.log=0.2,p.NB=0.1){
a<-1/(1-p.NB)
x<-Var.log.Vert(p.log)+ (EW.log.Vert(p.log))^2
b<--p.NB^2*x/((1-p.NB)^2)
return(-0.5*a+0.5*sqrt(a^2-4*b))
}


#     Probe
p.log<-0.7374
p.NB<-0.5
r.NB<-r.loesung(p.log,p.NB)
r.NB
EW.NB<-r.NB*(1-p.NB)/p.NB
Var.NB<-r.NB*(1-p.NB)/(p.NB^2)
Var.Klumpen.NB<-Var.KlumpenPP(1,1,EW.NB,Var.NB)
Var.Klumpen.log<-Var.KlumpenPP(1,1,EW.log.Vert(p.log),Var.log.Vert(p.log))
c(Var.Klumpen.NB,Var.Klumpen.log,Var.Klumpen.NB-Var.Klumpen.log)

#      Stelle r.NB in Abhängigkeit von p.NB bei 
#      vorgegebenem p.log dar
p.log<-0.2
t<-seq(0.0001,.9999,0.01)
r<-sapply(t,FUN=r.loesung,p.log=p.log)
plot(t,r,type="l",xlab="p.NB",ylab="r.NB")



#-------------------------------------------------#
#             Aufgabe 2.10                        #
#-------------------------------------------------#
x<-0:30
n<-182    # Entspricht 6 Monaten
p<-1/300
y<-dbinom(x,n,p)
y
plot(x,y,type="h",lwd=3,xlab="n",ylab=expression(p[n]))
EW<-n*p
EW
SD<-sqrt(n*p*(1-p))
SD

#-------------------------------------------------#
#             Aufgabe 2.12                        #
#-------------------------------------------------#
#-------------Teil (a)----------------------------#
n<-100
w<-0.01
k<-0:10
#   Überschreitungswahrscheinlichkeiten 
y.bin<-pbinom(k,n,w,lower.tail = FALSE)
plot(k,y.bin,type="h",lwd=3,xlab="k",ylab=expression(P(N>k)))
#-------------Teil (c)----------------------------#
lambda<-1
k<-0:10
#   Überschreitungswahrscheinlichkeiten 
y.pois<-ppois(k,lambda,lower.tail = FALSE)
plot(k,y.pois,type="h",lwd=3,xlab="k",ylab=expression(P(N>k)))
#     Differenz
y.diff<-y.bin-y.pois
plot(k,y.diff,type="h",lwd=3,xlab="k")



#-------------------------------------------------#
#             Aufgabe 2.14                        #
#-------------------------------------------------#
updown<-function(x,u,d){(u+d)*x-d}

#-------------Teil (a)----------------------------#
Gen.geo.BG<-function(n.Sim=12,u=0.11797,d=0.11297){
epsilon.1<-rbinom(n.Sim,1,0.5)
epsilon<-sapply(epsilon.1,FUN=updown,u,d)
G<-c(0,cumsum(epsilon))
Q<-exp(G)
return(Q)
}
set.seed(4711)
G<-100*Gen.geo.BG(12)
plot(G,type="l",lwd=2,xlab="t",ylab=expression(G[t]))
points(G,pch=19)


#-------------------------------------------------#
#             Aufgabe 2.15                        #
#-------------------------------------------------#
mu<-0.03
sigma<-0.4

u<-mu/12+sigma/sqrt(12)
v<--mu/12+sigma/sqrt(12)
set.seed(4711)
G<-100*Gen.geo.BG(12,u,v)
plot(G,type="l",lwd=2,xlab="t",ylab=expression(G[t]))
points(G,pch=19)


#     Der folgende Code erzeugt Abbildung 2.16
u<-0.11797
d<-0.11297
x.0<-d/(u+d)    #   Damit updown(x.0)=0 ist
x<-expand.grid(x.0,c(-1,1),c(-1,1),c(-1,1),c(-1,1),c(-1,1),c(-1,1),c(-1,1),c(-1,1),
c(-1,1),c(-1,1),c(-1,1),c(-1,1))
n.col<-ncol(x)
#y<-apply(x,1,FUN=cumsum)
y<-apply(x,1,FUN=updown,u,d)

#z1<-apply(y,2,FUN=updown,u,d)
z1<-apply(y,2,FUN=cumsum)
z<-apply(z1,2,FUN=exp)
n.row<-nrow(z)
i<-0:(n.col-1)
plot(i,z[,1],type="l",ylim=range(z),ylab="Kurs",xlab="Zeitpunkt")
points(i,z[,1],pch=19)
for (j in 1:ncol(z)){
lines(i,z[,j])
points(i,z[,j],pch=19)
}


#-------------------------------------------------#
#             Aufgabe 2.16                        #
#-------------------------------------------------#

#  Vasicek 
#  Modellparameter 
a<-0.058          #   Entspricht c (bis auf Skalierung) 
sigma<-0.0085     #   Entspricht s (bis auf Skalierung)
m<-0.05           
r.0<-0.03         #   Startpunkt

T<-50
N<-1000
d.t<- T/N
sigma.N<-sigma/sqrt(N/T)
t<-seq(0,T,length.out=N)
r<-t
set.seed(4711)
epsilon<-rnorm(length(t))
r[1]<-r.0
for (i in 2:length(r)) r[i]<-r[i-1]+a*(m- r[i-1])*d.t + sigma.N*epsilon[i]
plot(t,r,type="l",main="Vasicek-Modell")


#  CIR 
T<-50
N<-1000
d.t<- T/N
sigma.N<-sigma/sqrt(N/T)
t<-seq(0,T,length.out=N)
r<-t
set.seed(4711)
epsilon<-rnorm(length(t))
r[1]<-r.0
for (i in 2:length(r)) r[i]<-r[i-1]+a*(m- r[i-1])*d.t + sqrt(r[i-1])*sigma.N*epsilon[i]
plot(t,r,type="l",main="CIR-Modell")

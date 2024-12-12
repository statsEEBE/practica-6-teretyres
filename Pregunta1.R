## EJERCICIO 1

#Distr. Normal
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
var<- 25
n<- length(x)

##Apartado a: IC del 90% para mu
#MANUAL
xbar<- mean(x); xbar
sigma<- sqrt(var); sigma
Z0.05<- qnorm(0.95); Z0.05
IC<- c(xbar-Z0.05*sigma/sqrt(n), xbar+Z0.05*sigma/sqrt(n)); IC
#LIBRERIA
#instalar libreria
#install.packages('BSDA')
library(BSDA)
z.test(x, sigma.x=sigma, conf.level=0.9) #Solo si nos dan el vector x

#Apartado AC1: H0: mu0= 500
#H1: mu0!= 500
mu0<- 500
#MANUAL
Zc<- qnorm(0.95); Zc
Zobs<- (xbar-mu0)/(sigma/sqrt(n)); Zobs
pvalor<- 2*pnorm(-Zobs); pvalor
#LIBRERIA
z.test(x, sigma.x=sigma, conf.level=0.9, mu=mu0) #para 2 colas

##Apartado AC2: H0: mu<=mu0
#H1: mu> mu0 #Cola superior
z.test(x, sigma.x=sigma, conf.level=0.9, mu=mu0, alternative = 'greater') #para cola superior
#alternative = 'less' : para cola inferior
pvalor2<- pnorm(-Zobs); pvalor2

##Apartado b: n para un IC de 2 gramos
n2<- (qnorm(0.975)*sigma)^2; n2 #Miramos el entero (este caso n2=97)
#Comprovación
c(xbar-qnorm(0.975)*sigma/sqrt(n2), xbar+qnorm(0.975)*sigma/sqrt(n2))
504.75-502.75

##Apartado c: varianza desconocida, IC del 99%
s<- sd(x)
#MANUAL
c(xbar-qt(0.995,n-1)*s/sqrt(n), xbar+qt(0.995,n-1)*s/sqrt(n))
#LIBRERIA
t.test(x, conf.level = 0.99)

##Apartado AC3: H0: mu0= 500
#H1: mu0!= 500
t.test(x, conf.level = 0.99, mu=mu0)

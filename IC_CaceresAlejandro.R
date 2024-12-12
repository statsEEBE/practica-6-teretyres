
#X-> N(mu, sigma^2), mu desconido

## TEMA 7

mu_bar<- 1/n*sum(muestra) #Promedio
#Error= media-promedio
m<- qnorm(1-alfa)*(sigma/sqrt(n)) #Margen de error
#IC(1-alfa)%, Confianza= 1-alfa, alfa= significación estaística (0.025)
#IC: P(-m<=(media-promedio)<=m)=0.95
lim1<- media-m
lim2<- media+m
#Varianza desconocida
s2<-1/(n-1)*sum(x-xbar)^2 #Varianza muestral
IC<- c(media-qt(0.975,n-1)*s/sqrt(n),media+qt(0.975,n-1)*s/sqrt(n))
## TEMA 8

#hipotesis nula (H0): mu0=500 (mu deseada/ esperada) 
#hipotesis alternativa (H1): mu0 != 500

##CRITERIOS IC (son equivalentes)
#CRITERIO 1 IC: Si mu0 lo atrapa el IC, se acepta la H0, si no se rechaza la H0
#CRITERIO 2 IC: Si Zobs esta dentro de Zc, se acepta H0, si no se rechaza H0
Z<- (media-mu0)/(sigma/sqrt(n)) #Error estándar, variable estándar
#Si el error es muy grande, aceptamos H1
#Tolerancia del error: 95%
Zc<- qnorm(0.975) #Zeta crítica (-Zc, Zc): límites de la tolerancia
Zobs<- Z #Zeta observada
#CRITERIO 3 IC: Si el pvalor> alfa, acepto H0, si no se rechaza H0
pvalor<- 2*pnorm(-Zobs)

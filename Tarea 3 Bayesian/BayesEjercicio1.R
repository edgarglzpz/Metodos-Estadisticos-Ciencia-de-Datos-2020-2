#Densidad exponencial, priori de lambda

lambda = 1/720
avrg2 =  1/lambda
std.dv2 = sqrt(1/lambda^2)

rango2 = seq(0, avrg2 + 2*std.dv2, 0.01)
y2 = dexp(rango2, lambda)

# Densidad gamma, posteriori de lambda
X = c(751, 760, 744, 750, 753, 763, 706)
X_sum = sum(X)
alpha = 1 + X_sum
n = length(X)
beta = 1/720 + n
avrg = alpha/beta
std.dv = sqrt((alpha*(beta^2)))
x = 5227

rango = seq(0, avrg + 2*std.dv, 0.001)
y = dgamma(rango, shape = alpha, rate = beta)

#Gráfica de la exponencial
plot(rango2, y2, type='l', ylim = c(0, max(y)+ 0.0001), ann = FALSE, 
     axes = FALSE, col = 'red') 
par(new=TRUE)
#Gráfica de la Gamma
plot(rango, y, type = 'l', ylim = c(0, max(y)+0.0001), 
     main = 'Distribuciones de Lambda', xlab = 'Lambda', ylab = 'Densidades',
     col= 'blue')
legend(x=1300, y = 0.025, legend=c('Inicial', 'Final'), col=c("red", "blue"), 
       fill = c("red", "blue"), title= 'Distribución')

#Estimación de la región

k=qgamma(0.05,shape = alpha, rate = beta)

#Proponemos valores de a para encontrar los de b y su distancia.
a = seq(0,k,length.out = 100)
b=rep(NA, length(a))
#Encontrando el valor de b
for(i in 1:length(a)){
  b[i]= qgamma(0.95+pgamma(a[i], shape=alpha, rate = beta), shape = alpha , rate = beta)
}

dist = b-a
#Encontramos el de menor distancia
inta=a[1]
intb=b[1]
d=dist[1]
for(i in 2:length(a)){
  if(dist[i]< d){
    inta = a[i]
    intb = b[i]
    d = dist[i]
  }
  else{
    inta = inta
    intb = intb
    d = d
  }
}

#La region de maxima densidad de probabilidad 0.9 es
c(inta,intb)

pgamma(intb, shape = alpha, rate = beta) - pgamma(inta, shape = alpha, rate = beta)
intb-inta == min(dist)


#Prueba de hipótesis
#P(H1)
P_H1 = 1 - pgamma(760, shape = alpha, rate = beta)
#P(H2)
P_H2 = pgamma(760, shape = alpha, rate = beta)
library(PtProcess)

X = c(5.02, 18.75, 3.82, 36.95, 10.79, 2.38, 36.82, 22.66, 5.74, 27.99, 8.21, 32.99, 1.97, 1.34)
X_n = max(X)
n = length(X)

#Densidad pareto, distribución a priori de theta

beta = 1
alpha = log(2)/log(21)

rango = seq(beta, 100)
y = dpareto(rango, alpha, beta)

#Densidad Pareto, distribución a posteriori de theta

alpha2 = alpha + n
beta2 = max(X_n, beta)

rango2 = seq(beta2, 100)
y2 = dpareto(rango2, alpha2, beta2)


#Gráfica de la Pareto a priori
plot(rango, y, type = 'l', ylim = c(0, max(y)+ 0.0001), 
     ann = FALSE, axes = FALSE, col = 'green')
par(new=TRUE)
plot(rango2, y2, type = 'l', ylim = c(0, max(y)+0.0001),
     main = 'Distribuciones de Theta', xlab = 'Theta', ylab = 'Densidades',
     col= 'orange')
legend(x= 80, y = 0.20, legend=c('Inicial', 'Final'), 
       fill = c('green', 'orange'), title= 'Distribución')

a=alpha + n
M= X_n

F <- function(x,a,M) {
  if(x<=M){(a/(a+1))*(x/M)}
  else{(a/(a+1))*(M/M)+(1-((M/K)^(a)))/(a+1)}
}

F_inv<-function(p,M,a){
  if(p<=0){0}
  else if(p>0 & p<=.9343301){((a+1)/a)*p*M}
  #else if(p<1 & p>.9343301){(((a+1)/a)*.9343301*M)+(M/(((p*(a+1)-1))^(1/a)))}
  else if(p<1 & p>.9343301){(((a+1)/a)*(.9343301)*M)+(M/((1+(p*(a+1)))^(1/a)))}
  #else if(p<1 & p>.9343301){(((a+1)/a)*(.9343301)*M)+(1/(a+1))+(M/(((p*(a+1)))^(1/a)))}
  else{1}
}

#Intervalo, incompleto
r=F(1.977353,alpha+n, X_n)
r
k=F_inv(.05, X_n, alpha+n) 
k

#Prueba de hipotesis
#Hipotesis nula
P_H1 = F(27, alpha+n, X_n)
#Hipotesis alternativa
P_H2 = 1 - P_H1 
P_H1
P_H2
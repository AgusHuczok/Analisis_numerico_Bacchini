#1 Inputs----
cupon=0.05
amortizaciones=matrix(c(1,2,3,4,25,25,25,25),ncol=2,dimnames=list(NULL,c("t","Amort")))

m=2 #Numero de pagos de intereses por anio

# Marcha progresiva----
n=max(amortizaciones[,1])*m
#Creo matriz de marcha
marcha=matrix(rep(NA,(n+1)*5),ncol=5)
colnames(marcha)=c("t","Saldo","Amort","Int","CF")
#Columna t
marcha[,"t"]=seq(from=0,to=n/m,by=1/m)
#Columnas amort y saldo
k=1
marcha[1,"Saldo"]=sum(amortizaciones[,"Amort"])
for (i in 1:(n+1)){
  if(marcha[i,"t"]==amortizaciones[k,"t"]){
    marcha[i,"Amort"]=amortizaciones[k,"Amort"]
    if(i>1){marcha[i,"Saldo"]=marcha[i-1,"Saldo"]-amortizaciones[k,"Amort"]}
    k=k+1}
  else{
    marcha[i,"Amort"]=0
    if(i>1){marcha[i,"Saldo"]=marcha[i-1,"Saldo"]}
  }
  }
#Columna int
marcha[1,"Int"]=0
for(i in 2:(n+1)){
  marcha[i,"Int"]=marcha[i-1,"Saldo"]*cupon/m
}
marcha[,"CF"]=marcha[,"Amort"]+marcha[,"Int"]

#Funcion de precio----
precio=function(tasa,t,CF){
  n=length(CF)
  p=0
  for(i in 1:n){
    p=p+CF[i]*(1+tasa)^-t[i]
  }
return(p)
}

precio(0.05,marcha[,"t"],marcha[,"CF"])

#para saber si esta ok paso la TEA a TNA
tasa1=(1+0.05/2)^2-1
precio(tasa1,marcha[,"t"],marcha[,"CF"])

#grafico de las tasas
tasas=seq(0,1,0.001)
p=precio(tasas,marcha[,"t"],marcha[,"CF"])
plot(tasas,p,type="b")

#Resolucion de la ecuacion----
polinomio=function(r){return(-80+precio(r,marcha[,"t"],marcha[,"CF"]))}
plot(tasas,f(tasas),type="l",col="red")
abline(h=0)
raiz_biseccion(0,0.2,10^-6,100)

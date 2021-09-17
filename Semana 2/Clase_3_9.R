library(ggplot2)
source("C:/Users/Dell3000/Documents/Busqueda_raiz.R")

f <- function(x) {
  f = cos(x)- x^(0.5)
  f
}

x<-seq(0,10,0.1)#Genero vector para graficar f(x)
fx<-f(x)
df<-data.frame(x,fx) #Creo dataframe

ggfx=ggplot(data=df) #cargo los datos
ggfx=ggfx+aes(x=x,y=fx)#Cargo variables
ggfx=ggfx+geom_line(linetype=1,colour="darkblue") #Agrego linea
ggfx=ggfx+geom_hline(yintercept=0,linetype=1)+geom_vline(xintercept = 0,linetype=1)#Creo x=0 e y=0
ggfx=ggfx+scale_x_continuous(name="x",breaks=seq(1,10,1)) #cambio escala eje X
ggfx=ggfx+scale_y_continuous(name="y=f(x)",breaks=seq(-4.5,1,0.5)) #Cambio escala eje Y

#Ahora se que la raiz esta entre 0.5 y 2. Grafico puntos en ese area
ggfx=ggfx+geom_vline(xintercept=c(0.5,1),linetype=2,colour="red")
ggfx=ggfx+ggtitle("Funcion con raiz entre 0.5 y 1") #Agrego titulo



#g(x)----
#x=cos(x)^2 = g(x)
g=function(x){
  cos(x)^2
} #En realidad se llama g pero le pongo polinomio paa que funcione iteracion_punto_fijo
x2=seq(0,1,0.1)
gx=g(x2)
df2=data.frame(x=x2,gx)
gggx=ggplot(data=df2)+aes(x=x,y=gx)+geom_line(linetype=1,colour="darkblue")+ggtitle("Funcion con punto fijo")+geom_abline(intercept=0,slope=1,linetype=2)
#cambio las escalas
gggx=gggx+scale_x_continuous(name="x",limits=c(0,1),breaks=seq(0,1,0.1))+scale_y_continuous(name="g(x)",limits=c(0,1),breaks=seq(0,1,0.1))
                                                                                  
#Ahora se que el punto fijo esta entre 0.6 y 0.7
#calculo punto fijo y corroboro
iteracion_punto_fijo(g,0.6,10^-6,300)
g(iteracion_punto_fijo(g,0.6,10^-6,300))
f(iteracion_punto_fijo(g,0.6,10^-6,300)) #corroboro que sea raiz

#verifico graficamente
gggx=gggx+geom_vline(xintercept=iteracion_punto_fijo(g,0.6,10^-6,300),linetype=1,size=1,colour="darkgreen")+geom_point(aes(x=iteracion_punto_fijo(g,0.6,10^-6,300),y=g(iteracion_punto_fijo(g,0.6,10^-6,300))),pch=16,col="red")
gggx                                                                                            


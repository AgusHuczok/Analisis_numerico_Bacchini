x<-c(1,2,3,4,5,2,1,7,8,9,2,1,4,5,7,5,3,4)

barplot(table(x)) #Diagrama de barras

hist(x) #Histograma

x<-runif(100)
y<-runif(100)
plot(x,y)#Scatterplots

#QQplots
x<-rnorm(1000) #valores aleatorios de una normal
y<-rnorm(1000)
qqplot(x,y,main="x e y con la misma distribucion normal")

a<-rnorm(1000,mean=3,sd=2)
qqplot(x,a,main="x N(0,1), a N(3,2)")

b<-rt(1000,df=2)#t de student
qqplot(x,b,main="x N(0,1), b t(2)")

c<-exp(rnorm(x))
qqplot(x,c,main="X N(0,1), c la exponencial de N(0,1)")


table(x)
barplot(table(x))


#Biseccion---------------
raiz_biseccion<-function(func,a,b,tol,N){
  i<-1
  FA<-func(a)
  while(i<=N){
    p=a+(b-a)/2
    FP=func(p)
    if(FP==0 | (b-a)/2<tol){
      return(p)
    }
    i=i+1
    if(FA*FP>0){
      a=p
      FA=FP
    }
    else
    {b=p}
  }
  return(paste('El metodo fracaso luego de N',N,'intentos'))
}

#Punto fijo-------------------
iteracion_punto_fijo<-function(func,p0,tol,N){
  i<-1
  while(i<=N){
    p=func(p0)
    if(abs(p-p0) < tol){
      return(p)
    }
    i<-i+1
    p0=p
  }
  paste('El metodo fallo despues de ',N,'iteraciones')
}
#Newton-----------
f=function(x){
  f= sin(x)-exp(-x)
}
df=function(x){
  df= cos(x)+exp(x)
}


metodo_newton<-function(func,dfunc,p0,tol,N){
  i=1
  while (i<=N){
    p=p0-func(p0)/dfunc(p0)
    if(abs(p-p0)<tol){
      return(p)
    }
    i=i+1
    p0=p
  }
  paste('El metodo fallo despues de ', N,'iteraciones')
  
}
#Secante--------
#Necesito primero aproximar p1

f<-function(x){
  f= x^3 - 2*x^2-5
}
metodo_secante<-function(func,p0,p1,tol,N){
  i=2
  q0=func(p0)
  q1=func(p1)
  while(i<=N){
    p=p1-q1*(p1-p0)/(q1-q0)
    if(abs(p-p1)<tol){
      return(p)
    }
    i=i+1
    p0=p1
    q0=q1
    p1=p
    q1=func(p)
  }
  paste('El metodo fallo despues de', N, 'iteraciones')
}
#Posicion falsa------

#Necesito primero aproximar p1

posicion_falsa<-function(func,p0,p1,tol,N){
  i=2
  q0=func(p0)
  q1=func(p1)
  while(i<=N){
    p=p1-q1*(p1-p0)/(q1-q0)
    if(abs(p-p1)<tol){
      return(p)
    }
    i=i+1
    q=func(p)
    if(q*q1<0){
      p0=p
      q1=q
    }
    p1=p
    q1=q
  }
}

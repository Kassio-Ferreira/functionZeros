# funções investigadas:

f0 = function(x)  2^(-x) - 2*sin(x);
f0linha = function(x) -2^(-x)*log(2) - 2*cos(x)

f1 = function(x) sin(x) + log(x);
f1linha = function(x) cos(x) + (1/x)

f2 = function(x) x*log(x)-1
f2linha = function(x) log(x)+1

# método da bisseção

bissecao <- function(f,a,b,l){
  
  c = b-a;
  x0 = (a+b)/2;
  
  while(c>l){
    
    if(f(x0) == 0)
      break;
    
    if(f(a)*f(x0) < 0){
      b = x0;
    }
    
    if(f(a)*f(x0) > 0){
      a = x0;
    }
    
    c = b-a;
    x0 = (a+b)/2
  }
  
  return(x0);
  
}

bissecao(f0,0,1,0.05)
bissecao(f=f1,a=0.5,b=0.6,l=10^(-6))

# metodo das cordas:

cordas <- function(f,a,b,l){
  
  xi = (a*f(b) - b*f(a))/(f(b) - f(a));
  fxi = f(xi);
  
  repeat{
    
    b = xi
    xi = crit = (a*f(b) - b*f(a))/(f(b) - f(a));
    fxi = f(xi)
    
    if(abs(crit - b) < l)
      break;
  }
  
  return(xi);
}

cordas(f=f1,a=0.5,b=0.6,l = 10^(-6))
cordas(f=f0,a=0,b=1,l=0.05)

# Método de Newton-Raphson

NR <- function(f,flinha,a,b,l){
  xi = (a+b)/2
  
  repeat{
    
    crit = xi
    xi = xi - (f(xi)/flinha(xi));
    
    if(abs(xi - crit) < l)
      break;
  }
  
  return(xi);
}

NR(f0,f0linha,a=0.1,b=1,l=0.005)
NR(f1,f1linha,a=0.5,b=0.6,l=0.005)
NR(f2,f2linha,a=1.75,b=1.8,l=0.005)

# Algoritmo de eliminação de Gauss

gaussElim <- function(A,B){
  
  n = nrow(A)
  
  for(k in 1:(n-1)){
    for(i in (k+1):n){
      m = A[i,k]/A[k,k]
      A[i,k] = 0;
    
      for(j in (k+1):n){
        A[i,j] = A[i,j] - m*A[k,j]
      }
    
      B[i] = B[i] - m*B[k]
    }
  }

  x = NULL;
  x[n] = B[n]/A[n,n]

  for(k in 1:(n-1))
    x[k] = B[k]

  for(k in (n-1):1){
    for(i in (k+1):n){
      x[k] = x[k] - A[k,i]*x[i]
    }
    x[k] = x[k]/A[k,k]
  }
  
  return(x)
}


A = matrix(c(5,2,1,3,6,-2,2,-4,10), byrow=TRUE, nrow=3)
B = matrix(c(8,7,8),nrow=3)
gaussElim(A,B)  

C = matrix(c(5,6,12,2,-3,0,4,1,-1), byrow=TRUE, ncol=3)
D = matrix(c(-1,-1,6), nrow=3)
gaussElim(C,D)

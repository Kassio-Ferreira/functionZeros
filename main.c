#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float bissecao(float a, float b, float l, float (*f0)(float));
float f(float x);
float flinha(float x);
float g(float x);
float glinha(float x);
float z(float x);
float zlinha(float x);
float cordas(float a, float b, float l, float (*f0)(float));
float NR(float a, float b, float l, float (*f0)(float),float (*f0linha)(float));

int main()
{
    float resposta;
    float zero = bissecao(0,1,0.00002,f);
    float zero1 = NR(0.5,0.6,0.005,g,glinha);

    printf("%f\n", zero1);
    return 0;
}

float g(float x){
    return sin(x)+log(x);
}

float glinha(float x){
    return cos(x) + (1/x);
}

float f(float x){
    float out = pow(2,-x) - 2*sin(x);
    return out;
}

float flinha(float x){
    return pow(-2,-x)*log(2)-2*cos(x);
}

float z(float x){
    return x*log(x)-1;
}

float zlinha(float x){
    return log(x)+1;
}

float bissecao(float a, float b, float l, float (*f0)(float)){

    float c = b-a;
    float x0 = (a+b)/2;

    while(c > l){

        if(f0(x0) == 0)
            break;

        if(f0(a) * f0(x0) < 0){
            b = x0;
        }

        if(f0(a) * f0(x0) > 0){
            a = x0;

        }

        c = b-a;
        x0 = (a+b)/2;
    }

    return x0;

}

float cordas(float a, float b, float l,float (*f0)(float)){

    float xi = (a*f0(b) - b*f0(a))/(f0(b) - f0(a));
    float fxi = f0(xi);
    float crit;

    do{
        b = xi;
        xi = crit = (a*f0(b) - b*f0(a))/(f0(b) - f0(a));
        fxi = f0(xi);
    }while(abs(crit - b) > l);

    return xi;
}


float NR(float a, float b, float l, float (*f0)(float),
        float (*f0linha)(float)){

    float xi = (a+b)/2;
    float crit;

    do{
        crit = xi;
        xi = xi - (f0(xi)/f0linha(xi));
    }while(abs(xi-crit)>l);

    return xi;
}


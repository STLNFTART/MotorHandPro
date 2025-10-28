#pragma once
#include <Arduino.h>
#include <cmath>

// ---------- Numeric knobs ----------
namespace RTQ {
  constexpr double TOL = 1e-12;
  constexpr double TOL_FP = 1e-12;
  constexpr int MAX_IT = 1000000;
  constexpr double D = 149.9992314000;
  constexpr double I3 = 6.4939394023;
  constexpr double MU = 0.169050000000;
  constexpr double EPS_TARGET = 0.000005124000;

  inline double clamp(double x,double lo,double hi){return x<lo?lo:(x>hi?hi:x);}

  inline double planckTail(double Xc,double epsTerm=1e-20){
    double s=0;
    for(int n=1;n<=1000000;++n){
      double rn=n,z=rn*Xc,e=exp(-z);
      double term=e*(6+6*z+3*z*z+rn*rn*rn*Xc*Xc*Xc)/(rn*rn*rn*rn);
      s+=term;
      if(term<epsTerm)break;
    }
    return s;
  }

  inline double solveCutoffXc(double tol=TOL){
    const double target=EPS_TARGET;
    double a=0,b=30;
    while(planckTail(b)/I3>target){b*=2;if(b>1e6)break;}
    for(int k=0;k<200;++k){
      double m=(a+b)/2,d=planckTail(m)/I3;
      if(fabs(d-target)<tol)return m;
      (d>target)?a=m:b=m;
    }
    return (a+b)/2;
  }

  inline double planckCutValue(double Xc){
    return 150.0*(1.0-planckTail(Xc)/I3);
  }

  struct KernelParams{double mu;double c;};
  inline KernelParams kernelParamsFor(double mu){
    return {mu,(150.0-D)*exp(mu*D)};
  }

  inline double kernelIterate(double mu,double x0,double tol=TOL_FP,int maxIt=MAX_IT){
    auto kp=kernelParamsFor(mu);
    double x=x0;
    for(int it=0;it<maxIt;++it){
      double xn=150.0-kp.c*exp(-kp.mu*x);
      if(fabs(xn-x)<tol)return xn;
      x=xn;
    }
    return x;
  }

  inline double kernelLipschitzAt(double mu,double x){
    auto kp=kernelParamsFor(mu);
    return kp.c*mu*exp(-mu*x);
  }

  struct Results{
    double D,I3,S,Xc,deltaXc,Ncut,mu,c,FprimeD,xFixed;
  };

  inline Results computeAll(){
    Results r{};
    r.D=D;r.I3=I3;r.S=D/I3;
    r.Xc=solveCutoffXc();
    r.deltaXc=planckTail(r.Xc)/I3;
    r.Ncut=planckCutValue(r.Xc);
    r.mu=MU;
    auto kp=kernelParamsFor(r.mu);
    r.c=kp.c;
    r.FprimeD=kp.c*r.mu*exp(-r.mu*D);
    r.xFixed=kernelIterate(r.mu,150.0,TOL_FP);
    return r;
  }

  inline void print(const Results&r){
    Serial.println(F("=== RUNTIME QUANT ==="));
    Serial.printf("D           = %.10f\n",r.D);
    Serial.printf("I3          = %.10f\n",r.I3);
    Serial.printf("S=D/I3      = %.12f\n",r.S);
    Serial.printf("Xc          = %.12f\n",r.Xc);
    Serial.printf("delta(Xc)   = %.12f\n",r.deltaXc);
    Serial.printf("N_cut(Xc)   = %.10f\n",r.Ncut);
    Serial.printf("mu          = %.12f\n",r.mu);
    Serial.printf("c           = %.12f\n",r.c);
    Serial.printf("F'(D)       = %.12f\n",r.FprimeD);
    Serial.printf("x* (fixed)  = %.10f\n",r.xFixed);
    Serial.println(F("====================="));
  }
}

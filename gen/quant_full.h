#pragma once
#include <Arduino.h>
#include <cmath>

namespace QUANT {
static constexpr double PLANCK_SCALE     = 23.098341716530;   // D/I3
static constexpr double PLANCK_D         = 149.9992314000;    // Planck depth
static constexpr double PLANCK_I3        = 6.4939394023;      // π^4 / 15

static constexpr double CUTOFF_XC        = 19.358674138784;   // Solved Xc
static constexpr double CUTOFF_DELTA     = 0.000005124001;    // δ(Xc)
static constexpr double EPSILON_DELTA    = 0.000005124000;    // target δ
static constexpr double N_CUT_CONST      = 149.9992313999;    // N_cut(Xc)
static constexpr double ABS_DIFF_N_D     = 1.387e-10;         // |N-D|

static constexpr double KERNEL_MU        = 0.169050000000;    // μ
static constexpr double MU_CROSS         = 79116356.058643326163; // c = (150-D) e^{μD}
static constexpr double F_PRIME_D_CONST  = 0.000129931830;    // F'(D)
static constexpr double KERNEL_FIXED_PT  = 149.9992314000;    // fixed point
static constexpr double DONTE_CONSTANT   = 149.9992314000;    // alias

static constexpr double TOL=1e-12, TOL_FP=1e-12;
static constexpr int    MAX_IT=1000000;
static constexpr double EPS_TERM=1e-20;

inline double clamp(double x,double lo,double hi){ return x<lo?lo:(x>hi?hi:x); }

inline double planckTail(double X,double eps=EPS_TERM){
  if (X<=0) return INFINITY;
  double s=0;
  for(int n=1;n<=1000000;++n){
    double rn=n, z=rn*X, e=exp(-z);
    double term = e*(6+6*z+3*z*z+rn*rn*rn*X*X*X)/(rn*rn*rn*rn);
    s+=term;
    if(term<eps) break;
  }
  return s;
}

inline double solveCutoffXc(double tol=TOL){
  const double target=EPSILON_DELTA;
  double a=0,b=30;
  while (planckTail(b)/PLANCK_I3 > target){ b*=2; if(b>1e6) break; }
  for(int k=0;k<200;++k){
    double m=(a+b)/2, d=planckTail(m)/PLANCK_I3;
    if (fabs(d-target)<tol) return m;
    (d>target)? a=m : b=m;
  }
  return (a+b)/2;
}

inline double planckCutValue(double Xc){
  return 150.0*(1.0 - planckTail(Xc)/PLANCK_I3);
}

struct KernelParams{ double mu,c; };
inline KernelParams kernelParamsFor(double mu){
  return { mu, (150.0-PLANCK_D)*exp(mu*PLANCK_D) };
}

inline double kernelIterate(double mu,double x0,double tol=TOL_FP,int maxIt=MAX_IT){
  auto kp=kernelParamsFor(mu);
  double x=x0;
  for(int it=0; it<maxIt; ++it){
    double xn = 150.0 - kp.c*exp(-kp.mu*x);
    if (fabs(xn-x) < tol) return xn;
    x = xn;
  }
  return x;
}

inline double kernelSlopeAt(double mu,double x){
  auto kp=kernelParamsFor(mu);
  return kp.c*mu*exp(-mu*x);
}

struct Results{
  double D,I3,mu,S,Xc,deltaXc,Ncut,c,FprimeD,xFixed;
};

inline Results computeAll(double mu=KERNEL_MU){
  Results r{};
  r.D=PLANCK_D; r.I3=PLANCK_I3; r.mu=mu; r.S=r.D/r.I3;
  r.Xc=solveCutoffXc(); r.deltaXc=planckTail(r.Xc)/r.I3;
  r.Ncut=planckCutValue(r.Xc);
  auto kp=kernelParamsFor(r.mu); r.c=kp.c;
  r.FprimeD = r.c * r.mu * exp(-r.mu * r.D);
  r.xFixed  = kernelIterate(r.mu,150.0,TOL_FP);
  return r;
}

inline void print(const Results& r){
  Serial.println(F("=== QUANT FULL ==="));
  Serial.printf("D           = %.10f\n", r.D);
  Serial.printf("I3          = %.10f\n", r.I3);
  Serial.printf("S=D/I3      = %.12f\n", r.S);
  Serial.printf("Xc          = %.12f\n", r.Xc);
  Serial.printf("delta(Xc)   = %.12f\n", r.deltaXc);
  Serial.printf("N_cut(Xc)   = %.10f\n", r.Ncut);
  Serial.printf("mu          = %.12f\n", r.mu);
  Serial.printf("c           = %.12f\n", r.c);
  Serial.printf("F'(D)       = %.12f\n", r.FprimeD);
  Serial.printf("x* (fixed)  = %.10f\n", r.xFixed);
  Serial.println(F("=================="));
}

inline uint8_t throttleFromFixed(double xFixed){
  const double t = clamp((xFixed/150.0)*255.0, 0.0, 255.0);
  return (uint8_t)(t+0.5);
}
} // namespace QUANT

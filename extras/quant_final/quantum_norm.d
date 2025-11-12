import std.format : format;
import std.string : format;
import std.stdio, std.math, std.getopt;

enum real Dval = 149.9992314000L;
enum real I3   = (PI*PI)*(PI*PI)/15.0L;

real planckScaleS() @safe nothrow @nogc { return Dval / I3; }

real planckTail(real Xc, real epsTerm=1e-20L) @safe {
    real s=0;
    foreach(n; 1 .. 1_000_000){
        real rn=n, z=rn*Xc;
        real term = exp(-z)*(6+6*z+3*z*z+rn*rn*rn*Xc*Xc*Xc)/(rn*rn*rn*rn);
        s += term;
        if(term < epsTerm) break;
    }
    return s;
}

real solveCutoffXc(real tol=1e-12L) @safe {
    const real target = (Dval/150.0L)/150.0L;
    real a=0, b=30;
    while (planckTail(b)/I3 > target) { b*=2; if(b>1e6) break; }
    foreach(_; 0 .. 200){
        real m=(a+b)/2, d=planckTail(m)/I3;
        if (fabs(d-target) < tol) return m;
        if (d > target) a = m; else b = m;
    }
    return (a+b)/2;
}

struct KP { real mu; real c; }
KP kernelParamsFor(real mu) @safe nothrow { return KP(mu, (150.0L-Dval)*exp(mu*Dval)); }

real kernelIterate(real mu, real x0, real tol=1e-12L, size_t maxIt=1_000_000) @safe {
    auto k = kernelParamsFor(mu);
    real x=x0;
    foreach(_; 0 .. maxIt){
        real xn = 150.0L - k.c*exp(-k.mu*x);
        if (fabs(xn - x) < tol) return xn;
        x = xn;
    }
    return x;
}

real kernelLip(real mu, real x) @safe nothrow {
    auto k = kernelParamsFor(mu);
    return k.c * k.mu * exp(-k.mu*x);
}

void main(string[] args){
    string mode="planck"; real mu=0.16905L, tol=1e-12L, x0=150.0L;
    getopt(args, "mode",&mode, "mu",&mu, "tol",&tol, "x0",&x0);

    if (mode=="planck"){
        writeln("Mode: planck");
        writeln("D           = ", format("%.10f", cast(double)Dval));
        writeln("I3=π^4/15   = ", format("%.10f", cast(double)I3));
        auto S = Dval/I3;
        writeln("Scale S     = D/I3 = ", format("%.12f", cast(double)S));
        writeln("Check S*I3  = ", format("%.10f", cast(double)(S*I3)));
    } else if (mode=="cutoff"){
        writeln("Mode: cutoff");
        auto Xc = solveCutoffXc(tol);
        auto target = (Dval/150.0L)/150.0L;
        auto d = planckTail(Xc)/I3;
        writeln("Target δ    = EPS/150 = ", format("%.12f", cast(double)target));
        writeln("Solved Xc   = ", format("%.12f", cast(double)Xc));
        writeln("δ(Xc)       = ", format("%.12f", cast(double)d));
        auto Ncut = 150.0L*(1 - d);
        writeln("N_cut(Xc)   = ", format("%.10f", cast(double)Ncut));
        writeln("|N-D|       = ", format("%.3e", cast(double)fabs(Ncut - Dval)));
    } else if (mode=="kernel"){
        writeln("Mode: kernel");
        auto k = kernelParamsFor(mu);
        writeln("μ           = ", format("%.12f", cast(double)mu));
        writeln("c           = (150-D) e^{μD} = ", format("%.12f", cast(double)k.c));
        auto lip = kernelLip(mu, Dval);
        writeln("F'(D)       = c μ e^{-μ D}   = ", format("%.12f", cast(double)lip));
        auto fp = kernelIterate(mu, x0, tol);
        writeln("Fixed point ≈ ", format("%.10f", cast(double)fp));
        writeln("|x*-D|      = ", format("%.3e", cast(double)fabs(fp - Dval)));
    } else {
        writeln("unknown mode");
    }
}

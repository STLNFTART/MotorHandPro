import std.stdio, std.math, std.conv, std.process;

struct Params { double MU, ALPHA, LAMBDA, KGAIN, KE; }
struct State  { double psi, gamma, Ec; }

State step(State s, double t, double dt, const Params p){
    double psidot = -(p.MU + p.LAMBDA)*s.psi + cos(t) + p.KGAIN*s.gamma;
    double gdot   =  p.ALPHA*tanh(s.psi) - p.LAMBDA*s.gamma;
    double Pd = s.gamma * 0.4 * s.psi;
    double Pr = 0.2 * sin(t);
    s.psi   += dt * psidot;
    s.gamma += dt * gdot;
    s.Ec    += dt * (-Pd + Pr - p.KE * s.Ec);
    return s;
}

void run(double MU, double KE){
    enum D0 = 149.9992314;
    double I3 = pow(PI,4) / 15.0;
    double S  = D0 / I3;
    double ALPHA = 0.55, LAMBDA = 0.115, KGAIN = 1.47;

    double c  = (150 - D0) * exp(MU * D0);
    double Fp = c * MU * exp(-MU * D0);

    writeln("# MU=", MU, " KE=", KE);
    writeln("# Core: D0=", D0, " I3=", I3, " S=", S, " F'(D0)=", Fp);
    writeln("# t,psi,gamma,Ec");

    Params p = Params(MU, ALPHA, LAMBDA, KGAIN, KE);
    State s = State(1, 0, 0);
    for(double t = 0; t <= 5.0; t += 0.01){
        s = step(s, t, 0.01, p);
        writeln(t, ",", s.psi, ",", s.gamma, ",", s.Ec);
    }
}

void main(){
    double MU = (environment.get("MU","0.16905")).to!double;
    double KE = (environment.get("KE","0")).to!double;
    run(MU, KE);
}

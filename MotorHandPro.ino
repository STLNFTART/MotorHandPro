#include "quant_full.h"
void setup(){ Serial.begin(115200); delay(1500); auto r=QUANT::computeAll(); QUANT::print(r); }
void loop(){ delay(1000); }

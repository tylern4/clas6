#include <event.h>

class electroEvent : public event {
 private:
  particle _beamElectron;
  particle _scatteredElectron;

 public:
  electroEvent();
  ~electroEvent();

};

class lowq2Tagger {
 private:
  double _lowPhi;
  double _hiPhi;

 public:
  lowq2Tagger() { _lowPhi = -M_PI; _hiPhi = M_PI;}
  lowq2Tagger(double low,double hi) {_lowPhi = low; _hiPhi = hi;}
  void setPhi(double low,double hi) {_lowPhi = low; _hiPhi = hi;}
  double getPhi() {return( randm(_lowPhi,_hiPhi));}
};


class clasDetector {
   private:
  double _lowPhi;
  double _hiPhi;

 public:
  clasDetector() { _lowPhi = -M_PI; _hiPhi = M_PI;}
  clasDetector(double low,double hi) {_lowPhi = low; _hiPhi = hi;}
  void setBaryonPhi(double low,double hi) {_lowPhi = low; _hiPhi = hi;}
  double getBaryonPhi() {return( randm(_lowPhi,_hiPhi));}
};


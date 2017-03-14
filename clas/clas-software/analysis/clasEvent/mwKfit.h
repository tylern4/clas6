#ifndef _MWKFIT_H
#define _MWKFIT_H

#include <vector>
#include <list>

//use global variales for holding map entries
//the correction coefficients are sector dependent
//for each sector there are 6 numbers

// clasKineFit definition
class clasEvent;
class clasParticle;
class clasKineFit {

private:
  clasEvent *_ev;
  double _prob;
  int _ndf;
  std::list<clasParticle> _cpIn;
  std::list<clasParticle> _cpOut;
  std::vector<double> _pulls;
  double _beamp;

  

  int _status;
  double _chisq;


 public:

 clasKineFit() { this->_status = 0 ;this->_chisq = 0.0; this->_prob = 0.0;}

 clasKineFit& operator=(const clasKineFit& fit);


 clasKineFit& addParticleIn(const clasParticle& cp);
 clasKineFit& addParticleOut(const clasParticle& cp);

 int status() const {return(this->_status);}
 int status(int s) { return(this->_status = s);}


 clasParticle cpIn(int i) const; 
 clasParticle cpOut(int i) const;

 double chisq(double c) {return(this->_chisq = c);}
 double chisq() const {return(this->_chisq);}

 double chisq2prob(const double chisq, const int ndf);

 double beamEnergy() const {return(this->_beamp);}
 double beamEnergy(double f) {return(this->_beamp = f);}

 double prob(double c) {return(this->_prob = c);}
 double prob() const {return(this->_prob);}

 int ndf(int c) {return(this->_ndf = c);}
 int ndf() const {return(this->_ndf);}

 std::list<clasParticle> cpIn() const  {return(this->_cpIn);}
 std::list<clasParticle> cpOut()  const {return(this->_cpOut);}

 std::vector<double>pulls() const {return(this->_pulls);}
 double addPull(double p) {this->_pulls.push_back(p); return(p);}


};

// function prototypes

clasKineFit mwKfit(double beam, clasParticle *final, int numf, double mm, int constrain[], double conMass);
clasKineFit mwKfit(double beam,clasParticle *final, int numf);


#endif
